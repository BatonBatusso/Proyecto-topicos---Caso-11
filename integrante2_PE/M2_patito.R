# TOPICOS APLICADOS EN ESTADISTICA - CASO 11 NOAA
# INTEGRANTE 3: M2 ARBOL DE REGRESION Y PODA
# Los resultados y graficos aparecen en RStudio. No se guardan solos.

library(dplyr)
library(tidyr)
library(rpart)
library(ggplot2)

# Cambiar a TRUE solamente cuando corresponda.
integrar_ahora <- FALSE
evaluar_reserva <- FALSE


# 1. DATOS COMUNES -----------------------------------------------------------
base_desarrollo <- readRDS("datos_comunes/base_desarrollo.rds")
particiones <- readRDS("datos_comunes/particiones_desarrollo.rds")
configuracion <- readRDS("datos_comunes/configuracion_comun.rds")

if (!is.null(configuracion$semilla)) set.seed(configuracion$semilla)

# Compatibilidad con los nombres del paquete comun anterior.
# Las variables antiguas sirven SOLO para reconstruir las aprobadas;
# ninguna entra como predictor al arbol.
adaptar_nombres <- function(datos) {
  if (!"dano_usd" %in% names(datos) && "damage_property_usd" %in% names(datos)) {
    datos$dano_usd <- datos$damage_property_usd
  }
  # Usar la duracion en segundos, aunque la base comun venga en minutos.
  if (!"dur_seg" %in% names(datos) && "dur_min" %in% names(datos)) {
    datos$dur_seg <- as.numeric(datos$dur_min) * 60
  }
  if (!"dur_seg" %in% names(datos) && "duracion_min" %in% names(datos)) {
    datos$dur_seg <- as.numeric(datos$duracion_min) * 60
  }
  if (!"mag_origen" %in% names(datos) && "mag_type_grupo" %in% names(datos)) {
    datos$mag_origen <- datos$mag_type_grupo
  }
  if (!"magnitude" %in% names(datos) &&
      all(c("mag_hail_in", "mag_wind_kt") %in% names(datos))) {
    datos$magnitude <- dplyr::coalesce(as.numeric(datos$mag_hail_in),
                                      as.numeric(datos$mag_wind_kt))
  }
  datos
}

base_desarrollo <- adaptar_nombres(base_desarrollo)
base_desarrollo$EVENT_ID <- as.character(base_desarrollo$EVENT_ID)
base_desarrollo$EPISODE_ID <- as.character(base_desarrollo$EPISODE_ID)
particiones$EVENT_ID <- as.character(particiones$EVENT_ID)
particiones$EPISODE_ID <- as.character(particiones$EPISODE_ID)

# Solo las 10 variables aceptadas por el diccionario.
variables_modelo <- c(
  "event_type", "magnitude", "mag_origen", "dur_seg", "mes_num",
  "hora_inicio", "state", "wfo", "begin_lat", "begin_lon"
)

necesarias <- c("EVENT_ID", "EPISODE_ID", "year", "dano_usd", variables_modelo)
faltantes <- setdiff(necesarias, names(base_desarrollo))
if (length(faltantes) > 0) {
  stop("Faltan columnas en la base comun: ", paste(faltantes, collapse = ", "))
}
if (anyDuplicated(base_desarrollo$EVENT_ID)) stop("EVENT_ID esta duplicado.")
if (anyNA(base_desarrollo$EVENT_ID) || anyNA(base_desarrollo$EPISODE_ID)) {
  stop("Faltan identificadores de eventos o episodios.")
}
if (any(!base_desarrollo$year %in% 2018:2021)) {
  stop("La base de desarrollo debe contener solamente 2018-2021.")
}
if (anyNA(base_desarrollo$dano_usd) ||
    any(!is.finite(base_desarrollo$dano_usd)) ||
    any(base_desarrollo$dano_usd < 0)) {
  stop("La respuesta dano_usd debe estar observada y ser no negativa.")
}

cat("\nEVENTOS DE DESARROLLO:", nrow(base_desarrollo), "\n")
print(particiones %>% count(particion, rol, year))


# 2. VENTANAS TEMPORALES -----------------------------------------------------
# V1: 2018 -> 2019; V2: 2018-2019 -> 2020; V3: 2018-2020 -> 2021.
train_list <- list()
valid_list <- list()

for (i in 1:3) {
  nombre <- paste0("V", i)
  p <- particiones %>% filter(particion == nombre)
  train_list[[i]] <- base_desarrollo %>%
    filter(EVENT_ID %in% p$EVENT_ID[p$rol == "train"])
  valid_list[[i]] <- base_desarrollo %>%
    filter(EVENT_ID %in% p$EVENT_ID[p$rol == "valid"])

  if (nrow(train_list[[i]]) == 0 || nrow(valid_list[[i]]) == 0) {
    stop("Faltan eventos en ", nombre)
  }
  if (anyDuplicated(p$EVENT_ID) ||
      nrow(train_list[[i]]) != sum(p$rol == "train") ||
      nrow(valid_list[[i]]) != sum(p$rol == "valid")) {
    stop("Las particiones no coinciden con la base en ", nombre)
  }
  if (length(intersect(train_list[[i]]$EPISODE_ID,
                       valid_list[[i]]$EPISODE_ID)) > 0) {
    stop("Hay episodios compartidos en ", nombre)
  }
}


# 3. PREPARAR LOS PREDICTORES ------------------------------------------------
# Las medias, desvios, medianas y categorias se obtienen SOLO del train.
preparar_datos <- function(train, valid, con_wfo = TRUE) {
  vars <- variables_modelo
  if (!con_wfo) vars <- setdiff(vars, "wfo")

  x_train <- train[, vars, drop = FALSE]
  x_valid <- valid[, vars, drop = FALSE]

  # Magnitude tiene unidades distintas: granizo (pulgadas) y viento (nudos).
  # Se estandariza por event_type con la media y desvio del entrenamiento.
  magnitud_train <- as.numeric(train$magnitude)
  magnitud_train[!is.finite(magnitud_train)] <- NA_real_
  medias <- tapply(magnitud_train, train$event_type, mean, na.rm = TRUE)
  desvios <- tapply(magnitud_train, train$event_type, sd, na.rm = TRUE)
  medias[!is.finite(medias)] <- 0
  desvios[!is.finite(desvios) | desvios == 0] <- 1

  estandarizar <- function(datos) {
    tipo <- as.character(datos$event_type)
    z <- (as.numeric(datos$magnitude) - medias[tipo]) / desvios[tipo]
    z[!is.finite(z)] <- 0
    z
  }
  x_train$magnitude <- estandarizar(train)
  x_valid$magnitude <- estandarizar(valid)

  categoricas <- intersect(c("event_type", "mag_origen", "state", "wfo"), vars)
  for (v in categoricas) {
    a <- as.character(x_train[[v]])
    b <- as.character(x_valid[[v]])
    a[is.na(a) | trimws(a) == ""] <- "Sin_dato"
    b[is.na(b) | trimws(b) == ""] <- "Sin_dato"

    frecuencias <- table(a)
    frecuentes <- names(frecuencias[frecuencias >= 0.02 * length(a)])
    a[!a %in% c(frecuentes, "Sin_dato")] <- "Otros"
    b[!b %in% c(frecuentes, "Sin_dato")] <- "Otros"
    niveles <- unique(c(sort(unique(a)), "Sin_dato", "Otros"))
    x_train[[v]] <- factor(a, levels = niveles)
    x_valid[[v]] <- factor(b, levels = niveles)
  }

  for (v in setdiff(vars, categoricas)) {
    a <- as.numeric(x_train[[v]])
    b <- as.numeric(x_valid[[v]])
    a[!is.finite(a)] <- NA_real_
    b[!is.finite(b)] <- NA_real_
    med <- median(a, na.rm = TRUE)
    if (!is.finite(med)) med <- 0
    a[is.na(a)] <- med
    b[is.na(b)] <- med
    x_train[[v]] <- a
    x_valid[[v]] <- b
  }

  list(train = x_train, valid = x_valid)
}


# 4. AJUSTE Y PODA: 20 CONFIGURACIONES ---------------------------------------
grilla <- expand.grid(cp = c(0, 0.001, 0.003, 0.01, 0.03),
                      maxdepth = c(4, 7), minbucket = c(10, 30))
grilla$configuracion <- paste0("C", sprintf("%02d", seq_len(nrow(grilla))))

resultados <- list()
predicciones <- list()
divisiones <- list()
k <- 0

# WFO esta admitida, pero el diccionario pide comprobar que no cambie
# demasiado los resultados al quitarla (puede reflejar el reporte).
for (con_wfo in c(TRUE, FALSE)) {
  escenario <- ifelse(con_wfo, "con_wfo", "sin_wfo")
  preparados <- lapply(1:3, function(i) {
    preparar_datos(train_list[[i]], valid_list[[i]], con_wfo)
  })

  for (variante in c("original", "log1p")) {
    for (j in seq_len(nrow(grilla))) {
      for (i in 1:3) {
        train <- train_list[[i]]
        valid <- valid_list[[i]]
        x <- preparados[[i]]
        x_train <- x$train
        x_train$y <- if (variante == "original") train$dano_usd else log1p(train$dano_usd)

        tiempo <- proc.time()[3]
        arbol_grande <- rpart(
          y ~ ., data = x_train, method = "anova",
          control = rpart.control(cp = 0, maxdepth = grilla$maxdepth[j],
                                  minbucket = grilla$minbucket[j],
                                  minsplit = max(20, 3 * grilla$minbucket[j]),
                                  xval = 0)
        )
        arbol <- prune(arbol_grande, cp = grilla$cp[j])
        pred <- as.numeric(predict(arbol, newdata = x$valid))
        if (variante == "log1p") pred <- expm1(pred)
        pred <- pmax(0, pred)
        segundos <- proc.time()[3] - tiempo

        k <- k + 1
        resultados[[k]] <- data.frame(
          uso_wfo = escenario, variante_respuesta = variante,
          configuracion = grilla$configuracion[j],
          cp = grilla$cp[j], maxdepth = grilla$maxdepth[j],
          minbucket = grilla$minbucket[j], particion = paste0("V", i),
          n = nrow(valid), MAE = mean(abs(valid$dano_usd - pred)),
          RMSE = sqrt(mean((valid$dano_usd - pred)^2)),
          hojas = sum(arbol$frame$var == "<leaf>"), segundos = segundos
        )
        predicciones[[k]] <- data.frame(
          EVENT_ID = valid$EVENT_ID, EPISODE_ID = valid$EPISODE_ID,
          particion = paste0("V", i), anio_validacion = c(2019, 2020, 2021)[i],
          y_observado = valid$dano_usd, y_predicho = pred,
          modelo = "M2_arbol", variante_respuesta = variante,
          configuracion = grilla$configuracion[j], uso_wfo = escenario
        )
        divisiones[[k]] <- data.frame(
          uso_wfo = escenario, variante_respuesta = variante,
          configuracion = grilla$configuracion[j], particion = paste0("V", i),
          variables_division = paste(unique(arbol$frame$var[
            arbol$frame$var != "<leaf>"]), collapse = ", ")
        )
      }
    }
  }
}

tabla_busqueda <- bind_rows(resultados)
predicciones_busqueda <- bind_rows(predicciones)

resumen_busqueda <- tabla_busqueda %>%
  group_by(uso_wfo, variante_respuesta, configuracion, cp, maxdepth, minbucket) %>%
  summarise(sd_MAE = sd(MAE), MAE = weighted.mean(MAE, n),
            RMSE = sqrt(sum(n * RMSE^2) / sum(n)),
            hojas_promedio = mean(hojas),
            segundos_total = sum(segundos), .groups = "drop")

tolerancia <- configuracion$tolerancia_MAE
if (is.null(tolerancia)) tolerancia <- configuracion$tolerancia_mae_parsimonia
if (is.null(tolerancia)) tolerancia <- 0.01

# Si dos opciones estan dentro del 1% del menor MAE, gana la mas simple.
configuraciones_elegidas <- resumen_busqueda %>%
  group_by(uso_wfo, variante_respuesta) %>%
  filter(MAE <= min(MAE) * (1 + tolerancia)) %>%
  arrange(hojas_promedio, sd_MAE, MAE, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

cat("\nCONFIGURACIONES ELEGIDAS (20 PROBADAS POR ESCENARIO)\n")
print(configuraciones_elegidas %>%
        select(uso_wfo, variante_respuesta, configuracion,
               cp, maxdepth, minbucket, MAE, RMSE,
               sd_MAE, hojas_promedio, segundos_total))

predicciones_elegidas <- predicciones_busqueda %>%
  semi_join(configuraciones_elegidas,
            by = c("uso_wfo", "variante_respuesta", "configuracion"))

cat("\nDIVISIONES QUE USA CADA ARBOL\n")
divisiones_elegidas <- bind_rows(divisiones) %>%
  semi_join(configuraciones_elegidas,
            by = c("uso_wfo", "variante_respuesta", "configuracion"))
print(divisiones_elegidas)


# 5. RESULTADOS Y SENSIBILIDADES --------------------------------------------
tabla_ventanas_m2 <- predicciones_elegidas %>%
  group_by(uso_wfo, variante_respuesta, particion) %>%
  summarise(n = n(), MAE = mean(abs(y_observado - y_predicho)),
            RMSE = sqrt(mean((y_observado - y_predicho)^2)), .groups = "drop")

cat("\nERROR POR VENTANA: ORIGINAL, LOG1P, CON Y SIN WFO\n")
print(tabla_ventanas_m2, n = Inf)

# Elegir el modelo final usando SOLO las validaciones de 2019, 2020 y 2021.
mejor <- configuraciones_elegidas %>%
  filter(MAE <= min(MAE) * (1 + tolerancia)) %>%
  arrange(hojas_promedio, MAE) %>%
  slice(1)

cat("\nMODELO M2 SELECCIONADO\n")
print(mejor)

predicciones_m2_final <- predicciones_elegidas %>%
  filter(uso_wfo == mejor$uso_wfo,
         variante_respuesta == mejor$variante_respuesta) %>%
  select(EVENT_ID, EPISODE_ID, particion, anio_validacion, modelo,
         variante_respuesta, y_observado, y_predicho) %>%
  mutate(error_absoluto = abs(y_observado - y_predicho))

# Dos referencias: mediana general y mediana segun el tipo de evento.
referencias <- lapply(1:3, function(i) {
  train <- train_list[[i]]
  valid <- valid_list[[i]]
  med_general <- median(train$dano_usd)
  med_tipo <- tapply(train$dano_usd, train$event_type, median)
  pred_tipo <- med_tipo[as.character(valid$event_type)]
  pred_tipo[is.na(pred_tipo)] <- med_general
  data.frame(particion = paste0("V", i),
             MAE_mediana_general = mean(abs(valid$dano_usd - med_general)),
             MAE_mediana_tipo = mean(abs(valid$dano_usd - pred_tipo)))
}) %>% bind_rows()

cat("\nM2 FRENTE A LAS REFERENCIAS\n")
print(tabla_ventanas_m2 %>% left_join(referencias, by = "particion"), n = Inf)

# Errores por granizo, viento medido y viento estimado.
resultados_grupos <- predicciones_elegidas %>%
  filter(uso_wfo == mejor$uso_wfo) %>%
  left_join(base_desarrollo %>% select(EVENT_ID, mag_origen), by = "EVENT_ID") %>%
  group_by(variante_respuesta, particion, mag_origen) %>%
  summarise(n = n(), MAE = mean(abs(y_observado - y_predicho)),
            RMSE = sqrt(mean((y_observado - y_predicho)^2)), .groups = "drop")
cat("\nMAE Y RMSE POR TIPO DE EVENTO Y MEDICION\n")
print(resultados_grupos, n = Inf)

# Extremos definidos por el percentil 99 de CADA entrenamiento.
umbrales <- data.frame(
  particion = paste0("V", 1:3),
  umbral = sapply(train_list, function(d) {
    as.numeric(quantile(d$dano_usd, 0.99, na.rm = TRUE))
  })
)
resultados_extremos <- predicciones_m2_final %>%
  left_join(umbrales, by = "particion") %>%
  mutate(grupo_dano = case_when(
    y_observado == 0 ~ "Cero",
    y_observado >= umbral ~ "Extremo",
    TRUE ~ "Positivo_no_extremo"
  )) %>%
  group_by(particion, grupo_dano) %>%
  summarise(n = n(), MAE = mean(error_absoluto),
            suma_errores = sum(error_absoluto), .groups = "drop") %>%
  group_by(particion) %>%
  mutate(pct_error = 100 * suma_errores / sum(suma_errores)) %>%
  ungroup()
cat("\nERRORES EN CEROS, DANOS POSITIVOS Y EXTREMOS\n")
print(resultados_extremos, n = Inf)

# Graficos en la pestana Plots de RStudio.
print(ggplot(tabla_ventanas_m2 %>% filter(uso_wfo == mejor$uso_wfo),
             aes(particion, MAE, fill = variante_respuesta)) +
        geom_col(position = "dodge") + theme_minimal() +
        labs(title = "M2: respuesta original y log1p",
             x = "Ventana", y = "MAE (dolares)", fill = "Respuesta"))

print(ggplot(resultados_grupos,
             aes(mag_origen, MAE, fill = variante_respuesta)) +
        geom_col(position = "dodge") + coord_flip() +
        facet_wrap(~ particion, ncol = 1) + theme_minimal() +
        labs(title = "M2: error por tipo de evento y medicion",
             x = "Grupo", y = "MAE (dolares)", fill = "Respuesta"))

print(ggplot(configuraciones_elegidas,
             aes(uso_wfo, MAE, fill = variante_respuesta)) +
        geom_col(position = "dodge") + theme_minimal() +
        labs(title = "Sensibilidad: con y sin WFO",
             x = "Variables usadas", y = "MAE (dolares)", fill = "Respuesta"))


# 6. ARBOL FINAL (2018-2021) ------------------------------------------------
# Con los parametros seleccionados, se reajusta usando todo desarrollo.
x_final <- preparar_datos(base_desarrollo, base_desarrollo[0, ],
                         con_wfo = (mejor$uso_wfo == "con_wfo"))$train
x_final$y <- if (mejor$variante_respuesta == "original") {
  base_desarrollo$dano_usd
} else {
  log1p(base_desarrollo$dano_usd)
}

arbol_grande_final <- rpart(
  y ~ ., data = x_final, method = "anova",
  control = rpart.control(cp = 0, maxdepth = mejor$maxdepth,
                          minbucket = mejor$minbucket,
                          minsplit = max(20, 3 * mejor$minbucket), xval = 0)
)
arbol_final <- prune(arbol_grande_final, cp = mejor$cp)
cat("\nARBOL FINAL\n")
print(arbol_final)

if (length(arbol_final$variable.importance) > 0) {
  importancia <- data.frame(
    variable = names(arbol_final$variable.importance),
    importancia = as.numeric(arbol_final$variable.importance)
  ) %>% arrange(desc(importancia))
  cat("\nIMPORTANCIA DE VARIABLES\n")
  print(importancia)
}

tabla_hojas <- arbol_final$frame %>%
  filter(var == "<leaf>") %>%
  transmute(n_eventos = n, valor_arbol = yval)
tabla_hojas$prediccion_usd <- if (mejor$variante_respuesta == "log1p") {
  pmax(0, expm1(tabla_hojas$valor_arbol))
} else {
  pmax(0, tabla_hojas$valor_arbol)
}
cat("\nVALORES DE LAS HOJAS EN DOLARES\n")
print(tabla_hojas)

plot(arbol_final, uniform = TRUE, margin = 0.1,
     main = ifelse(mejor$variante_respuesta == "log1p",
                   "Arbol M2 (hojas en log1p)", "Arbol M2 (hojas en dolares)"))
text(arbol_final, use.n = TRUE, cex = 0.7)

# Funcion para predecir otros eventos con reglas aprendidas en desarrollo.
predecir_m2 <- function(nuevos_eventos) {
  nuevos_eventos <- adaptar_nombres(nuevos_eventos)
  x_nuevo <- preparar_datos(base_desarrollo, nuevos_eventos,
                           con_wfo = (mejor$uso_wfo == "con_wfo"))$valid
  pred <- as.numeric(predict(arbol_final, newdata = x_nuevo))
  if (mejor$variante_respuesta == "log1p") pred <- expm1(pred)
  pmax(0, pred)
}


# 7. MIS PREDICCIONES DE M2 -------------------------------------------------
cat("\nPREDICCIONES M2 EN VALIDACION\n")
cat("Total eventos:", nrow(predicciones_m2_final), "\n")
print(head(predicciones_m2_final, 15))
cat("\nRESUMEN DE PREDICCIONES EN DOLARES\n")
print(summary(predicciones_m2_final$y_predicho))
cat("MAE total M2:", round(mean(predicciones_m2_final$error_absoluto), 2), "USD\n")
# View(predicciones_m2_final)   # Descomentar si quieres revisar todas las filas.


# 8. COMPARAR CON LOS OTROS MODELOS (CUANDO ESTEN LISTOS) ----------------------
# Cargar antes pred_referencia, pred_m1 y pred_m3; luego activar integrar_ahora.
if (integrar_ahora) {
  if (!all(c("pred_referencia", "pred_m1", "pred_m3") %in% ls())) {
    stop("Faltan las predicciones de referencia, M1 o M3.")
  }
  # Para la sensibilidad integrada dejamos las DOS respuestas de M2.
  m2_para_comparar <- predicciones_elegidas %>%
    filter(uso_wfo == mejor$uso_wfo)
  claves <- c("EVENT_ID", "EPISODE_ID", "particion", "anio_validacion")
  columnas <- c(claves, "modelo", "variante_respuesta", "y_observado", "y_predicho")
  lista_modelos <- list(pred_referencia, pred_m1, m2_para_comparar, pred_m3)
  for (i in seq_along(lista_modelos)) {
    p <- lista_modelos[[i]]
    if (!"variante_respuesta" %in% names(p)) {
      p$variante_respuesta <- "sin_transformacion"
    }
    if (!all(columnas %in% names(p))) {
      stop("Las predicciones deben tener las columnas acordadas.")
    }
    lista_modelos[[i]] <- p %>%
      select(all_of(columnas)) %>%
      mutate(across(all_of(claves), as.character),
             modelo = as.character(modelo),
             variante_respuesta = as.character(variante_respuesta),
             y_observado = as.numeric(y_observado),
             y_predicho = as.numeric(y_predicho))
  }
  todas <- bind_rows(lista_modelos)

  esperados <- particiones %>% filter(rol == "valid") %>%
    transmute(across(all_of(claves), as.character))
  for (m in unique(todas$modelo)) {
    for (v in unique(todas$variante_respuesta[todas$modelo == m])) {
      p <- todas %>% filter(modelo == m, variante_respuesta == v)
      if (anyDuplicated(p[claves]) ||
          nrow(anti_join(esperados, p, by = claves)) > 0 ||
          nrow(anti_join(p, esperados, by = claves)) > 0) {
        stop("Los eventos de validacion no coinciden para ", m, " / ", v)
      }
    }
  }
  if (anyNA(todas$y_observado) || anyNA(todas$y_predicho) ||
      any(!is.finite(todas$y_predicho)) || any(todas$y_predicho < 0)) {
    stop("Hay predicciones faltantes, infinitas o negativas.")
  }
  respuestas <- base_desarrollo %>%
    transmute(EVENT_ID, respuesta = dano_usd)
  todas <- todas %>% left_join(respuestas, by = "EVENT_ID")
  if (anyNA(todas$respuesta) ||
      any(abs(todas$respuesta - todas$y_observado) > 0.001)) {
    stop("Las respuestas observadas no coinciden entre modelos.")
  }

  comparacion_modelos <- todas %>%
    group_by(modelo, variante_respuesta) %>%
    summarise(n = n(), MAE = mean(abs(y_observado - y_predicho)),
              RMSE = sqrt(mean((y_observado - y_predicho)^2)),
              .groups = "drop") %>% arrange(MAE)
  cat("\nCOMPARACION FINAL ENTRE LOS MODELOS EN DESARROLLO\n")
  print(comparacion_modelos, n = Inf)

  comparacion_ventanas <- todas %>%
    group_by(modelo, variante_respuesta, particion) %>%
    summarise(n = n(), MAE = mean(abs(y_observado - y_predicho)),
              RMSE = sqrt(mean((y_observado - y_predicho)^2)),
              .groups = "drop")
  estabilidad <- comparacion_ventanas %>%
    group_by(modelo, variante_respuesta) %>%
    summarise(sd_MAE = sd(MAE), MAE_min = min(MAE),
              MAE_max = max(MAE), .groups = "drop")
  cat("\nCOMPARACION POR VENTANA\n")
  print(comparacion_ventanas, n = Inf)
  cat("\nESTABILIDAD ENTRE VENTANAS\n")
  print(estabilidad, n = Inf)

  comparacion_grupos <- todas %>%
    left_join(base_desarrollo %>% select(EVENT_ID, mag_origen), by = "EVENT_ID") %>%
    group_by(modelo, variante_respuesta, mag_origen) %>%
    summarise(n = n(), MAE = mean(abs(y_observado - y_predicho)),
              .groups = "drop")
  cat("\nCOMPARACION POR TIPO Y MEDICION\n")
  print(comparacion_grupos, n = Inf)

  print(ggplot(comparacion_modelos, aes(reorder(modelo, MAE), MAE,
                                      fill = variante_respuesta)) +
          geom_col(position = "dodge") + coord_flip() + theme_minimal() +
          labs(title = "Comparacion de modelos", x = "Modelo",
               y = "MAE (dolares)", fill = "Respuesta"))
}


# 9. RESERVA 2022-2023: NO ACTIVAR HASTA EL CIERRE ----------------------------
if (evaluar_reserva) {
  base_reserva <- readRDS("datos_comunes/base_prueba_sin_respuesta.rds")
  if (any(c("dano_usd", "damage_property_usd") %in% names(base_reserva))) {
    stop("La reserva de prediccion debe venir SIN respuesta.")
  }
  base_reserva <- adaptar_nombres(base_reserva)
  predicciones_reserva_m2 <- data.frame(
    EVENT_ID = base_reserva$EVENT_ID,
    EPISODE_ID = base_reserva$EPISODE_ID,
    modelo = "M2_arbol", variante_respuesta = mejor$variante_respuesta,
    y_predicho = predecir_m2(base_reserva)
  )
  cat("\nPREDICCIONES M2 PARA 2022-2023 (SIN VER RESPUESTA)\n")
  print(head(predicciones_reserva_m2, 15))
}

# Cuando el grupo lo necesite, se puede guardar expresamente:
# saveRDS(predicciones_m2_final, "predicciones_desarrollo_m2.rds")
