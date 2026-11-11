# TOPICOS APLICADOS - CASO 11
# M2: ARBOL DE REGRESION CON PODA
# Los graficos se ven en RStudio. Los resultados se guardan al final.

library(dplyr)
library(rpart)
library(ggplot2)

# Estas dos opciones quedan apagadas hasta que el grupo las necesite.
integrar_ahora <- FALSE
evaluar_reserva <- FALSE

# 1. DATOS COMUNES ----------------------------------------------------------
# Leo las bases que preparo el grupo. No las modifico.
archivos_comunes <- c(
  "datos_comunes/base_desarrollo.rds",
  "datos_comunes/particiones_desarrollo.rds",
  "datos_comunes/configuracion_comun.rds"
)

archivos_faltantes <- archivos_comunes[!file.exists(archivos_comunes)]
if (length(archivos_faltantes) > 0) {
  stop(
    "No se encontraron estos archivos comunes: ",
    paste(archivos_faltantes, collapse = ", "),
    ". Ejecuta M2 desde la carpeta principal del proyecto."
  )
}

base_desarrollo <- readRDS("datos_comunes/base_desarrollo.rds")
particiones <- readRDS("datos_comunes/particiones_desarrollo.rds")
configuracion <- readRDS("datos_comunes/configuracion_comun.rds")

if (!is.null(configuracion$semilla)) set.seed(configuracion$semilla)

# Dejo los nombres de las variables como los usa el modelo.
adaptar_nombres <- function(datos) {
  if (!"dano_usd" %in% names(datos) && "damage_property_usd" %in% names(datos)) {
    datos$dano_usd <- datos$damage_property_usd
  }
  # La duracion la trabajo en segundos.
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

# Los ID pueden venir en mayusculas o minusculas.
normalizar_identificadores <- function(datos, nombre_objeto) {
  nombres_originales <- names(datos)
  nombres_mayuscula <- toupper(nombres_originales)

  posicion_evento <- which(nombres_mayuscula == "EVENT_ID")
  posicion_episodio <- which(nombres_mayuscula == "EPISODE_ID")

  if (length(posicion_evento) == 0) {
    stop(
      "No se encontro EVENT_ID ni event_id en ", nombre_objeto,
      ". Columnas relacionadas con ID: ",
      paste(grep("id", nombres_originales, value = TRUE,
                 ignore.case = TRUE), collapse = ", ")
    )
  }
  if (length(posicion_episodio) == 0) {
    stop(
      "No se encontro EPISODE_ID ni episode_id en ", nombre_objeto,
      ". Columnas relacionadas con ID: ",
      paste(grep("id", nombres_originales, value = TRUE,
                 ignore.case = TRUE), collapse = ", ")
    )
  }
  if (length(posicion_evento) > 1) {
    stop("Hay mas de una columna equivalente a EVENT_ID en ", nombre_objeto, ".")
  }
  if (length(posicion_episodio) > 1) {
    stop("Hay mas de una columna equivalente a EPISODE_ID en ", nombre_objeto, ".")
  }

  names(datos)[posicion_evento] <- "event_id"
  names(datos)[posicion_episodio] <- "episode_id"
  datos
}

base_desarrollo <- adaptar_nombres(base_desarrollo)
base_desarrollo <- normalizar_identificadores(base_desarrollo,
                                              "base_desarrollo")
base_desarrollo$event_id <- as.character(base_desarrollo$event_id)
base_desarrollo$episode_id <- as.character(base_desarrollo$episode_id)

# Si las particiones no traen los ID, las armo por año y episodio.
nombres_particiones <- toupper(names(particiones))
particiones_tienen_ids <- all(
  c("EVENT_ID", "EPISODE_ID") %in% nombres_particiones
)

if (particiones_tienen_ids) {
  particiones <- normalizar_identificadores(particiones, "particiones")
  particiones$event_id <- as.character(particiones$event_id)
  particiones$episode_id <- as.character(particiones$episode_id)
  cat("\nSE USARON LAS PARTICIONES POR EVENTO DEL PAQUETE COMUN.\n")
} else {
  if (!"year" %in% names(base_desarrollo)) {
    stop("No se puede reconstruir las ventanas porque falta la variable year.")
  }
  if (anyNA(base_desarrollo$year)) {
    stop("No se puede reconstruir las ventanas porque existen años faltantes.")
  }

  # Un episodio siempre queda en un solo grupo, aunque pase de un año a otro.
  base_para_particiones <- base_desarrollo %>%
    group_by(episode_id) %>%
    mutate(year_episode = min(as.integer(year))) %>%
    ungroup()

  crear_ventana <- function(datos, nombre, anios_train, anio_valid) {
    train <- datos %>%
      filter(year_episode %in% anios_train) %>%
      transmute(
        event_id,
        episode_id,
        year = as.integer(year),
        particion = nombre,
        rol = "train"
      )

    valid <- datos %>%
      filter(year_episode == anio_valid) %>%
      transmute(
        event_id,
        episode_id,
        year = as.integer(year),
        particion = nombre,
        rol = "valid"
      )

    bind_rows(train, valid)
  }

  particiones <- bind_rows(
    crear_ventana(base_para_particiones, "V1", 2018, 2019),
    crear_ventana(base_para_particiones, "V2", 2018:2019, 2020),
    crear_ventana(base_para_particiones, "V3", 2018:2020, 2021)
  )

  particiones$event_id <- as.character(particiones$event_id)
  particiones$episode_id <- as.character(particiones$episode_id)

  cat(
    "\nEL ARCHIVO DE PARTICIONES NO TENIA IDENTIFICADORES. ",
    "LAS TRES VENTANAS FUERON RECONSTRUIDAS POR AÑO Y EPISODIO.\n",
    sep = ""
  )
}

# Uso las variables acordadas para M2.
variables_modelo <- c(
  "event_type", "magnitude", "mag_origen", "dur_seg", "mes_num",
  "hora_inicio", "state", "wfo", "begin_lat", "begin_lon"
)

necesarias <- c("event_id", "episode_id", "year", "dano_usd", variables_modelo)
faltantes <- setdiff(necesarias, names(base_desarrollo))
if (length(faltantes) > 0) {
  stop("Faltan columnas en la base comun: ", paste(faltantes, collapse = ", "))
}
if (anyDuplicated(base_desarrollo$event_id)) stop("event_id esta duplicado.")
if (anyNA(base_desarrollo$event_id) || anyNA(base_desarrollo$episode_id)) {
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

# 2. VENTANAS ---------------------------------------------------------------
# Entreno con años anteriores y valido con el año siguiente.
train_list <- list()
valid_list <- list()

for (i in 1:3) {
  nombre <- paste0("V", i)
  p <- particiones %>% filter(particion == nombre)
  train_list[[i]] <- base_desarrollo %>%
    filter(event_id %in% p$event_id[p$rol == "train"])
  valid_list[[i]] <- base_desarrollo %>%
    filter(event_id %in% p$event_id[p$rol == "valid"])

  if (nrow(train_list[[i]]) == 0 || nrow(valid_list[[i]]) == 0) {
    stop("Faltan eventos en ", nombre)
  }
  if (anyDuplicated(p$event_id) ||
      nrow(train_list[[i]]) != sum(p$rol == "train") ||
      nrow(valid_list[[i]]) != sum(p$rol == "valid")) {
    stop("Las particiones no coinciden con la base en ", nombre)
  }
  if (length(intersect(train_list[[i]]$episode_id,
                       valid_list[[i]]$episode_id)) > 0) {
    stop("Hay episodios compartidos en ", nombre)
  }
}

# 3. PREPARAR LOS DATOS -----------------------------------------------------
# Todo lo que se calcula aca sale del entrenamiento de cada ventana.
preparar_datos <- function(train, valid, con_wfo = TRUE) {
  vars <- variables_modelo
  if (!con_wfo) vars <- setdiff(vars, "wfo")

  x_train <- train[, vars, drop = FALSE]
  x_valid <- valid[, vars, drop = FALSE]

  # Estandarizo la magnitud por tipo de evento, porque usa distintas unidades.
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

# 4. AJUSTAR LOS ARBOLES ----------------------------------------------------
grilla <- expand.grid(cp = c(0, 0.001, 0.003, 0.01, 0.03),
                      maxdepth = c(4, 7), minbucket = c(10, 30))
grilla$configuracion <- paste0("C", sprintf("%02d", seq_len(nrow(grilla))))

resultados <- list()
predicciones <- list()
divisiones <- list()
k <- 0

# Pruebo con y sin WFO, y con la respuesta original y log1p.
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
          event_id = valid$event_id, episode_id = valid$episode_id,
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

# Si el MAE cambia menos de un 1%, me quedo con el arbol mas simple.
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

# Elijo el M2 final usando las tres validaciones.
mejor <- configuraciones_elegidas %>%
  filter(MAE <= min(MAE) * (1 + tolerancia)) %>%
  arrange(hojas_promedio, MAE) %>%
  slice(1)

cat("\nMODELO M2 SELECCIONADO\n")
print(mejor)

predicciones_m2_final <- predicciones_elegidas %>%
  filter(uso_wfo == mejor$uso_wfo,
         variante_respuesta == mejor$variante_respuesta) %>%
  select(event_id, episode_id, particion, anio_validacion, modelo,
         variante_respuesta, y_observado, y_predicho) %>%
  mutate(error_absoluto = abs(y_observado - y_predicho))

# Dejo las dos respuestas con el mismo uso de WFO.
# Cada una conserva su propia configuracion.
predicciones_m2_ambas <- predicciones_elegidas %>%
  filter(uso_wfo == mejor$uso_wfo) %>%
  left_join(
    configuraciones_elegidas %>%
      select(uso_wfo, variante_respuesta, configuracion,
             cp, maxdepth, minbucket),
    by = c("uso_wfo", "variante_respuesta", "configuracion")
  ) %>%
  mutate(error_absoluto = abs(y_observado - y_predicho))

# Cada evento debe tener una prediccion original y otra log1p.
claves_m2 <- c("event_id", "episode_id", "particion", "anio_validacion")
n_validaciones_m2 <- sum(sapply(valid_list, nrow))

if (!identical(sort(unique(predicciones_m2_ambas$variante_respuesta)),
               c("log1p", "original")) ||
    nrow(predicciones_m2_ambas) != 2 * n_validaciones_m2 ||
    any(table(predicciones_m2_ambas$variante_respuesta) != n_validaciones_m2) ||
    anyDuplicated(predicciones_m2_ambas[
      c(claves_m2, "variante_respuesta")
    ]) ||
    anyNA(predicciones_m2_ambas$cp)) {
  stop("Las predicciones originales y log1p no coinciden con las validaciones.")
}

# Comparo con dos formas simples de predecir usando la mediana.
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
  left_join(base_desarrollo %>% select(event_id, mag_origen), by = "event_id") %>%
  group_by(variante_respuesta, particion, mag_origen) %>%
  summarise(n = n(), MAE = mean(abs(y_observado - y_predicho)),
            RMSE = sqrt(mean((y_observado - y_predicho)^2)), .groups = "drop")
cat("\nMAE Y RMSE POR TIPO DE EVENTO Y MEDICION\n")
print(resultados_grupos, n = Inf)

# El percentil 99 se calcula de nuevo en cada entrenamiento.
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

# 6. ARBOL FINAL ------------------------------------------------------------
# Uso todos los datos de 2018 a 2021 con la configuracion elegida.
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

# Esto permite usar el arbol final con eventos nuevos.
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
cat("Predicciones por respuesta:", n_validaciones_m2, "\n")
print(head(predicciones_m2_final, 15))
cat("\nRESUMEN DE PREDICCIONES EN DOLARES\n")
print(summary(predicciones_m2_final$y_predicho))
cat("MAE total M2:", round(mean(predicciones_m2_final$error_absoluto), 2), "USD\n")

# 8. COMPARACION CON LOS OTROS MODELOS (OPCIONAL) ---------------------------
# Cuando esten las otras predicciones, se puede activar integrar_ahora.
if (integrar_ahora) {
  if (!all(c("pred_referencia", "pred_m1", "pred_m3") %in% ls())) {
    stop("Faltan las predicciones de referencia, M1 o M3.")
  }
  # Para M2 comparo las dos respuestas.
  m2_para_comparar <- predicciones_m2_ambas
  claves <- c("event_id", "episode_id", "particion", "anio_validacion")
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

  esperados <- particiones %>%
    filter(rol == "valid") %>%
    mutate(anio_validacion = case_when(
      particion == "V1" ~ "2019",
      particion == "V2" ~ "2020",
      particion == "V3" ~ "2021"
    )) %>%
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
    transmute(event_id, respuesta = dano_usd)
  todas <- todas %>% left_join(respuestas, by = "event_id")
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
    left_join(base_desarrollo %>% select(event_id, mag_origen), by = "event_id") %>%
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

# 9. RESERVA 2022-2023 (TODAVIA NO SE USA) ----------------------------------
if (evaluar_reserva) {
  base_reserva <- readRDS("datos_comunes/base_prueba_sin_respuesta.rds")
  if (any(c("dano_usd", "damage_property_usd") %in% names(base_reserva))) {
    stop("La reserva de prediccion debe venir SIN respuesta.")
  }
  base_reserva <- adaptar_nombres(base_reserva)
  base_reserva <- normalizar_identificadores(base_reserva, "base_reserva")
  base_reserva$event_id <- as.character(base_reserva$event_id)
  base_reserva$episode_id <- as.character(base_reserva$episode_id)
  predicciones_reserva_m2 <- data.frame(
    event_id = base_reserva$event_id,
    episode_id = base_reserva$episode_id,
    modelo = "M2_arbol", variante_respuesta = mejor$variante_respuesta,
    y_predicho = predecir_m2(base_reserva)
  )
  cat("\nPREDICCIONES M2 PARA 2022-2023 (SIN VER RESPUESTA)\n")
  print(head(predicciones_reserva_m2, 15))
}

# 10. GUARDAR RESULTADOS DEL MODELO M2 --------------------------------------

carpeta_resultados <- "integrante2_PE/resultados_m2"
dir.create(carpeta_resultados, recursive = TRUE, showWarnings = FALSE)

# Guardo la busqueda completa y el modelo que elegi.
write.csv(tabla_busqueda,
          file.path(carpeta_resultados, "hiperparametros_busqueda_m2.csv"),
          row.names = FALSE)
write.csv(resumen_busqueda,
          file.path(carpeta_resultados, "resumen_hiperparametros_m2.csv"),
          row.names = FALSE)
write.csv(mejor,
          file.path(carpeta_resultados, "hiperparametros_elegidos_m2.csv"),
          row.names = FALSE)
saveRDS(mejor, file.path(carpeta_resultados, "configuracion_elegida_m2.rds"))

# Las dos respuestas van juntas en el mismo CSV: 9.967 de cada una.
# Las predicciones estan en dolares, no en escala logaritmica.
predicciones_exportar_m2 <- predicciones_m2_ambas %>%
  select(
    event_id,
    episode_id,
    particion,
    anio_validacion,
    modelo,
    variante_respuesta,
    uso_wfo,
    configuracion,
    cp,
    maxdepth,
    minbucket,
    y_observado,
    y_predicho,
    error_absoluto
  )

if (nrow(predicciones_exportar_m2) != 2 * n_validaciones_m2 ||
    anyNA(predicciones_exportar_m2$y_predicho) ||
    anyDuplicated(predicciones_exportar_m2[
      c(claves_m2, "variante_respuesta")
    ])) {
  stop("El CSV debe contener una fila por evento y por respuesta.")
}

write.csv(predicciones_exportar_m2,
          file.path(carpeta_resultados, "predicciones_m2.csv"),
          row.names = FALSE)

# Error del M2 elegido en cada ventana.
errores_finales_ventanas_m2 <- predicciones_m2_final %>%
  group_by(particion, anio_validacion) %>%
  summarise(
    n = n(),
    MAE = mean(abs(y_observado - y_predicho)),
    RMSE = sqrt(mean((y_observado - y_predicho)^2)),
    .groups = "drop"
  )

write.csv(errores_finales_ventanas_m2,
          file.path(carpeta_resultados, "errores_ventanas_m2.csv"),
          row.names = FALSE)

# Guardo las comparaciones con y sin WFO, y original frente a log1p.
write.csv(tabla_ventanas_m2,
          file.path(carpeta_resultados, "sensibilidad_escenarios_m2.csv"),
          row.names = FALSE)

sensibilidad_wfo_m2 <- configuraciones_elegidas %>%
  select(
    uso_wfo,
    variante_respuesta,
    configuracion,
    cp,
    maxdepth,
    minbucket,
    MAE,
    RMSE,
    sd_MAE,
    hojas_promedio,
    segundos_total
  )

write.csv(sensibilidad_wfo_m2,
          file.path(carpeta_resultados, "sensibilidad_wfo_m2.csv"),
          row.names = FALSE)
write.csv(resultados_grupos,
          file.path(carpeta_resultados, "sensibilidad_subgrupos_m2.csv"),
          row.names = FALSE)
write.csv(resultados_extremos,
          file.path(carpeta_resultados, "sensibilidad_extremos_m2.csv"),
          row.names = FALSE)

# Guardo el error de las distintas configuraciones de poda.
curva_poda_m2 <- resumen_busqueda %>%
  select(
    uso_wfo,
    variante_respuesta,
    configuracion,
    cp,
    maxdepth,
    minbucket,
    MAE,
    RMSE,
    sd_MAE,
    hojas_promedio
  ) %>%
  arrange(
    uso_wfo,
    variante_respuesta,
    cp,
    maxdepth,
    minbucket
  )

write.csv(curva_poda_m2,
          file.path(carpeta_resultados, "curva_poda_m2.csv"),
          row.names = FALSE)
write.csv(divisiones_elegidas,
          file.path(carpeta_resultados, "divisiones_arbol_m2.csv"),
          row.names = FALSE)
write.csv(tabla_hojas,
          file.path(carpeta_resultados, "hojas_arbol_final_m2.csv"),
          row.names = FALSE)

# Si el arbol tiene divisiones, guardo la importancia de variables.

if (exists("importancia")) {
  write.csv(importancia,
            file.path(carpeta_resultados, "importancia_m2.csv"),
            row.names = FALSE)
} else {
  cat(
    "\nEl árbol final no tiene divisiones;",
    "por eso no se creó importancia_m2.csv.\n"
  )
}

# Tiempo que tomo cada ajuste.
tiempos_m2 <- tabla_busqueda %>%
  select(
    uso_wfo,
    variante_respuesta,
    configuracion,
    cp,
    maxdepth,
    minbucket,
    particion,
    n,
    segundos
  )

write.csv(tiempos_m2,
          file.path(carpeta_resultados, "tiempos_m2.csv"),
          row.names = FALSE)

# Guardo el arbol final y los datos que necesito para usarlo despues.
saveRDS(arbol_final, file.path(carpeta_resultados, "arbol_final_m2.rds"))
modelo_completo_m2 <- list(
  modelo = "M2_arbol_regresion",
  arbol = arbol_final,
  configuracion = mejor,
  variables = variables_modelo,
  anios_entrenamiento = 2018:2021
)

saveRDS(modelo_completo_m2,
        file.path(carpeta_resultados, "modelo_completo_m2.rds"))

# La reserva solo se guarda cuando el grupo la active.

if (
  evaluar_reserva &&
  exists("predicciones_reserva_m2")
) {
  write.csv(predicciones_reserva_m2,
            file.path(carpeta_resultados, "predicciones_reserva_m2.csv"),
            row.names = FALSE)
}

# Listo: muestro los archivos que quedaron guardados.
cat("\nRESULTADOS DEL MODELO M2 GUARDADOS\n")
cat("Carpeta:", carpeta_resultados, "\n")
print(list.files(carpeta_resultados))

# 11. REVISAR QUE LAS PREDICCIONES ESTEN BIEN -------------------------------

predicciones_revisar <- read.csv(
  file.path(carpeta_resultados, "predicciones_m2.csv"),
  stringsAsFactors = FALSE)
configuracion_revisar <- readRDS(
  file.path(carpeta_resultados, "configuracion_elegida_m2.rds"))

# Cada variante debe tener la cantidad de eventos que se validaron.
n_predicciones_esperadas <- sum(particiones$rol == "valid")

cat("\nREVISION DE PREDICCIONES\n")
cat("Por respuesta:", n_predicciones_esperadas, "\n")
cat("Total en predicciones_m2.csv:", 2 * n_predicciones_esperadas, "\n")
print(table(predicciones_revisar$variante_respuesta))

stopifnot(nrow(predicciones_revisar) == 2 * n_predicciones_esperadas)
stopifnot(
  identical(sort(unique(predicciones_revisar$variante_respuesta)),
            c("log1p", "original"))
)
stopifnot(
  all(table(predicciones_revisar$variante_respuesta) ==
        n_predicciones_esperadas)
)

stopifnot(!anyNA(predicciones_revisar$y_observado),
          !anyNA(predicciones_revisar$y_predicho),
          all(is.finite(predicciones_revisar$y_observado)),
          all(is.finite(predicciones_revisar$y_predicho)),
          all(predicciones_revisar$y_observado >= 0),
          all(predicciones_revisar$y_predicho >= 0))

stopifnot(!anyDuplicated(predicciones_revisar[
  c("event_id", "particion", "variante_respuesta")
]))
stopifnot(all(predicciones_revisar$uso_wfo == configuracion_revisar$uso_wfo))

stopifnot(
  isTRUE(all.equal(
    predicciones_revisar$error_absoluto,
    abs(predicciones_revisar$y_observado - predicciones_revisar$y_predicho),
    tolerance = 1e-9
  ))
)

# El M2 elegido tambien debe estar dentro de estas predicciones.
predicciones_final_csv <- predicciones_revisar %>%
  filter(variante_respuesta == configuracion_revisar$variante_respuesta) %>%
  mutate(across(all_of(claves_m2), as.character)) %>%
  select(all_of(claves_m2), y_predicho)

comparacion_final <- predicciones_m2_final %>%
  mutate(across(all_of(claves_m2), as.character)) %>%
  select(all_of(claves_m2), y_predicho) %>%
  left_join(predicciones_final_csv, by = claves_m2,
            suffix = c("_modelo", "_csv"))

stopifnot(
  nrow(comparacion_final) == n_predicciones_esperadas,
  !anyNA(comparacion_final$y_predicho_csv),
  all(abs(comparacion_final$y_predicho_modelo -
          comparacion_final$y_predicho_csv) < 1e-6)
)

cat("\nTODAS LAS COMPROBACIONES M2 FUERON SUPERADAS\n")