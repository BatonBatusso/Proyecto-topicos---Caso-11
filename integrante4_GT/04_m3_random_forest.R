################################################################################
# M3: RANDOM FOREST (ranger)  |
################################################################################

library(dplyr)
library(tidyr)
library(ranger)
library(ggplot2)

# Cambiar a TRUE solamente cuando corresponda.
evaluar_reserva  <- FALSE   # 2022-2023: NO activar hasta el cierre
correr_con_source <- FALSE  # sensibilidad opcional: agrega SOURCE (si el equipo la acuerda)

# Rutas (relativas a la carpeta desde donde se ejecuta; ajustar si hace falta,
# por ejemplo "../datos_comunes" si se ejecuta desde integrante4_GT)
dir_comun  <- "../datos_comunes"
dir_salida <- "resultados/m3"
dir.create(dir_salida, recursive = TRUE, showWarnings = FALSE)

# Presupuesto de busqueda
n_combinaciones <- 20
semillas_curva  <- 0:4      # desplazamientos de semilla para la curva de B
umbral_B        <- 0.5      # % de estabilidad exigido a la curva de B


# 1. DATOS COMUNES -----------------------------------------------------------
# particiones es una LISTA de 3 elementos; cada uno trae:
#   particion (1, 2, 3), anio_validacion, train_id y valid_id (valores de event_id).
base_desarrollo <- readRDS(file.path(dir_comun, "base_desarrollo.rds"))
particiones     <- readRDS(file.path(dir_comun, "particiones_desarrollo.rds"))
configuracion   <- readRDS(file.path(dir_comun, "configuracion_comun.rds"))

semilla <- if (!is.null(configuracion$semilla)) configuracion$semilla else 3407
set.seed(semilla)
cat("Semilla utilizada:", semilla, "\n")

# Compatibilidad de nombres. 

normalizar_ids <- function(datos) {
  if (!"event_id" %in% names(datos) && "event_id" %in% names(datos)) {
    datos$event_id <- datos$event_id
  }
  if (!"episode_id" %in% names(datos) && "episode_id" %in% names(datos)) {
    datos$episode_id <- datos$episode_id
  }
  if ("event_id" %in% names(datos))   datos$event_id   <- as.character(datos$event_id)
  if ("episode_id" %in% names(datos)) datos$episode_id <- as.character(datos$episode_id)
  datos
}

adaptar_nombres <- function(datos) {
  datos <- normalizar_ids(datos)
  if (!"dano_usd" %in% names(datos) && "damage_property_usd" %in% names(datos)) {
    datos$dano_usd <- datos$damage_property_usd
  }
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

# Las 10 variables aceptadas por el diccionario (source y otras quedan fuera).
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
if (anyNA(base_desarrollo$dano_usd) || any(!is.finite(base_desarrollo$dano_usd)) ||
    any(base_desarrollo$dano_usd < 0)) {
  stop("La respuesta dano_usd debe estar observada y ser no negativa.")
}
cat("\nEVENTOS DE DESARROLLO:", nrow(base_desarrollo), "\n")


# 2. VENTANAS TEMPORALES -----------------------------------------------------
# V1: 2018 -> 2019; V2: 2018-2019 -> 2020; V3: 2018-2020 -> 2021.
if (!is.list(particiones) || length(particiones) != 3 ||
    !all(c("particion", "anio_validacion", "train_id", "valid_id") %in%
         names(particiones[[1]]))) {
  stop("particiones debe ser una lista de 3 elementos con particion, ",
       "anio_validacion, train_id y valid_id.")
}

train_list <- list()
valid_list <- list()
nombres_ventana <- character(3)
anios_validacion <- numeric(3)
for (i in 1:3) {
  pt <- particiones[[i]]
  nombres_ventana[i]   <- paste0("V", pt$particion)
  anios_validacion[i]  <- as.numeric(pt$anio_validacion)
  id_train <- as.character(pt$train_id)
  id_valid <- as.character(pt$valid_id)
  
  # Los ids de cada particion deben existir en la base y no repetirse
  if (anyDuplicated(c(id_train, id_valid))) {
    stop("Hay event_id repetidos o compartidos entre train y valid en ", nombres_ventana[i])
  }
  if (length(setdiff(c(id_train, id_valid), base_desarrollo$event_id)) > 0) {
    stop("Hay event_id de la particion que no estan en la base en ", nombres_ventana[i])
  }
  
  train_list[[i]] <- base_desarrollo %>% filter(event_id %in% id_train)
  valid_list[[i]] <- base_desarrollo %>% filter(event_id %in% id_valid)
  
  if (nrow(train_list[[i]]) == 0 || nrow(valid_list[[i]]) == 0) {
    stop("Faltan eventos en ", nombres_ventana[i])
  }
  if (nrow(train_list[[i]]) != length(id_train) ||
      nrow(valid_list[[i]]) != length(id_valid)) {
    stop("Las particiones no coinciden con la base en ", nombres_ventana[i])
  }
  # Cronologia: todo el entrenamiento es anterior al anio de validacion
  if (any(valid_list[[i]]$year != anios_validacion[i]) ||
      max(train_list[[i]]$year) >= anios_validacion[i]) {
    stop("Las particiones no respetan el orden temporal en ", nombres_ventana[i])
  }
  # Ningun episodio compartido entre train y valid
  if (length(intersect(train_list[[i]]$episode_id, valid_list[[i]]$episode_id)) > 0) {
    stop("Hay episodios compartidos en ", nombres_ventana[i])
  }
}
stopifnot(identical(nombres_ventana, c("V1", "V2", "V3")))
cat("\nConjuntos de entrenamiento y validacion:\n")
for (i in 1:3) {
  cat(sprintf("  %s: train = %5d filas | valid = %5d filas (valida %d)\n",
              nombres_ventana[i], nrow(train_list[[i]]), nrow(valid_list[[i]]),
              anios_validacion[i]))
}


# 3. PREPROCESAMIENTO: TODO SE APRENDE SOLO EN EL TRAIN ----------------------
# La magnitud se estandariza por event_type; las medianas y los niveles de los
# factores salen del entrenamiento. Categorias nunca vistas pasan a "Otros".
preparar_datos_m3 <- function(train, nuevos, con_wfo = TRUE, con_source = FALSE) {
  num <- c("begin_lat", "begin_lon", "dur_seg", "hora_inicio", "mes_num")
  fac <- c("event_type", "mag_origen", "state",
           if (con_wfo) "wfo", if (con_source) "source")
  
  # Magnitud: media y desvio por tipo, con el entrenamiento
  tipo_tr <- as.character(train$event_type)
  mag_tr  <- as.numeric(train$magnitude)
  mag_tr[!is.finite(mag_tr)] <- NA_real_
  medias  <- tapply(mag_tr, tipo_tr, mean, na.rm = TRUE)
  desvios <- tapply(mag_tr, tipo_tr, sd, na.rm = TRUE)
  medias[!is.finite(medias)] <- 0
  desvios[!is.finite(desvios) | desvios == 0] <- 1
  estandarizar <- function(d) {
    t <- as.character(d$event_type)
    z <- (as.numeric(d$magnitude) - medias[t]) / desvios[t]
    z[!is.finite(z)] <- 0
    unname(z)
  }
  
  # Medianas y niveles aprendidos en train
  med_num <- sapply(num, function(v) {
    x <- as.numeric(train[[v]]); x[!is.finite(x)] <- NA_real_
    m <- median(x, na.rm = TRUE); if (is.finite(m)) m else 0
  })
  limpiar <- function(x) { x <- as.character(x); x[is.na(x) | trimws(x) == ""] <- "Sin_dato"; x }
  niveles <- lapply(fac, function(v) sort(unique(limpiar(train[[v]]))))
  names(niveles) <- fac
  
  armar <- function(d) {
    out <- data.frame(magnitude_z = estandarizar(d))
    for (v in num) {
      x <- as.numeric(d[[v]]); x[!is.finite(x)] <- NA_real_
      x[is.na(x)] <- med_num[[v]]
      out[[v]] <- x
    }
    for (v in fac) {
      x <- limpiar(d[[v]])
      x[!x %in% niveles[[v]]] <- "Otros"
      out[[v]] <- factor(x, levels = unique(c(niveles[[v]], "Otros")))
    }
    out
  }
  list(train = armar(train), valid = armar(nuevos))
}

ajustar_rf_m3 <- function(x_train, y, hp, n_arboles, importancia = "none", semilla = 3407) {
  x_train$y <- y
  ranger(y ~ ., data = x_train, num.trees = n_arboles,
         mtry = min(hp$mtry, ncol(x_train) - 1),
         min.node.size = hp$min.node.size,
         sample.fraction = hp$sample.fraction, replace = TRUE,
         respect.unordered.factors = "order",
         importance = importancia, seed = semilla, num.threads = 1)
}

predecir_rf_m3 <- function(modelo, x_nuevo, variante) {
  pred <- predict(modelo, x_nuevo)$predictions
  if (variante == "log1p") pred <- expm1(pmin(pred, 50))   # retransformacion
  pmax(0, pred)
}

preparados_base <- lapply(1:3, function(i) {
  preparar_datos_m3(train_list[[i]], valid_list[[i]], con_wfo = TRUE)
})
cat("\nPredictores del modelo base:",
    paste(names(preparados_base[[1]]$train), collapse = ", "), "\n")
stopifnot(!any(c("dano_usd", "event_id", "episode_id", "year") %in%
                 names(preparados_base[[1]]$train)))


# 4. NUMERO DE ARBOLES: CURVA CON VARIAS SEMILLAS ----------------------------
# B se fija antes de buscar hiperparametros, con una configuracion intermedia.
bs <- seq(50, 1000, 50)
curva_todas <- do.call(rbind, lapply(semilla + semillas_curva, function(s) {
  do.call(rbind, lapply(1:3, function(i) {
    x <- preparados_base[[i]]
    p_i <- ncol(x$train)
    m <- ajustar_rf_m3(x$train, train_list[[i]]$dano_usd,
                       list(mtry = max(1, round(p_i / 2)), min.node.size = 5,
                            sample.fraction = 0.7), 1000, "none", s)
    P <- predict(m, x$valid, predict.all = TRUE)$predictions
    data.frame(semilla = s, particion = paste0("V", i), B = bs,
               MAE = sapply(bs, function(b) {
                 mean(abs(valid_list[[i]]$dano_usd - rowMeans(P[, 1:b, drop = FALSE])))
               }))
  }))
}))

curva_B <- curva_todas %>%
  group_by(particion, B) %>%
  summarise(MAE_sd = sd(MAE), MAE = mean(MAE), .groups = "drop")

estabilidad_B <- curva_B %>%
  group_by(particion) %>%
  mutate(ref = mean(MAE[B >= 500]), dif = abs(MAE - ref) / ref * 100) %>%
  group_by(B) %>%
  summarise(max_dif = max(dif), .groups = "drop") %>%
  mutate(dif_desde_aqui = rev(cummax(rev(max_dif))))

elegir_B <- function(u) {
  ok <- estabilidad_B$B[estabilidad_B$dif_desde_aqui <= u]
  if (length(ok) > 0) min(ok) else NA_real_
}
B_sugerido <- NA_real_; umbral_usado <- NA_real_
for (u in unique(c(umbral_B, 1, 2))) {
  B_sugerido <- elegir_B(u)
  if (!is.na(B_sugerido)) { umbral_usado <- u; break }
}
if (is.na(B_sugerido)) {
  B_sugerido <- 500
  warning("Ningun B cumple el criterio de estabilidad; se usa B = 500 por defecto.")
}
n_arboles <- as.integer(B_sugerido)
stopifnot(is.finite(n_arboles), n_arboles >= 1)
cat("\nNUMERO DE ARBOLES FIJADO:", n_arboles, "| umbral usado (%):", umbral_usado, "\n")

print(ggplot(curva_B, aes(B, MAE)) +
        geom_line(data = curva_todas, aes(group = semilla), color = "grey70", linewidth = 0.4) +
        geom_line(color = "firebrick", linewidth = 1) +
        geom_vline(xintercept = n_arboles, linetype = 2) +
        facet_wrap(~ particion, scales = "free_y", ncol = 1) + theme_minimal() +
        labs(title = "M3: MAE segun numero de arboles (semillas en gris, promedio en rojo)",
             x = "Numero de arboles", y = "MAE de validacion (USD)"))

saveRDS(list(curva_por_semilla = curva_todas, curva_promedio = curva_B,
             estabilidad = estabilidad_B, B_elegido = n_arboles,
             umbral_usado = umbral_usado, semillas = semilla + semillas_curva),
        file.path(dir_salida, "m3_num_arboles.rds"))


# 5. BUSQUEDA DE HIPERPARAMETROS: 20 CONFIGURACIONES -------------------------
# 19 bosques (m < p) sorteados + 1 bagging (m = p). Se busca por cada respuesta.
p_base <- ncol(preparados_base[[1]]$train)
grilla_total <- expand.grid(mtry = unique(pmax(1, round(p_base * c(.2, .4, .6, .8, 1)))),
                            min.node.size = c(1, 5, 10, 25, 50),
                            sample.fraction = c(.5, .7, .9))
pool <- grilla_total[grilla_total$mtry < p_base, ]
set.seed(semilla)
grilla <- pool[sample(nrow(pool), min(n_combinaciones - 1, nrow(pool))), ]
grilla <- rbind(grilla, data.frame(mtry = p_base, min.node.size = 5, sample.fraction = 1))
rownames(grilla) <- NULL
grilla$configuracion <- paste0("C", sprintf("%02d", seq_len(nrow(grilla))))

resultados <- list(); k <- 0
for (variante in c("original", "log1p")) {
  for (j in seq_len(nrow(grilla))) {
    for (i in 1:3) {
      x <- preparados_base[[i]]
      y <- if (variante == "original") train_list[[i]]$dano_usd else log1p(train_list[[i]]$dano_usd)
      t0 <- proc.time()[["elapsed"]]
      m <- ajustar_rf_m3(x$train, y, grilla[j, ], n_arboles, "none", semilla)
      pred <- predecir_rf_m3(m, x$valid, variante)
      segundos <- proc.time()[["elapsed"]] - t0
      obs <- valid_list[[i]]$dano_usd
      k <- k + 1
      resultados[[k]] <- data.frame(
        variante_respuesta = variante, configuracion = grilla$configuracion[j],
        mtry = grilla$mtry[j], min.node.size = grilla$min.node.size[j],
        sample.fraction = grilla$sample.fraction[j], particion = paste0("V", i),
        n = length(obs), MAE = mean(abs(obs - pred)),
        RMSE = sqrt(mean((obs - pred)^2)), segundos = segundos)
    }
  }
}
tabla_busqueda <- bind_rows(resultados)

resumen_busqueda <- tabla_busqueda %>%
  group_by(variante_respuesta, configuracion, mtry, min.node.size, sample.fraction) %>%
  summarise(sd_MAE = sd(MAE), MAE = weighted.mean(MAE, n),
            RMSE = sqrt(sum(n * RMSE^2) / sum(n)),
            segundos_total = sum(segundos), .groups = "drop")

# Se elige la configuracion de menor MAE ponderado por n.
configuracion_elegida_m3 <- resumen_busqueda %>%
  group_by(variante_respuesta) %>%
  arrange(MAE, sd_MAE, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(num.trees = n_arboles, semilla = semilla)

cat("\nCONFIGURACIONES ELEGIDAS (", nrow(grilla), " PROBADAS POR RESPUESTA)\n", sep = "")
print(configuracion_elegida_m3)

saveRDS(list(grilla = grilla, tabla_busqueda = tabla_busqueda,
             resumen_busqueda = resumen_busqueda,
             configuracion_elegida = configuracion_elegida_m3),
        file.path(dir_salida, "m3_hiperparametros.rds"))
saveRDS(configuracion_elegida_m3, file.path(dir_salida, "m3_configuracion_elegida.rds"))


# 6. AJUSTE CON HIPERPARAMETROS CONGELADOS: ORIGINAL vs LOG1P, CON Y SIN WFO --
# Aqui se guardan predicciones, importancia (permutacion, OOB) y tiempos.
variantes_pred <- data.frame(uso_wfo = c("con_wfo", "sin_wfo"),
                             con_wfo = c(TRUE, FALSE), con_source = c(FALSE, FALSE),
                             stringsAsFactors = FALSE)
if (correr_con_source && "source" %in% names(base_desarrollo)) {
  variantes_pred <- rbind(variantes_pred,
                          data.frame(uso_wfo = "con_source", con_wfo = TRUE, con_source = TRUE))
}

predicciones <- list(); importancias <- list(); tiempos <- list(); k <- 0
for (e in seq_len(nrow(variantes_pred))) {
  preparados <- lapply(1:3, function(i) {
    preparar_datos_m3(train_list[[i]], valid_list[[i]],
                      con_wfo = variantes_pred$con_wfo[e],
                      con_source = variantes_pred$con_source[e])
  })
  for (variante in c("original", "log1p")) {
    cfg <- configuracion_elegida_m3 %>% filter(variante_respuesta == variante)
    for (i in 1:3) {
      x <- preparados[[i]]
      y <- if (variante == "original") train_list[[i]]$dano_usd else log1p(train_list[[i]]$dano_usd)
      t0 <- proc.time()[["elapsed"]]
      m <- ajustar_rf_m3(x$train, y, cfg, n_arboles, "permutation", semilla)
      pred <- predecir_rf_m3(m, x$valid, variante)
      segundos <- proc.time()[["elapsed"]] - t0
      valid <- valid_list[[i]]
      k <- k + 1
      predicciones[[k]] <- data.frame(
        event_id = valid$event_id, episode_id = valid$episode_id,
        particion = paste0("V", i), anio_validacion = anios_validacion[i],
        y_observado = valid$dano_usd, y_predicho = pred,
        modelo = "M3_random_forest", variante_respuesta = variante,
        configuracion = cfg$configuracion, uso_wfo = variantes_pred$uso_wfo[e],
        mag_origen = as.character(valid$mag_origen), stringsAsFactors = FALSE)
      
      importancias[[k]] <- data.frame(
        variable = names(m$variable.importance),
        importancia = as.numeric(m$variable.importance),
        particion = paste0("V", i), variante_respuesta = variante,
        uso_wfo = variantes_pred$uso_wfo[e], stringsAsFactors = FALSE)
      
      tiempos[[k]] <- data.frame(
        particion = paste0("V", i), variante_respuesta = variante,
        uso_wfo = variantes_pred$uso_wfo[e], segundos = segundos,
        stringsAsFactors = FALSE)
    }
  }
}
predicciones_m3 <- bind_rows(predicciones)
importancia_m3  <- bind_rows(importancias)
tiempos_m3      <- bind_rows(tiempos)

stopifnot(!anyNA(predicciones_m3$y_predicho), all(is.finite(predicciones_m3$y_predicho)),
          all(predicciones_m3$y_predicho >= 0))


# 7. RESULTADOS Y SENSIBILIDADES ---------------------------------------------
tabla_ventanas_m3 <- predicciones_m3 %>%
  group_by(uso_wfo, variante_respuesta, particion) %>%
  summarise(n = n(), MAE = mean(abs(y_observado - y_predicho)),
            RMSE = sqrt(mean((y_observado - y_predicho)^2)), .groups = "drop")
cat("\nERROR POR VENTANA: ORIGINAL, LOG1P, CON Y SIN WFO\n")
print(tabla_ventanas_m3, n = Inf)

# Sensibilidad 2: tipo de medicion (granizo / viento medido / viento estimado)
resultados_grupos_m3 <- predicciones_m3 %>%
  group_by(uso_wfo, variante_respuesta, particion, mag_origen) %>%
  summarise(n = n(), MAE = mean(abs(y_observado - y_predicho)),
            RMSE = sqrt(mean((y_observado - y_predicho)^2)), .groups = "drop")
cat("\nMAE Y RMSE POR TIPO DE EVENTO Y MEDICION\n")
print(resultados_grupos_m3, n = Inf)

# Ceros, positivos y extremos (p99 de CADA entrenamiento, igual que M2)
umbrales <- data.frame(
  particion = paste0("V", 1:3),
  umbral = sapply(train_list, function(d) as.numeric(quantile(d$dano_usd, 0.99, na.rm = TRUE)))
)
resultados_extremos_m3 <- predicciones_m3 %>%
  left_join(umbrales, by = "particion") %>%
  mutate(error_absoluto = abs(y_observado - y_predicho),
         grupo_dano = case_when(y_observado == 0 ~ "Cero",
                                y_observado >= umbral ~ "Extremo",
                                TRUE ~ "Positivo_no_extremo")) %>%
  group_by(uso_wfo, variante_respuesta, particion, grupo_dano) %>%
  summarise(MAE = mean(error_absoluto),
            RMSE = sqrt(mean((y_predicho - y_observado)^2)),
            suma_errores = sum(error_absoluto),
            sesgo = mean(y_predicho - y_observado), .groups = "drop") %>%
  group_by(uso_wfo, variante_respuesta, particion) %>%
  mutate(pct_error = 100 * suma_errores / sum(suma_errores)) %>%
  ungroup()
cat("\nERRORES EN CEROS, DANOS POSITIVOS Y EXTREMOS\n")
print(resultados_extremos_m3, n = Inf)

# Sensibilidad 1: importancia original vs log1p (modelo con WFO)
importancia_promedio <- importancia_m3 %>%
  filter(uso_wfo == "sin_wfo") %>%
  group_by(variante_respuesta, variable) %>%
  summarise(importancia = mean(importancia), .groups = "drop") %>%
  group_by(variante_respuesta) %>%
  mutate(importancia_rel = importancia / sum(importancia),
         ranking = rank(-importancia)) %>%
  ungroup()

imp_ancho <- importancia_promedio %>%
  select(variable, variante_respuesta, importancia_rel, ranking) %>%
  pivot_wider(names_from = variante_respuesta, values_from = c(importancia_rel, ranking))

spearman_importancia <- cor(imp_ancho$ranking_original, imp_ancho$ranking_log1p,
                            method = "spearman")
cat("\nIMPORTANCIA (PROMEDIO DE VENTANAS, SIN WFO)\n")
print(imp_ancho %>% arrange(ranking_original), n = Inf)
cat("Spearman entre rankings (original vs log1p):", round(spearman_importancia, 3), "\n")

print(ggplot(importancia_promedio,
             aes(reorder(variable, importancia_rel), importancia_rel, fill = variante_respuesta)) +
        geom_col(position = "dodge") + coord_flip() + theme_minimal() +
        labs(title = "M3: importancia por permutacion (relativa)",
             x = NULL, y = "Importancia relativa", fill = "Respuesta"))

cat("\nTIEMPOS DE AJUSTE (segundos, totales por escenario)\n")
print(tiempos_m3 %>% group_by(uso_wfo, variante_respuesta) %>%
        summarise(segundos = sum(segundos), .groups = "drop"))

# Modelo M3 seleccionado en desarrollo (menor MAE ponderado, sin WFO)
mejor_m3 <- configuracion_elegida_m3 %>% arrange(MAE) %>% slice(1)
cat("\nMODELO M3 SELECCIONADO\n"); print(mejor_m3)


# 8. GUARDAR RESULTADOS (SOLO .rds) ------------------------------------------
saveRDS(predicciones_m3, file.path(dir_salida, "m3_predicciones_desarrollo.rds"))
saveRDS(importancia_m3,  file.path(dir_salida, "m3_importancia.rds"))
saveRDS(tiempos_m3,      file.path(dir_salida, "m3_tiempos.rds"))
saveRDS(list(tabla_ventanas = tabla_ventanas_m3,
             resultados_grupos = resultados_grupos_m3,
             resultados_extremos = resultados_extremos_m3,
             importancia_promedio = importancia_promedio,
             importancia_ancha = imp_ancho,
             spearman_importancia = spearman_importancia,
             modelo_seleccionado = mejor_m3),
        file.path(dir_salida, "m3_resultados_sensibilidades.rds"))
saveRDS(list(fecha = Sys.time(), R = R.version.string,
             sistema = Sys.info()[["sysname"]],
             ranger = as.character(packageVersion("ranger")),
             semilla = semilla, n_arboles = n_arboles),
        file.path(dir_salida, "m3_info_ejecucion.rds"))


# 9. FUNCION FINAL (HIPERPARAMETROS CONGELADOS) ------------------------------
# Mismo estilo que la funcion final de M1: se ajusta con todo el desarrollo y
# se predice sobre eventos nuevos.
ajustar_predecir_m3_final <- function(df_desarrollo, df_nuevos,
                                      config_elegida = configuracion_elegida_m3,
                                      variante = c("original", "log1p"),
                                      con_wfo = TRUE) {
  variante <- match.arg(variante)
  cfg <- config_elegida %>% filter(variante_respuesta == variante) %>% slice(1)
  df_desarrollo <- adaptar_nombres(df_desarrollo)
  df_nuevos     <- adaptar_nombres(df_nuevos)
  x <- preparar_datos_m3(df_desarrollo, df_nuevos, con_wfo = con_wfo)
  y <- if (variante == "original") df_desarrollo$dano_usd else log1p(df_desarrollo$dano_usd)
  modelo <- ajustar_rf_m3(x$train, y, cfg, cfg$num.trees, "none", cfg$semilla)
  pred <- predecir_rf_m3(modelo, x$valid, variante)
  tibble(
    event_id   = as.character(df_nuevos$event_id),
    episode_id = as.character(df_nuevos$episode_id),
    anio       = if ("year" %in% names(df_nuevos)) df_nuevos$year else NA_real_,
    y_predicho = pred, modelo = "M3_random_forest",
    variante_respuesta = variante, configuracion = cfg$configuracion,
    mtry = cfg$mtry, min.node.size = cfg$min.node.size,
    sample.fraction = cfg$sample.fraction, num.trees = cfg$num.trees)
}


# 10. RESERVA 2022-2023: NO ACTIVAR HASTA EL CIERRE ---------------------------
if (evaluar_reserva) {
  base_reserva <- leer_comun("base_prueba_sin_respuesta.rds")
  if (any(c("dano_usd", "damage_property_usd") %in% names(base_reserva))) {
    stop("La reserva de prediccion debe venir SIN respuesta.")
  }
  predicciones_reserva_m3 <- ajustar_predecir_m3_final(
    base_desarrollo, base_reserva, variante = mejor_m3$variante_respuesta)
  cat("\nPREDICCIONES M3 PARA 2022-2023 (SIN VER RESPUESTA)\n")
  print(head(predicciones_reserva_m3, 15))
  saveRDS(predicciones_reserva_m3, file.path(dir_salida, "m3_predicciones_reserva.rds"))
}

