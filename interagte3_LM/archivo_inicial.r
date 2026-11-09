source("preparacion_base_datos.R")

# ==============================================================================
# R/02_m1_elastic_net.R
# PASO 1: Preprocesamiento por ventana, matrices de diseño y grilla
# ==============================================================================

library(glmnet)
library(dplyr)
library(tidyr)

# 2. Definir la fórmula de predictores admisibles con variables separadas
# - Se excluye cz_type porque tiene 1 solo nivel ("C") en Hail y Thunderstorm Wind.
# - Se incluyen mag_hail_in, mag_wind_kt y mag_type_grupo según el plan común.
# - Se incluyen mag_ausente y coordenadas_validas para capturar ausencias en predictores.
formula_m1 <- as.formula(
  "~ is_hail + mag_hail_in + mag_wind_kt + mag_type_grupo + mag_ausente + 
     dur_min + mes_num + hora_inicio + state + source + coordenadas_validas + dist_km - 1"
)

# 3. Función auxiliar para aprender parámetros de imputación SOLO en entrenamiento
aprender_preproc_m1 <- function(df_train) {
  media_hail <- df_train %>%
    filter(event_type == "Hail") %>%
    summarise(m = mean(mag_hail_in, na.rm = TRUE)) %>%
    pull(m)
  
  media_wind <- df_train %>%
    filter(event_type == "Thunderstorm Wind") %>%
    summarise(m = mean(mag_wind_kt, na.rm = TRUE)) %>%
    pull(m)
  
  mediana_dur <- median(df_train$dur_min, na.rm = TRUE)
  
  list(
    media_hail   = ifelse(is.na(media_hail), 0, media_hail),
    media_wind   = ifelse(is.na(media_wind), 0, media_wind),
    mediana_dur  = ifelse(is.na(mediana_dur), 0, mediana_dur),
    niveles_type = levels(df_train$mag_type_grupo),
    niveles_st   = levels(df_train$state),
    niveles_src  = levels(df_train$source)
  )
}

# 4. Función auxiliar para aplicar el preprocesamiento a train o valid
aplicar_preproc_m1 <- function(df, params) {
  df %>%
    mutate(
      # Asegurar consistencia de niveles en factores entre train y valid
      mag_type_grupo = factor(mag_type_grupo, levels = params$niveles_type),
      state          = factor(state, levels = params$niveles_st),
      source         = factor(source, levels = params$niveles_src),
      
      # Imputación estructural (0 cuando no aplica el fenómeno) y por media en train cuando falta
      mag_hail_in = case_when(
        event_type == "Hail" & !is.na(mag_hail_in) ~ mag_hail_in,
        event_type == "Hail" & is.na(mag_hail_in)  ~ params$media_hail,
        TRUE                                       ~ 0
      ),
      mag_wind_kt = case_when(
        event_type == "Thunderstorm Wind" & !is.na(mag_wind_kt) ~ mag_wind_kt,
        event_type == "Thunderstorm Wind" & is.na(mag_wind_kt)  ~ params$media_wind,
        TRUE                                                    ~ 0
      ),
      
      # Resguardo ante posibles NA en duración o distancia espacial
      dur_min = coalesce(dur_min, params$mediana_dur),
      dist_km = coalesce(dist_km, 0)
    )
}

# 5. Construir matrices X e Y aisladas para cada una de las 3 ventanas
ventanas_m1 <- lapply(names(ventanas), function(id_v) {
  v <- ventanas[[id_v]]
  
  # a. Aprender parámetros en entrenamiento y transformar ambos conjuntos
  params_v    <- aprender_preproc_m1(v$train)
  train_clean <- aplicar_preproc_m1(v$train, params_v)
  valid_clean <- aplicar_preproc_m1(v$valid, params_v)
  
  # b. Construir matrices de diseño (glmnet estandariza internamente por defecto)
  x_train <- model.matrix(formula_m1, data = train_clean)
  x_valid <- model.matrix(formula_m1, data = valid_clean)
  
  # c. Respuestas en escala original (USD) y escala log1p (Sensibilidad 1)
  y_train_orig <- train_clean$dano_usd
  y_valid_orig <- valid_clean$dano_usd
  
  y_train_log  <- log1p(train_clean$dano_usd)
  y_valid_log  <- log1p(valid_clean$dano_usd)
  
  # d. Conservar llaves y metadatos para el contrato de entrega y subgrupos
  meta_valid <- valid_clean %>%
    select(event_id, episode_id, year, event_type, mag_origen, state, source) %>%
    mutate(particion = id_v)
  
  list(
    id_ventana   = id_v,
    nombre       = v$nombre,
    params       = params_v,
    x_train      = x_train,
    x_valid      = x_valid,
    y_train_orig = y_train_orig,
    y_valid_orig = y_valid_orig,
    y_train_log  = y_train_log,
    y_valid_log  = y_valid_log,
    meta_valid   = meta_valid
  )
})

names(ventanas_m1) <- names(ventanas)



# ==============================================================================
# BLOQUE DE AUDITORÍA DEL PASO 1
# ==============================================================================
cat("\n==================== AUDITORIA PASO 1 (M1) ====================\n")
for (id_v in names(ventanas_m1)) {
  vm <- ventanas_m1[[id_v]]
  v_orig <- ventanas[[id_v]]
  
  filas_train_ok <- nrow(vm$x_train) == nrow(v_orig$train)
  filas_valid_ok <- nrow(vm$x_valid) == nrow(v_orig$valid)
  cols_iguales   <- identical(colnames(vm$x_train), colnames(vm$x_valid))
  sin_na_train   <- sum(is.na(vm$x_train)) == 0
  sin_na_valid   <- sum(is.na(vm$x_valid)) == 0
  
  cat(sprintf("[%s] %s\n", id_v, vm$nombre))
  cat(sprintf("  - Filas train intactas (%d == %d): %s\n", 
              nrow(vm$x_train), nrow(v_orig$train), filas_train_ok))
  cat(sprintf("  - Filas valid intactas (%d == %d): %s\n", 
              nrow(vm$x_valid), nrow(v_orig$valid), filas_valid_ok))
  cat(sprintf("  - Columnas X idénticas (%d cols): %s\n", 
              ncol(vm$x_train), cols_iguales))
  cat(sprintf("  - Sin NAs en matrices (train/valid): %s / %s\n", 
              sin_na_train, sin_na_valid))
  cat(sprintf("  - Medias aprendidas en train -> Hail: %.2f in | Wind: %.2f kt\n\n",
              vm$params$media_hail, vm$params$media_wind))
}

cat("Columnas generadas en model.matrix:\n")
print(colnames(ventanas_m1$V1$x_train))

for (id_v in names(ventanas_m1)) {
  vm <- ventanas_m1[[id_v]]
  const <- colnames(vm$x_train)[apply(vm$x_train, 2, sd) == 0]
  cat(sprintf("[%s] y sin NA: %s | meta alineada: %s | cols constantes: %s\n",
              id_v,
              !anyNA(vm$y_train_orig) && !anyNA(vm$y_valid_orig),
              nrow(vm$meta_valid) == nrow(vm$x_valid),
              ifelse(length(const) == 0, "ninguna", paste(const, collapse = ", "))))
}


# ==============================================================================
# PASO 2: Definición de grillas (20 configs), ajuste y validación en desarrollo
# ==============================================================================

# 1. Definir presupuesto estricto de 20 configuraciones (Ridge alpha=0, Lasso alpha=1)
# Se adapta el rango de lambda a la desviación estándar de cada escala de Y:
# - En escala original (USD, SD ~ 2e6): exploramos de 10^6 a 10^2
# - En escala log1p (log(1+USD), SD ~ 3.5): exploramos de 10^1 a 10^-3

grilla_m1_orig <- expand.grid(
  alpha  = c(0, 1),
  lambda = 10^seq(6, 2, length.out = 10)
) %>%
  arrange(alpha, desc(lambda)) %>%
  mutate(config_id = sprintf("M1_a%d_L%02d", alpha, rep(1:10, 2)))

grilla_m1_log <- expand.grid(
  alpha  = c(0, 1),
  lambda = 10^seq(1, -3, length.out = 10)
) %>%
  arrange(alpha, desc(lambda)) %>%
  mutate(config_id = sprintf("M1_a%d_L%02d", alpha, rep(1:10, 2)))

# 2. Función para ajustar y validar las 20 configuraciones en las 3 ventanas
ejecutar_busqueda_m1 <- function(ventanas_m1, grilla_m1, variante = c("original", "log1p")) {
  variante <- match.arg(variante)
  
  resultados_ventanas <- list()
  predicciones_lista  <- list()
  tiempos_lista       <- list()
  modelos_guardados   <- list()
  
  for (id_v in names(ventanas_m1)) {
    vm <- ventanas_m1[[id_v]]
    y_tr <- if (variante == "original") vm$y_train_orig else vm$y_train_log
    y_va <- vm$y_valid_orig # Siempre evaluamos en dólares reales (USD)
    
    modelos_guardados[[id_v]] <- list()
    
    for (a in unique(grilla_m1$alpha)) {
      sub_grilla <- grilla_m1 %>%
        filter(alpha == a) %>%
        arrange(desc(lambda))
      
      # Medir tiempo de ajuste para este bloque de alpha
      t_ini <- proc.time()[["elapsed"]]
      fit <- glmnet(
        x           = vm$x_train,
        y           = y_tr,
        family      = "gaussian",
        alpha       = a,
        lambda      = sub_grilla$lambda,
        standardize = TRUE
      )
      t_fin <- proc.time()[["elapsed"]]
      tiempo_seg <- (t_fin - t_ini) / nrow(sub_grilla)
      
      modelos_guardados[[id_v]][[paste0("a", a)]] <- fit
      
      # Predecir para los 10 valores de lambda simultáneamente
      pred_raw_mat <- predict(fit, newx = vm$x_valid, s = sub_grilla$lambda)
      
      # Número de coeficientes no nulos (sin contar el intercepto) por cada lambda
      coefs_mat <- coef(fit, s = sub_grilla$lambda)
      n_activos <- colSums(coefs_mat[-1, , drop = FALSE] != 0)
      
      for (j in seq_len(nrow(sub_grilla))) {
        cfg_id <- sub_grilla$config_id[j]
        lam_j  <- sub_grilla$lambda[j]
        pred_j <- as.numeric(pred_raw_mat[, j])
        
        # Retransformación (si es log1p) y regla de no negatividad en USD
        if (variante == "original") {
          pred_usd <- pmax(0, pred_j)
        } else {
          # Evitar desborde numérico (Inf) si algún extremo lineal supera 50 en escala log
          pred_usd <- pmax(0, expm1(pmin(pred_j, 50)))
        }
        
        # Métricas en dólares (MAE principal, RMSE complementario)
        err <- y_va - pred_usd
        mae_val  <- mean(abs(err))
        rmse_val <- sqrt(mean(err^2))
        
        resultados_ventanas[[length(resultados_ventanas) + 1]] <- tibble(
          variante_respuesta = variante,
          particion          = id_v,
          anio_validacion    = unique(vm$meta_valid$year),
          configuracion      = cfg_id,
          alpha              = a,
          lambda             = lam_j,
          mae_usd            = mae_val,
          rmse_usd           = rmse_val,
          n_vars_activas     = as.integer(n_activos[j]),
          tiempo_seg         = tiempo_seg
        )
        
        predicciones_lista[[paste(id_v, cfg_id, sep = "_")]] <- vm$meta_valid %>%
          mutate(
            anio_validacion    = year,
            y_observado        = y_va,
            y_predicho         = pred_usd,
            modelo             = "M1_elastic_net",
            variante_respuesta = variante,
            configuracion      = cfg_id
          )
      }
      
      tiempos_lista[[length(tiempos_lista) + 1]] <- tibble(
        modelo             = "M1_elastic_net",
        variante_respuesta = variante,
        particion          = id_v,
        alpha              = a,
        n_configuraciones  = nrow(sub_grilla),
        tiempo_total_seg   = t_fin - t_ini
      )
    }
  }
  
  df_detalle <- bind_rows(resultados_ventanas)
  
  # Resumen por configuración a través de las 3 ventanas (Criterio: menor MAE medio)
  df_resumen <- df_detalle %>%
    group_by(variante_respuesta, configuracion, alpha, lambda) %>%
    summarise(
      mae_medio_usd      = mean(mae_usd),
      mae_sd_usd         = sd(mae_usd),
      rmse_medio_usd     = mean(rmse_usd),
      rmse_sd_usd        = sd(rmse_usd),
      vars_activas_media = mean(n_vars_activas),
      tiempo_total_seg   = sum(tiempo_seg),
      .groups = "drop"
    ) %>%
    arrange(mae_medio_usd, rmse_medio_usd, desc(lambda))
  
  mejor_cfg <- df_resumen %>% slice(1)
  
  # Extraer las predicciones de validación únicamente de la configuración ganadora
  preds_ganadora <- bind_rows(predicciones_lista) %>%
    filter(configuracion == mejor_cfg$configuracion)
  
  list(
    variante       = variante,
    detalle        = df_detalle,
    resumen        = df_resumen,
    mejor_config   = mejor_cfg,
    predicciones   = preds_ganadora,
    tiempos        = bind_rows(tiempos_lista),
    modelos        = modelos_guardados
  )
}

# 3. Ejecutar búsqueda en las dos variantes de la respuesta (Sensibilidad 1)
res_m1_orig <- ejecutar_busqueda_m1(ventanas_m1, grilla_m1_orig, variante = "original")
res_m1_log  <- ejecutar_busqueda_m1(ventanas_m1, grilla_m1_log,  variante = "log1p")


# REVISION DE VALORES DE VARIABLE RESPUESTA -------------------------------

# Función auxiliar para calcular el promedio del X% más alto
promedio_top_x <- function(vector_danos, proporcion) {
  # Calcula el cuantil de corte (ej: para 0.01, busca el percentil 99)
  corte <- quantile(vector_danos, probs = 1 - proporcion, na.rm = TRUE)
  # Promedia solo los valores que superan ese corte
  mean(vector_danos[vector_danos >= corte], na.rm = TRUE)
}

# Generar la tabla de resumen por año de validación
tabla_extremos <- base_entrenamiento %>%
  filter(year %in% c(2019, 2020, 2021)) %>%
  group_by(year) %>%
  summarise(
    n_eventos        = n(),
    promedio_100_pct = mean(dano_usd, na.rm = TRUE),
    promedio_top_50  = promedio_top_x(dano_usd, 0.50),
    promedio_top_25  = promedio_top_x(dano_usd, 0.25),
    promedio_top_10  = promedio_top_x(dano_usd, 0.10),
    promedio_top_5   = promedio_top_x(dano_usd, 0.05),
    promedio_top_1   = promedio_top_x(dano_usd, 0.01)
  ) %>%
  arrange(year)

# Imprimir la tabla para visualizar
print(tabla_extremos)



# MAE_USD POR AÑO DE VALIDACION -------------------------------------------

tabla_mae <- res_m1_orig$detalle %>%
  select(configuracion, anio_validacion, mae_usd) %>%
  pivot_wider(
    names_from   = anio_validacion,
    values_from  = mae_usd,
    names_prefix = "mae_usd_"
  )

tabla_mae


# ==============================================================================
# BLOQUE DE AUDITORÍA DEL PASO 2 (CON GRILLAS ESCALADAS)
# ==============================================================================
cat("\n==================== AUDITORIA PASO 2 (M1) ====================\n")

cat("1. TOP 5 CONFIGURACIONES - ESCALA ORIGINAL (ordenadas por MAE medio en USD):\n")
print(res_m1_orig$resumen %>% select(configuracion, alpha, lambda, mae_medio_usd, rmse_medio_usd, vars_activas_media) %>% head(5))

cat("\n2. TOP 5 CONFIGURACIONES - ESCALA LOG1P (evaluadas en USD tras expm1):\n")
print(res_m1_log$resumen %>% select(configuracion, alpha, lambda, mae_medio_usd, rmse_medio_usd, vars_activas_media) %>% head(5))

cat("\n3. CONFIGURACIONES ELEGIDAS EN DESARROLLO:\n")
cat(sprintf("  - Original: %s (alpha = %d, lambda = %.4f) | MAE = $%.2f | RMSE = $%.2f | Vars activas = %.1f\n",
            res_m1_orig$mejor_config$configuracion, res_m1_orig$mejor_config$alpha,
            res_m1_orig$mejor_config$lambda, res_m1_orig$mejor_config$mae_medio_usd,
            res_m1_orig$mejor_config$rmse_medio_usd, res_m1_orig$mejor_config$vars_activas_media))

cat(sprintf("  - Log1p:    %s (alpha = %d, lambda = %.4f) | MAE = $%.2f | RMSE = $%.2f | Vars activas = %.1f\n",
            res_m1_log$mejor_config$configuracion, res_m1_log$mejor_config$alpha,
            res_m1_log$mejor_config$lambda, res_m1_log$mejor_config$mae_medio_usd,
            res_m1_log$mejor_config$rmse_medio_usd, res_m1_log$mejor_config$vars_activas_media))

cat("\n4. RANGO DE VARIABLES ACTIVAS EXPLORADO EN LASSO (alpha = 1):\n")
rango_orig <- res_m1_orig$resumen %>% filter(alpha == 1) %>% summarise(min_v = min(vars_activas_media), max_v = max(vars_activas_media))
rango_log  <- res_m1_log$resumen  %>% filter(alpha == 1) %>% summarise(min_v = min(vars_activas_media), max_v = max(vars_activas_media))
cat(sprintf("  - Escala Original: de %.1f a %.1f variables activas\n", rango_orig$min_v, rango_orig$max_v))
cat(sprintf("  - Escala Log1p:    de %.1f a %.1f variables activas\n", rango_log$min_v, rango_log$max_v))

cat("\n5. CHEQUEO DE INTEGRIDAD DE PREDICCIONES (V1 + V2 + V3 = 9975 filas):\n")
for (obj in list(res_m1_orig, res_m1_log)) {
  p <- obj$predicciones
  cat(sprintf("  [%s] Filas: %d | NAs: %d | Inf: %d | Negativos (<0): %d | Min: $%.2f | Max: $%.2f\n",
              obj$variante, nrow(p), sum(is.na(p$y_predicho)), sum(is.infinite(p$y_predicho)),
              sum(p$y_predicho < 0), min(p$y_predicho), max(p$y_predicho)))
}
cat("===============================================================\n")


# ==============================================================================
# PASO 3: Coeficientes, Sensibilidad 2 (Subgrupos) y Archivos del Contrato
# ==============================================================================

# 1. Extraer coeficientes de la configuración ganadora en cada ventana
extraer_coeficientes_m1 <- function(res_obj) {
  cfg     <- res_obj$mejor_config
  a_win   <- cfg$alpha
  lam_win <- cfg$lambda
  
  lista_coefs <- lapply(names(res_obj$modelos), function(id_v) {
    fit   <- res_obj$modelos[[id_v]][[paste0("a", a_win)]]
    mat_c <- as.matrix(coef(fit, s = lam_win))
    tibble(
      variante_respuesta = res_obj$variante,
      configuracion      = cfg$configuracion,
      alpha              = a_win,
      lambda             = lam_win,
      particion          = id_v,
      termino            = rownames(mat_c),
      coeficiente        = as.numeric(mat_c[, 1]),
      activo             = as.numeric(mat_c[, 1]) != 0
    )
  })
  bind_rows(lista_coefs)
}

coefs_m1 <- bind_rows(
  extraer_coeficientes_m1(res_m1_orig),
  extraer_coeficientes_m1(res_m1_log)
)

# Resumen de estabilidad y magnitud de coeficientes en las 3 ventanas
resumen_coefs_m1 <- coefs_m1 %>%
  filter(termino != "(Intercept)") %>%
  group_by(variante_respuesta, configuracion, termino) %>%
  summarise(
    veces_activo = sum(activo),
    coef_V1      = coeficiente[particion == "V1"],
    coef_V2      = coeficiente[particion == "V2"],
    coef_V3      = coeficiente[particion == "V3"],
    coef_medio   = mean(coeficiente),
    .groups      = "drop"
  ) %>%
  arrange(variante_respuesta, desc(veces_activo), desc(abs(coef_medio)))

# 2. Sensibilidad 2: Desempeño por tipo y calidad de medición (granizo, viento_medido, viento_estimado)
calcular_subgrupos_m1 <- function(df_preds) {
  # a. Métricas por ventana y subgrupo
  por_ventana <- df_preds %>%
    group_by(modelo, variante_respuesta, configuracion, particion, anio_validacion, mag_origen) %>%
    summarise(
      n_eventos    = n(),
      mae_usd      = mean(abs(y_observado - y_predicho)),
      rmse_usd     = sqrt(mean((y_observado - y_predicho)^2)),
      mediana_obs  = median(y_observado),
      mediana_pred = median(y_predicho),
      .groups      = "drop"
    )
  
  # b. Resumen inter-ventana por subgrupo
  resumen_subgrupo <- por_ventana %>%
    group_by(modelo, variante_respuesta, configuracion, mag_origen) %>%
    summarise(
      n_total_valid  = sum(n_eventos),
      mae_medio_usd  = mean(mae_usd),
      mae_sd_usd     = sd(mae_usd),
      rmse_medio_usd = mean(rmse_usd),
      rmse_sd_usd    = sd(rmse_usd),
      .groups        = "drop"
    )
  
  list(por_ventana = por_ventana, resumen = resumen_subgrupo)
}

sub_orig <- calcular_subgrupos_m1(res_m1_orig$predicciones)
sub_log  <- calcular_subgrupos_m1(res_m1_log$predicciones)

resultados_subgrupos_m1 <- bind_rows(sub_orig$resumen, sub_log$resumen)
detalle_subgrupos_m1    <- bind_rows(sub_orig$por_ventana, sub_log$por_ventana)

# 3. Construir archivo principal según el CONTRATO DE ENTREGA (Sección 4 del Plan)
predicciones_desarrollo_m1 <- bind_rows(
  res_m1_orig$predicciones,
  res_m1_log$predicciones
) %>%
  transmute(
    EVENT_ID           = event_id,
    EPISODE_ID         = episode_id,
    particion          = particion,
    anio_validacion    = anio_validacion,
    y_observado        = y_observado,
    y_predicho         = y_predicho,
    modelo             = modelo,
    variante_respuesta = variante_respuesta,
    configuracion      = configuracion
  )



# GUARDADO ----------------------------------------------------------------



ruta_out_m1 <- "interagte3_LM/Resultados"
dir.create(ruta_out_m1, recursive = TRUE, showWarnings = FALSE)

saveRDS(predicciones_desarrollo_m1, file.path(ruta_out_m1, "predicciones_desarrollo_m1.rds"))

configuracion_elegida_m1 <- bind_rows(res_m1_orig$mejor_config, res_m1_log$mejor_config)
saveRDS(configuracion_elegida_m1, file.path(ruta_out_m1, "configuracion_elegida_m1.rds"))

tabla_busqueda_m1 <- bind_rows(res_m1_orig$resumen, res_m1_log$resumen)
write_csv(tabla_busqueda_m1, file.path(ruta_out_m1, "tabla_busqueda_m1.csv"))

tiempos_m1 <- bind_rows(res_m1_orig$tiempos, res_m1_log$tiempos)
write_csv(tiempos_m1, file.path(ruta_out_m1, "tiempos_m1.csv"))

write_csv(resultados_subgrupos_m1, file.path(ruta_out_m1, "resultados_subgrupos_m1.csv"))
write_csv(detalle_subgrupos_m1,    file.path(ruta_out_m1, "detalle_subgrupos_ventana_m1.csv"))
write_csv(coefs_m1,                file.path(ruta_out_m1, "coeficientes_ventanas_m1.csv"))
write_csv(resumen_coefs_m1,        file.path(ruta_out_m1, "resumen_coeficientes_m1.csv"))


# ==============================================================================
# BLOQUE DE AUDITORÍA DEL PASO 3
# ==============================================================================
cat("\n==================== AUDITORIA PASO 3 (M1) ====================\n")

cat("1. VARIABLES SELECCIONADAS POR LASSO EN AL MENOS 1 VENTANA (Escala Original):\n")
print(resumen_coefs_m1 %>% filter(variante_respuesta == "original", veces_activo > 0))

cat("\n2. VARIABLES SELECCIONADAS POR LASSO EN AL MENOS 1 VENTANA (Escala Log1p):\n")
print(resumen_coefs_m1 %>% filter(variante_respuesta == "log1p", veces_activo > 0))

cat("\n3. SENSIBILIDAD 2 - DESEMPEÑO POR SUBGRUPO DE MEDICIÓN (Promedio 3 ventanas):\n")
print(resultados_subgrupos_m1 %>% select(variante_respuesta, mag_origen, n_total_valid, mae_medio_usd, rmse_medio_usd))

cat("\n4. VERIFICACIÓN DEL CONTRATO DE ENTREGA (predicciones_desarrollo_m1.rds):\n")
cols_contrato <- c("EVENT_ID", "EPISODE_ID", "particion", "anio_validacion",
                   "y_observado", "y_predicho", "modelo", "variante_respuesta", "configuracion")
cumple_cols <- identical(names(predicciones_desarrollo_m1), cols_contrato)
cat("  - Columnas exactas según contrato:", cumple_cols, "\n")
cat("  - Dimensiones (9975 filas x 2 variantes = 19950):", nrow(predicciones_desarrollo_m1), "filas x", ncol(predicciones_desarrollo_m1), "cols\n")
cat("  - Duplicados por (EVENT_ID, particion, variante_respuesta):",
    sum(duplicated(predicciones_desarrollo_m1[, c("EVENT_ID", "particion", "variante_respuesta")])), "\n")
cat("  - Archivos guardados en resultados/m1/:\n")
print(list.files("resultados/m1"))
cat("===============================================================\n")





# ==============================================================================
# PASO 4: Función final congelada, figuras obligatorias y control de filas
# ==============================================================================

library(ggplot2)

# 1. Definir la función final de M1 para evaluación en reserva (2022-2023)
ajustar_predecir_m1_final <- function(df_desarrollo,
                                      df_nuevos,
                                      config_elegida = configuracion_elegida_m1,
                                      variante = c("log1p", "original")) {
  variante <- match.arg(variante)
  
  # a. Extraer hiperparámetros congelados para la variante solicitada
  cfg <- config_elegida %>% filter(variante_respuesta == variante) %>% slice(1)
  alpha_fijo  <- cfg$alpha
  lambda_fijo <- cfg$lambda
  cfg_nombre  <- cfg$configuracion
  
  # b. Aprender agrupación de 'source' (> 2%) exclusivamente en todo desarrollo
  frec_source <- df_desarrollo %>%
    count(source) %>%
    mutate(porcentaje = (n / sum(n)) * 100)
  
  source_retener <- frec_source %>%
    filter(porcentaje > 2) %>%
    pull(source) %>%
    as.character()
  
  dev_src <- df_desarrollo %>%
    mutate(
      source = if_else(as.character(source) %in% source_retener, as.character(source), "Otros"),
      source = factor(source)
    )
  
  nuevos_src <- df_nuevos %>%
    mutate(
      source = if_else(as.character(source) %in% source_retener, as.character(source), "Otros"),
      source = factor(source, levels = levels(dev_src$source))
    )
  
  # c. Aprender parámetros de imputación física en desarrollo y aplicar a ambos
  params_dev   <- aprender_preproc_m1(dev_src)
  dev_clean    <- aplicar_preproc_m1(dev_src, params_dev)
  nuevos_clean <- aplicar_preproc_m1(nuevos_src, params_dev)
  
  # d. Construir matrices de diseño (formula_m1 no requiere variable respuesta)
  x_dev    <- model.matrix(formula_m1, data = dev_clean)
  x_nuevos <- model.matrix(formula_m1, data = nuevos_clean)
  
  # e. Definir respuesta de entrenamiento según la variante
  y_dev <- if (variante == "original") dev_clean$dano_usd else log1p(dev_clean$dano_usd)
  
  # Secuencia de lambda correspondiente para estabilidad numérica de glmnet
  seq_lambda <- if (variante == "original") 10^seq(6, 2, length.out = 10) else 10^seq(1, -3, length.out = 10)
  seq_lambda <- sort(unique(c(seq_lambda, lambda_fijo)), decreasing = TRUE)
  
  # f. Ajustar modelo final en todo desarrollo
  fit_final <- glmnet(
    x           = x_dev,
    y           = y_dev,
    family      = "gaussian",
    alpha       = alpha_fijo,
    lambda      = seq_lambda,
    standardize = TRUE
  )
  
  # g. Predecir sobre los nuevos eventos en el lambda congelado
  pred_raw <- as.numeric(predict(fit_final, newx = x_nuevos, s = lambda_fijo))
  
  pred_usd <- if (variante == "original") {
    pmax(0, pred_raw)
  } else {
    pmax(0, expm1(pmin(pred_raw, 50)))
  }
  
  # h. Devolver predicciones con formato estandarizado
  tibble(
    EVENT_ID           = nuevos_clean$event_id,
    EPISODE_ID         = nuevos_clean$episode_id,
    anio               = nuevos_clean$year,
    y_predicho         = pred_usd,
    modelo             = "M1_elastic_net",
    variante_respuesta = variante,
    configuracion      = cfg_nombre,
    alpha              = alpha_fijo,
    lambda             = lambda_fijo
  )
}

# Guardar las funciones de preprocesamiento, fórmula y función final en interagte3_LM
dump(
  list = c("formula_m1", "aprender_preproc_m1", "aplicar_preproc_m1", "ajustar_predecir_m1_final"),
  file = file.path(ruta_out_m1, "funcion_final_m1.R")
)


# 2. Guardar control de filas comunes de validación en interagte3_LM
control_filas_m1 <- predicciones_desarrollo_m1 %>%
  filter(variante_respuesta == "log1p") %>%
  select(particion, anio_validacion, EVENT_ID, EPISODE_ID) %>%
  arrange(particion, EVENT_ID)

saveRDS(control_filas_m1, file.path(ruta_out_m1, "control_filas_validacion_m1.rds"))


# 3. Generar Figura Principal de M1 en interagte3_LM
df_plot_m1 <- tabla_busqueda_m1 %>%
  mutate(
    tipo_penalizacion = if_else(alpha == 1, "Lasso (alpha = 1)", "Ridge (alpha = 0)"),
    escala_etiqueta   = if_else(
      variante_respuesta == "original",
      "Escala Original (Y en USD)",
      "Escala Transformada (log(1 + Y), evaluada en USD)"
    )
  )

optimos_plot <- configuracion_elegida_m1 %>%
  mutate(
    tipo_penalizacion = if_else(alpha == 1, "Lasso (alpha = 1)", "Ridge (alpha = 0)"),
    escala_etiqueta   = if_else(
      variante_respuesta == "original",
      "Escala Original (Y en USD)",
      "Escala Transformada (log(1 + Y), evaluada en USD)"
    )
  )

fig_m1 <- ggplot(df_plot_m1, aes(x = log10(lambda), y = mae_medio_usd, color = tipo_penalizacion)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  geom_point(
    data = optimos_plot,
    aes(x = log10(lambda), y = mae_medio_usd),
    color = "black", fill = "gold", shape = 21, size = 4, stroke = 1.1
  ) +
  facet_wrap(~ escala_etiqueta, scales = "free") +
  labs(
    title    = "M1 (Elastic Net): Desempeño de validación en desarrollo según penalización",
    subtitle = "Promedio de MAE en las 3 ventanas expansivas (2018-2021). Círculo dorado indica la configuración elegida.",
    x        = "Log10(lambda)",
    y        = "MAE medio de validación (USD nominales)",
    color    = "Tipo de regularización"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", strip.text = element_text(face = "bold"))

ggsave(file.path(ruta_out_m1, "figura_principal_m1.png"), fig_m1, width = 10, height = 5, dpi = 300)
ggsave(file.path(ruta_out_m1, "figura_principal_m1.pdf"), fig_m1, width = 10, height = 5)


# 4. Generar Diagrama Temporal de Validación y Episodios en interagte3_LM
conteo_anual <- eventos_sin_na %>%
  group_by(year) %>%
  summarise(
    n_eventos   = n(),
    n_episodios = n_distinct(episode_id),
    .groups     = "drop"
  )

esquema_bloques <- tibble::tribble(
  ~etapa,                  ~year, ~rol,
  "Ventana 1 (Desarrollo)", 2018L, "Entrenamiento",
  "Ventana 1 (Desarrollo)", 2019L, "Validación",
  "Ventana 2 (Desarrollo)", 2018L, "Entrenamiento",
  "Ventana 2 (Desarrollo)", 2019L, "Entrenamiento",
  "Ventana 2 (Desarrollo)", 2020L, "Validación",
  "Ventana 3 (Desarrollo)", 2018L, "Entrenamiento",
  "Ventana 3 (Desarrollo)", 2019L, "Entrenamiento",
  "Ventana 3 (Desarrollo)", 2020L, "Entrenamiento",
  "Ventana 3 (Desarrollo)", 2021L, "Validación",
  "Evaluación Final",       2018L, "Ajuste Final (Desarrollo)",
  "Evaluación Final",       2019L, "Ajuste Final (Desarrollo)",
  "Evaluación Final",       2020L, "Ajuste Final (Desarrollo)",
  "Evaluación Final",       2021L, "Ajuste Final (Desarrollo)",
  "Evaluación Final",       2022L, "Reserva de Prueba",
  "Evaluación Final",       2023L, "Reserva de Prueba"
) %>%
  left_join(conteo_anual, by = "year") %>%
  mutate(
    etapa    = factor(etapa, levels = c("Evaluación Final", "Ventana 3 (Desarrollo)",
                                        "Ventana 2 (Desarrollo)", "Ventana 1 (Desarrollo)")),
    etiqueta = sprintf("%s\n(%d ev / %d ep)", rol, n_eventos, n_episodios)
  )

fig_diagrama <- ggplot(esquema_bloques, aes(x = factor(year), y = etapa, fill = rol)) +
  geom_tile(color = "white", linewidth = 1.5, width = 0.92, height = 0.72) +
  geom_text(aes(label = etiqueta), size = 3.1, fontface = "bold", color = "white") +
  scale_fill_manual(values = c(
    "Entrenamiento"             = "#2b5c8f",
    "Validación"                = "#d95f02",
    "Ajuste Final (Desarrollo)" = "#1b7837",
    "Reserva de Prueba"         = "#762a83"
  )) +
  labs(
    title    = "Diseño de Validación Temporal con Ventana Expansiva y Preservación de Episodios",
    subtitle = "Conteo de eventos válidos (ev) y episodios independientes (ep) por año (Caso 11 NOAA, 2018-2023)",
    x        = "Año del registro meteorológico",
    y        = NULL,
    fill     = "Rol del bloque temporal"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid = element_blank())

ggsave(file.path(ruta_out_m1, "diagrama_validacion_temporal.png"), fig_diagrama, width = 10.5, height = 5.2, dpi = 300)
ggsave(file.path(ruta_out_m1, "diagrama_validacion_temporal.pdf"), fig_diagrama, width = 10.5, height = 5.2)


# ==============================================================================
# BLOQUE DE AUDITORÍA DEL PASO 4 (SIMULACRO EN SECO Y ARCHIVOS EN interagte3_LM)
# ==============================================================================
cat("\n==================== AUDITORIA PASO 4 (M1) ====================\n")

# Simulacro: Entrenar con años 2018-2020 y predecir 2021 usando base_entrenamiento_sin_respuesta
dev_sim    <- base_entrenamiento %>% filter(year <= 2020)
nuevos_sim <- base_entrenamiento_sin_respuesta %>% filter(year == 2021) %>% select(-dano_usd)

test_fn_log  <- ajustar_predecir_m1_final(dev_sim, nuevos_sim, configuracion_elegida_m1, variante = "log1p")
test_fn_orig <- ajustar_predecir_m1_final(dev_sim, nuevos_sim, configuracion_elegida_m1, variante = "original")

# Comparar contra las predicciones que ya teníamos en V3 para confirmar reproducibilidad exacta
pred_v3_log <- predicciones_desarrollo_m1 %>%
  filter(particion == "V3", variante_respuesta == "log1p") %>%
  pull(y_predicho)

dif_max_v3 <- max(abs(test_fn_log$y_predicho - pred_v3_log))

cat("1. PRUEBA DE LA FUNCIÓN FINAL (Simulacro 2018-2020 -> 2021 sin columna respuesta):\n")
cat(sprintf("  - Filas predichas (esperadas 2653): %d\n", nrow(test_fn_log)))
cat(sprintf("  - NAs o negativos en predicción: %d\n", sum(is.na(test_fn_log$y_predicho) | test_fn_log$y_predicho < 0)))
cat(sprintf("  - Diferencia máxima vs predicción V3 del Paso 2: %.8f USD\n", dif_max_v3))

cat("\n2. VERIFICACIÓN FINAL DE ENTREGABLES EN CARPETA:", ruta_out_m1, "\n")
archivos_finales <- file.path(ruta_out_m1, c(
  "predicciones_desarrollo_m1.rds",
  "configuracion_elegida_m1.rds",
  "tabla_busqueda_m1.csv",
  "tiempos_m1.csv",
  "resultados_subgrupos_m1.csv",
  "detalle_subgrupos_ventana_m1.csv",
  "coeficientes_ventanas_m1.csv",
  "resumen_coeficientes_m1.csv",
  "funcion_final_m1.R",
  "control_filas_validacion_m1.rds",
  "figura_principal_m1.png",
  "figura_principal_m1.pdf",
  "diagrama_validacion_temporal.png",
  "diagrama_validacion_temporal.pdf"
))

tabla_entregables <- tibble(
  archivo = basename(archivos_finales),
  existe  = file.exists(archivos_finales),
  kb      = round(file.info(archivos_finales)$size / 1024, 1)
)
print(tabla_entregables, n = 20)
cat("Total de archivos generados en", ruta_out_m1, ":", sum(tabla_entregables$existe), "de", nrow(tabla_entregables), "\n")
cat("===============================================================\n")



# ==============================================================================
# PASO 5: Benchmark contra Referencias (Trivial/Mediana) y Control de Filas (#18)
# ==============================================================================

# 0. Resguardo por si se ejecuta en una sesión limpia sin reajustar glmnet
ruta_out_m1 <- "interagte3_LM/Resultados"
if (!exists("ventanas")) source("preparacion_base_datos.R")
if (!exists("predicciones_desarrollo_m1")) {
  predicciones_desarrollo_m1 <- readRDS(file.path(ruta_out_m1, "predicciones_desarrollo_m1.rds"))
}
if (!exists("control_filas_m1")) {
  control_filas_m1 <- readRDS(file.path(ruta_out_m1, "control_filas_validacion_m1.rds"))
}

# 1. Resumen estructural de las 3 ventanas y preservación de EPISODE_ID (Sección 3)
resumen_ventanas_diseno <- bind_rows(lapply(names(ventanas), function(id_v) {
  tr <- ventanas[[id_v]]$train
  va <- ventanas[[id_v]]$valid
  ep_cruzados <- length(intersect(unique(tr$episode_id), unique(va$episode_id)))
  
  tibble(
    particion          = id_v,
    anios_train        = paste(sort(unique(tr$year)), collapse = "-"),
    anio_valid         = unique(va$year),
    n_eventos_train    = nrow(tr),
    n_episodios_train  = n_distinct(tr$episode_id),
    n_eventos_valid    = nrow(va),
    n_episodios_valid  = n_distinct(va$episode_id),
    episodios_cruzados = ep_cruzados,
    prop_ceros_train   = mean(tr$dano_usd == 0) * 100,
    prop_ceros_valid   = mean(va$dano_usd == 0) * 100,
    mediana_train_usd  = median(tr$dano_usd),
    media_train_usd    = mean(tr$dano_usd),
    mediana_valid_usd  = median(va$dano_usd),
    media_valid_usd    = mean(va$dano_usd)
  )
}))

# 2. Construir predicciones de Referencias (Cero, Mediana Global y Mediana por Tipo)
#    aprendidas estrictamente en el entrenamiento de cada ventana
preds_referencias <- bind_rows(lapply(names(ventanas), function(id_v) {
  tr <- ventanas[[id_v]]$train
  va <- ventanas[[id_v]]$valid
  
  # a. Mediana global en entrenamiento
  med_global <- median(tr$dano_usd, na.rm = TRUE)
  
  # b. Mediana por EVENT_TYPE en entrenamiento (con respaldo en med_global)
  med_por_tipo <- tr %>%
    group_by(event_type) %>%
    summarise(med_tipo = median(dano_usd, na.rm = TRUE), .groups = "drop")
  
  va_join <- va %>%
    left_join(med_por_tipo, by = "event_type") %>%
    mutate(med_tipo = coalesce(med_tipo, med_global))
  
  bind_rows(
    tibble(
      EVENT_ID           = va$event_id,
      EPISODE_ID         = va$episode_id,
      particion          = id_v,
      anio_validacion    = va$year,
      y_observado        = va$dano_usd,
      y_predicho         = 0,
      modelo             = "trivial_cero",
      variante_respuesta = "original",
      configuracion      = "constante_0_usd"
    ),
    tibble(
      EVENT_ID           = va$event_id,
      EPISODE_ID         = va$episode_id,
      particion          = id_v,
      anio_validacion    = va$year,
      y_observado        = va$dano_usd,
      y_predicho         = med_global,
      modelo             = "referencia_global",
      variante_respuesta = "original",
      configuracion      = sprintf("mediana_global ($%.0f)", med_global)
    ),
    tibble(
      EVENT_ID           = va$event_id,
      EPISODE_ID         = va$episode_id,
      particion          = id_v,
      anio_validacion    = va$year,
      y_observado        = va$dano_usd,
      y_predicho         = va_join$med_tipo,
      modelo             = "referencia_tipo",
      variante_respuesta = "original",
      configuracion      = "mediana_por_event_type"
    )
  )
}))

# 3. Unir M1 (original y log1p) con las Referencias sobre las mismas 9,975 filas
mapa_subgrupos <- bind_rows(lapply(names(ventanas), function(id_v) {
  ventanas[[id_v]]$valid %>%
    transmute(particion = id_v, EVENT_ID = event_id, mag_origen = mag_origen, event_type = event_type)
}))

comparacion_completa <- bind_rows(
  preds_referencias,
  predicciones_desarrollo_m1
) %>%
  left_join(mapa_subgrupos, by = c("particion", "EVENT_ID"))

# a. Resumen global e inter-ventana (V1, V2, V3)
resumen_m1_vs_ref_ventana <- comparacion_completa %>%
  group_by(modelo, variante_respuesta, particion, anio_validacion) %>%
  summarise(
    n_valid      = n(),
    mae_usd      = mean(abs(y_observado - y_predicho)),
    rmse_usd     = sqrt(mean((y_observado - y_predicho)^2)),
    pred_mediana = median(y_predicho),
    pred_media   = mean(y_predicho),
    pred_max     = max(y_predicho),
    .groups      = "drop"
  )

tabla_m1_vs_referencias <- resumen_m1_vs_ref_ventana %>%
  group_by(modelo, variante_respuesta) %>%
  summarise(
    mae_V1         = mae_usd[particion == "V1"],
    mae_V2         = mae_usd[particion == "V2"],
    mae_V3         = mae_usd[particion == "V3"],
    mae_medio_usd  = mean(mae_usd),
    mae_sd_usd     = sd(mae_usd),
    rmse_medio_usd = mean(rmse_usd),
    rmse_sd_usd    = sd(rmse_usd),
    pred_mediana   = mean(pred_mediana),
    pred_media     = mean(pred_media),
    .groups        = "drop"
  ) %>%
  mutate(
    dif_mae_vs_ref_global = mae_medio_usd - mae_medio_usd[modelo == "referencia_global"][1],
    dif_mae_vs_ref_tipo   = mae_medio_usd - mae_medio_usd[modelo == "referencia_tipo"][1]
  ) %>%
  arrange(mae_medio_usd)

# b. Resumen por subgrupos de medición (Sensibilidad 2 comparada contra referencias)
subgrupos_m1_vs_ref <- comparacion_completa %>%
  group_by(modelo, variante_respuesta, particion, mag_origen) %>%
  summarise(
    n_eventos = n(),
    mae_usd   = mean(abs(y_observado - y_predicho)),
    rmse_usd  = sqrt(mean((y_observado - y_predicho)^2)),
    .groups   = "drop"
  ) %>%
  group_by(mag_origen, modelo, variante_respuesta) %>%
  summarise(
    n_total_valid  = sum(n_eventos),
    mae_medio_usd  = mean(mae_usd),
    mae_sd_usd     = sd(mae_usd),
    rmse_medio_usd = mean(rmse_usd),
    .groups        = "drop"
  ) %>%
  arrange(mag_origen, mae_medio_usd)

# 4. Función verificadora de filas comunes entre los 4 procedimientos (Resp. #18)
verificar_filas_comunes_modelos <- function(control_base, archivos_rds) {
  ref_ord <- control_base %>%
    arrange(particion, EVENT_ID) %>%
    select(particion, anio_validacion, EVENT_ID, EPISODE_ID)
  
  bind_rows(lapply(archivos_rds, function(ruta) {
    if (!file.exists(ruta)) {
      return(tibble(archivo = basename(ruta), existe = FALSE))
    }
    df <- readRDS(ruta)
    cols_req <- c("EVENT_ID", "EPISODE_ID", "particion", "anio_validacion",
                  "y_observado", "y_predicho", "modelo", "variante_respuesta", "configuracion")
    tiene_cols <- all(cols_req %in% names(df))
    
    df %>%
      group_by(modelo, variante_respuesta) %>%
      summarise(
        archivo             = basename(ruta),
        existe              = TRUE,
        columnas_contrato   = tiene_cols,
        n_filas             = n(),
        n_V1                = sum(particion == "V1"),
        n_V2                = sum(particion == "V2"),
        n_V3                = sum(particion == "V3"),
        ids_exactos_9975    = identical(
          tibble(particion = particion, anio_validacion = anio_validacion,
                 EVENT_ID = EVENT_ID, EPISODE_ID = EPISODE_ID) %>% arrange(particion, EVENT_ID),
          ref_ord
        ),
        sin_na_pred         = sum(is.na(y_predicho)) == 0,
        sin_negativos_pred  = sum(y_predicho < 0, na.rm = TRUE) == 0,
        .groups = "drop"
      )
  }))
}

# Detectar automáticamente cualquier predicciones_desarrollo_*.rds presente en el proyecto
rutas_candidatas <- unique(c(
  file.path(ruta_out_m1, "predicciones_desarrollo_m1.rds"),
  list.files(pattern = "^predicciones_desarrollo_.*\\.rds$", recursive = TRUE, full.names = TRUE)
))

tabla_control_filas <- verificar_filas_comunes_modelos(control_filas_m1, rutas_candidatas)

# Guardar las 3 tablas diagnósticas en interagte3_LM/Resultados
write_csv(resumen_ventanas_diseno, file.path(ruta_out_m1, "resumen_ventanas_diseno_m1.csv"))
write_csv(tabla_m1_vs_referencias, file.path(ruta_out_m1, "comparacion_m1_vs_referencias.csv"))
write_csv(subgrupos_m1_vs_ref,     file.path(ruta_out_m1, "subgrupos_m1_vs_referencias.csv"))
write_csv(tabla_control_filas,     file.path(ruta_out_m1, "control_integridad_filas_m1.csv"))


# ==============================================================================
# BLOQUE DE AUDITORÍA DEL PASO 5
# ==============================================================================
cat("\n==================== AUDITORIA PASO 5 (M1) ====================\n")

cat("1. DISEÑO TEMPORAL Y PRESERVACIÓN DE EPISODE_ID (Sección 3):\n")
print(resumen_ventanas_diseno %>%
        select(particion, anios_train, anio_valid, n_eventos_train, n_episodios_train,
               n_eventos_valid, n_episodios_valid, episodios_cruzados,
               prop_ceros_train, mediana_train_usd, media_train_usd))

cat("\n2. BENCHMARK GLOBAL EN DESARROLLO: M1 vs. REFERENCIAS (Ordenado por MAE medio):\n")
print(tabla_m1_vs_referencias %>%
        select(modelo, variante_respuesta, mae_V1, mae_V2, mae_V3,
               mae_medio_usd, rmse_medio_usd, dif_mae_vs_ref_global, dif_mae_vs_ref_tipo))

cat("\n3. BENCHMARK POR SUBGRUPOS DE MEDICIÓN (Sensibilidad 2: M1 vs. Referencias):\n")
print(subgrupos_m1_vs_ref, n = 20)

cat("\n4. VERIFICACIÓN DE FILAS COMUNES ENTRE MODELOS (Responsabilidad #18):\n")
print(tabla_control_filas)
cat("===============================================================\n")
