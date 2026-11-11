library(glmnet)
library(dplyr)
library(tidyr)

formula_m1 <- as.formula(
  "~ mag_origen + event_type:magnitud_z + dur_seg +
     mes_num + hora_inicio + state + begin_lat + begin_lon"
)




aprender_preproc_m1 <- function(df_train) {
  est_mag <- df_train %>%
    group_by(event_type = as.character(event_type)) %>%
    summarise(mag_mu = mean(magnitude, na.rm = TRUE),
              mag_sd = sd(magnitude,   na.rm = TRUE), .groups = "drop") %>%
    mutate(mag_sd = ifelse(is.na(mag_sd) | mag_sd == 0, 1, mag_sd))
  
  list(
    est_mag        = est_mag,
    mediana_dur    = median(df_train$dur_seg,   na.rm = TRUE),
    dur_max        = max(df_train$dur_seg,      na.rm = TRUE),
    mediana_lat    = median(df_train$begin_lat, na.rm = TRUE),
    mediana_lon    = median(df_train$begin_lon, na.rm = TRUE),
    niveles_tipo   = levels(df_train$event_type),
    niveles_origen = levels(df_train$mag_origen),
    niveles_st     = levels(df_train$state)
  )
}

# 4. Función auxiliar para aplicar el preprocesamiento a train o valid
aplicar_preproc_m1 <- function(df, params) {
  df %>%
    mutate(tipo_chr = as.character(event_type)) %>%
    left_join(params$est_mag, by = c("tipo_chr" = "event_type")) %>%
    mutate(
      magnitud_z = coalesce((magnitude - mag_mu) / mag_sd, 0),
      event_type = factor(tipo_chr, levels = params$niveles_tipo),
      mag_origen = factor(mag_origen, levels = params$niveles_origen),
      state      = factor(state,      levels = params$niveles_st),
      dur_seg    = pmin(coalesce(dur_seg, params$mediana_dur), params$dur_max),
      begin_lat  = coalesce(begin_lat, params$mediana_lat),
      begin_lon  = coalesce(begin_lon, params$mediana_lon)
    ) %>%
    select(-tipo_chr, -mag_mu, -mag_sd)
}


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
  
  
  
  # c. Aprender parámetros de imputación física en desarrollo y aplicar a ambos
  params_dev   <- aprender_preproc_m1(df_desarrollo)
  dev_clean    <- aplicar_preproc_m1(df_desarrollo, params_dev)
  nuevos_clean <- aplicar_preproc_m1(df_nuevos, params_dev)
  
  # d. Construir matrices de diseño (formula_m1 no requiere variable respuesta)
  x_dev    <- model.matrix(formula_m1, data = dev_clean)[, -1]
  x_nuevos <- model.matrix(formula_m1, data = nuevos_clean)[, -1]
  
  
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

