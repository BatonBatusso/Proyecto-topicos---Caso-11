formula_m1 <-
~is_hail + mag_hail_in + mag_wind_kt + mag_type_grupo + mag_ausente + 
    dur_seg + mes_num + hora_inicio + state + coordenadas_validas + 
    dist_km - 1
aprender_preproc_m1 <-
function(df_train) {
  media_hail <- df_train %>%
    filter(event_type == "Hail") %>%
    summarise(m = mean(mag_hail_in, na.rm = TRUE)) %>%
    pull(m)
  
  media_wind <- df_train %>%
    filter(event_type == "Thunderstorm Wind") %>%
    summarise(m = mean(mag_wind_kt, na.rm = TRUE)) %>%
    pull(m)
  
  mediana_dur <- median(df_train$dur_seg, na.rm = TRUE)
  
  list(
    media_hail   = ifelse(is.na(media_hail), 0, media_hail),
    media_wind   = ifelse(is.na(media_wind), 0, media_wind),
    mediana_dur  = ifelse(is.na(mediana_dur), 0, mediana_dur),
    niveles_type = levels(df_train$mag_type_grupo),
    niveles_st   = levels(df_train$state)
  )
}
aplicar_preproc_m1 <-
function(df, params) {
  df %>%
    mutate(
      # Asegurar consistencia de niveles en factores entre train y valid
      mag_type_grupo = factor(mag_type_grupo, levels = params$niveles_type),
      state          = factor(state, levels = params$niveles_st),
      
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
      dur_seg = coalesce(dur_seg, params$mediana_dur),
      dist_km = coalesce(dist_km, 0)
    )
}
ajustar_predecir_m1_final <-
function(df_desarrollo,
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
