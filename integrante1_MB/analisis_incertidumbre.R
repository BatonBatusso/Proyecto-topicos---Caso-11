library(dplyr); library(tidyr)
predicciones_ref    <- readRDS("resultados/referencia/predicciones_desarrollo_referencia.rds")
base_desarrollo     <- readRDS("datos_comunes/base_desarrollo.rds")
configuracion_comun <- readRDS("datos_comunes/configuracion_comun.rds")

# Diferencia de MAE (modelo - referencia) con IC por bootstrap de episodios, dentro de cada ventana.
# dif_MAE > 0 significa que el modelo tiene MÁS error que la referencia.
bootstrap_episodios <- function(pred, modelo_comparado, modelo_ref = "referencia_global", #"referencia_tipo"
                                B = 2000, seed = 3407) {
  set.seed(seed)
  d <- pred %>%
    filter(modelo %in% c(modelo_comparado, modelo_ref)) %>%
    mutate(ae = abs(y_observado - y_predicho)) %>%
    select(event_id, episode_id, anio_validacion, modelo, ae) %>%
    pivot_wider(names_from = modelo, values_from = ae)
  stopifnot(!anyNA(d[[modelo_comparado]]), !anyNA(d[[modelo_ref]]))   # mismas filas
  d$dif <- d[[modelo_comparado]] - d[[modelo_ref]]
  
  ep <- d %>% group_by(anio_validacion, episode_id) %>%
    summarise(s = sum(dif), n = n(), .groups = "drop")
  ventanas <- split(ep, ep$anio_validacion)
  
  obs <- c(sapply(ventanas, function(w) sum(w$s) / sum(w$n)),
           agrupado = sum(ep$s) / sum(ep$n))
  sim <- replicate(B, {
    rem <- lapply(ventanas, function(w) w[sample.int(nrow(w), replace = TRUE), ])
    c(sapply(rem, function(w) sum(w$s) / sum(w$n)),
      agrupado = sum(sapply(rem, function(w) sum(w$s))) / sum(sapply(rem, function(w) sum(w$n))))
  })
  tibble(modelo = modelo_comparado, referencia = modelo_ref, ventana = names(obs),
         dif_MAE = unname(obs),
         ic95_inf = apply(sim, 1, quantile, 0.025),
         ic95_sup = apply(sim, 1, quantile, 0.975))
}

# Cuánto del error absoluto total concentran los eventos más destructivos (por daño observado)
concentracion_error <- function(pred, modelo_x, p = c(0.01, 0.05)) {
  d <- pred %>% filter(modelo == modelo_x) %>%
    mutate(ae = abs(y_observado - y_predicho)) %>% arrange(desc(y_observado))
  tibble(modelo = modelo_x, top = p,
         share_error = sapply(p, function(q) sum(head(d$ae, ceiling(q * nrow(d)))) / sum(d$ae)))
}

# Error por clase de daño (cero, positivo moderado, extremo) y por subgrupos
diagnostico_grupos <- function(pred, atributos, umbral_extremo) {
  d <- pred %>% left_join(atributos, by = "event_id") %>%
    mutate(clase_dano = case_when(y_observado == 0 ~ "cero",
                                  y_observado >= umbral_extremo ~ "extremo",
                                  TRUE ~ "positivo moderado"),
           ae = abs(y_observado - y_predicho),
           se = (y_observado - y_predicho)^2)
  por <- function(var) d %>%
    group_by(modelo, variable = var, nivel = as.character(.data[[var]])) %>%
    summarise(n = n(), MAE = mean(ae), RMSE = sqrt(mean(se)), .groups = "drop")
  bind_rows(lapply(c("clase_dano", "state", "event_type", "mag_origen"), por))
}

##### PRUEBA DE FUNCIONES (Modelo simple) #####

set.seed(3407)
falso <- predicciones_ref %>% filter(modelo == "referencia_tipo") %>%
  mutate(modelo = "modelo_simulado", y_predicho = y_predicho + runif(n(), 0, 5000))
prueba <- bind_rows(predicciones_ref, falso)

bootstrap_episodios(prueba, "modelo_simulado", B = 500)
concentracion_error(prueba, "modelo_simulado")
atributos <- base_desarrollo %>% select(event_id, state, event_type, mag_origen)
diagnostico_grupos(prueba, atributos, configuracion_comun$umbral_extremo_usd)
