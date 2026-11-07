library(dplyr); library(tidyr)

base_desarrollo <- readRDS("datos_comunes/base_desarrollo.rds")
particiones     <- readRDS("datos_comunes/particiones_desarrollo.rds")

ref_ventana <- function(p) {
  tr <- base_desarrollo %>% filter(event_id %in% p$train_id)
  va <- base_desarrollo %>% filter(event_id %in% p$valid_id)
  med_global <- median(tr$dano_usd)
  por_tipo <- tr %>% group_by(event_type) %>%
    summarise(med = median(dano_usd), media = mean(dano_usd), .groups = "drop")
  
  va %>% left_join(por_tipo, by = "event_type") %>%
    transmute(event_id, episode_id,
              particion = p$particion, anio_validacion = p$anio_validacion,
              y_observado = dano_usd,
              referencia_global     = med_global,
              referencia_tipo       = coalesce(med, med_global)) %>%
    pivot_longer(starts_with("referencia"), names_to = "modelo", values_to = "y_predicho")
}

predicciones_ref <- bind_rows(lapply(particiones, ref_ventana)) %>%
  mutate(variante_respuesta = "original", configuracion = "ninguna")

metricas_ref <- predicciones_ref %>%
  group_by(modelo, anio_validacion) %>%
  summarise(n = n(), MAE = mean(abs(y_observado - y_predicho)),
            RMSE = sqrt(mean((y_observado - y_predicho)^2)), .groups = "drop")

# Comprobación pedida: log1p no cambia la mediana (INNECESARIO SE PUEDE BORRAR)
chequeo_log1p <- bind_rows(lapply(particiones, function(p) {
  y <- base_desarrollo$dano_usd[base_desarrollo$event_id %in% p$train_id]
  tibble(particion = p$particion, mediana = median(y),
         mediana_log1p_retransformada = expm1(median(log1p(y))))
}))

dir.create("resultados/referencia", recursive = TRUE, showWarnings = FALSE)
saveRDS(predicciones_ref, "resultados/referencia/predicciones_desarrollo_referencia.rds")
write.csv(metricas_ref,   "resultados/referencia/metricas_referencia.csv", row.names = FALSE)

# Función final para la reserva: reajusta con todo desarrollo y predice para eventos nuevos
referencia_final <- function(base_dev, nuevos) {
  med_global <- median(base_dev$dano_usd)
  por_tipo <- base_dev %>% group_by(event_type) %>%
    summarise(med = median(dano_usd), .groups = "drop")
  nuevos %>% select(event_id, episode_id, event_type) %>%
    left_join(por_tipo, by = "event_type") %>%
    transmute(event_id, episode_id,
              referencia_global     = med_global,
              referencia_tipo       = coalesce(med, med_global))
}
