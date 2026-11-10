library(readr); library(dplyr); library(tidyr)
dir.create("salidas", showWarnings = FALSE)

storm_limpio    <- readRDS("datos_prueba_sellado/storm_limpio.rds")
base_desarrollo <- readRDS("datos_comunes/base_desarrollo.rds")
particiones     <- readRDS("datos_comunes/particiones_desarrollo.rds")
dev <- storm_limpio %>% filter(dano_informado, year <= 2021)   # respuesta: SOLO desarrollo

# Resumen de la respuesta y concentración (total y por tipo)
conc <- function(d) {
  d <- arrange(d, desc(dano_usd))
  tibble(n = nrow(d), total_usd = sum(d$dano_usd),
         top10_share   = sum(head(d$dano_usd, 10)) / sum(d$dano_usd),
         top1pct_share = sum(head(d$dano_usd, ceiling(.01 * nrow(d)))) / sum(d$dano_usd),
         prop_cero = mean(d$dano_usd == 0), mediana = median(d$dano_usd),
         media = mean(d$dano_usd), p99 = unname(quantile(d$dano_usd, .99)))
}
resumen_respuesta <- bind_rows(
  mutate(conc(dev), grupo = "Todos"),
  dev %>% group_by(grupo = as.character(event_type)) %>% group_modify(~ conc(.x)))
write_csv(resumen_respuesta, "salidas/resumen_respuesta_dev.csv")

write_csv(dev %>% group_by(year) %>%
            summarise(n = n(), prop_cero = mean(dano_usd == 0), media = mean(dano_usd),
                      maximo = max(dano_usd), .groups = "drop"),
          "salidas/respuesta_por_anio_dev.csv")

# Episodios
ep_dev <- dev %>% group_by(episode_id) %>%
  summarise(n = n(), n_pos = sum(dano_usd > 0), dano_suma = sum(dano_usd), .groups = "drop")

write_csv(ep_dev %>% summarise(episodios = n(), todos_cero = mean(n_pos == 0),
                               mixtos = mean(n_pos > 0 & n_pos < n), todos_pos = mean(n_pos == n)),
          "salidas/clasificacion_episodios_dev.csv")

conc_ep <- ep_dev %>% arrange(desc(dano_suma)) %>%
  mutate(share = dano_suma / sum(dano_suma), acum = cumsum(share))
write_csv(head(conc_ep, 10), "salidas/concentracion_episodios_dev.csv")
write_csv(tibble(episodios_para_90pct = which(conc_ep$acum >= 0.9)[1],
                 episodios_para_99pct = which(conc_ep$acum >= 0.99)[1],
                 episodios_totales = nrow(conc_ep)),
          "salidas/episodios_para_concentracion_dev.csv")

write_csv(tibble(metrica = "R2 ajustado de log1p(dano_usd) ~ factor(episode_id), desarrollo",
                 valor = summary(lm(log1p(dano_usd) ~ factor(episode_id), data = dev))$adj.r.squared,
                 eventos = nrow(dev), episodios = n_distinct(dev$episode_id)),
          "salidas/r2_episodio.csv")

# Estructura de episodios (solo conteos, sin respuesta)
write_csv(storm_limpio %>% group_by(episode_id) %>%
            summarise(eventos = n(), anio_min = min(year), anio_max = max(year), .groups = "drop") %>%
            summarise(episodios = n(), mediana_eventos = median(eventos), media_eventos = mean(eventos),
                      max_eventos = max(eventos), cruzan_anios = sum(anio_min != anio_max),
                      cruzan_limite_dev_reserva = sum(anio_min <= 2021 & anio_max >= 2022)),
          "salidas/estructura_episodios.csv")

# Repetición de montos dentro del episodio
write_csv(dev %>% filter(dano_usd > 0) %>% group_by(episode_id, dano_usd) %>%
            mutate(n_rep = n()) %>% ungroup() %>%
            summarise(eventos_positivos = n(), prop_eventos_repetidos = mean(n_rep > 1),
                      prop_dano_en_repetidos = sum(dano_usd[n_rep > 1]) / sum(dano_usd)),
          "salidas/repeticion_montos_dev.csv")

# Episodios mixtos: ¿se distingue por magnitud quién recibe el daño?
mixtos <- dev %>% group_by(episode_id) %>%
  filter(any(dano_usd > 0), any(dano_usd == 0)) %>% ungroup() %>%
  mutate(con_dano = dano_usd > 0)
write_csv(mixtos %>% group_by(episode_id, event_type, con_dano) %>%
            summarise(mag = median(magnitude), .groups = "drop") %>%
            pivot_wider(names_from = con_dano, values_from = mag, names_prefix = "dano_") %>%
            filter(!is.na(dano_TRUE), !is.na(dano_FALSE)) %>% group_by(event_type) %>%
            summarise(episodios = n(), dif_mediana = median(dano_TRUE - dano_FALSE),
                      pct_con_mayor_mag = round(100 * mean(dano_TRUE > dano_FALSE), 1), .groups = "drop"),
          "salidas/mixtos_diferencia_magnitud.csv")

# Auditoría de categorías y rango de magnitudes
write_csv(dev %>% group_by(source) %>% summarise(n = n(), prop_dano = mean(dano_usd > 0), .groups = "drop") %>%
            arrange(desc(n)), "salidas/auditoria_source_dev.csv")
write_csv(dev %>% group_by(wfo) %>% summarise(n = n(), prop_dano = mean(dano_usd > 0), .groups = "drop") %>%
            arrange(desc(n)), "salidas/auditoria_wfo_dev.csv")
write_csv(storm_limpio %>% group_by(event_type) %>%
            summarise(n = n(), minimo = min(magnitude), mediana = median(magnitude),
                      maximo = max(magnitude), .groups = "drop"),
          "salidas/resumen_magnitud.csv")

# Particiones y valores de la referencia
write_csv(bind_rows(lapply(particiones, function(p) {
  tr <- filter(base_desarrollo, event_id %in% p$train_id)
  va <- filter(base_desarrollo, event_id %in% p$valid_id)
  tibble(ventana = p$particion,
         entrenamiento = paste0(min(tr$year), if (max(tr$year) > min(tr$year)) paste0("–", max(tr$year)) else ""),
         validacion = p$anio_validacion,
         eventos_entrenamiento = nrow(tr), episodios_entrenamiento = n_distinct(tr$episode_id),
         eventos_validacion = nrow(va), episodios_validacion = n_distinct(va$episode_id))
})), "salidas/tabla_particiones.csv")

write_csv(bind_rows(lapply(particiones, function(p) {
  tr <- filter(base_desarrollo, event_id %in% p$train_id)
  tr %>% group_by(event_type = as.character(event_type)) %>%
    summarise(mediana = median(dano_usd), media = mean(dano_usd), .groups = "drop") %>%
    bind_rows(tibble(event_type = "Global", mediana = median(tr$dano_usd), media = mean(tr$dano_usd))) %>%
    mutate(ventana = p$particion, .before = 1)
})), "salidas/valores_referencia.csv")
