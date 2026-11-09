library(dplyr)
storm_limpio <- readRDS("datos_prueba_sellado/storm_limpio.rds")
dir.create("datos_comunes", showWarnings = FALSE)
dir.create("datos_prueba_sellado", showWarnings = FALSE)

cols_predictores <- c("event_id", "episode_id", "year", "event_type",
                      "mag_origen", "magnitude",
                      "dur_min", "mes_num", "hora_inicio",
                      "state", "wfo",
                      "begin_lat", "begin_lon")

base_desarrollo <- storm_limpio %>%
  filter(year <= 2021, dano_informado) %>%
  select(all_of(cols_predictores), dano_usd)

base_reserva              <- storm_limpio %>% filter(year >= 2022)
base_prueba_sin_respuesta <- base_reserva %>% select(all_of(cols_predictores))
respuesta_prueba          <- base_reserva %>% select(event_id, dano_informado, dano_usd)
nrow(base_desarrollo);nrow(base_reserva)
#2.629 en reserva sin respuesta informada


particiones <- lapply(2019:2021, function(a) list(
  particion = a - 2018, anio_validacion = a,
  train_id  = base_desarrollo$event_id[base_desarrollo$year <  a],
  valid_id  = base_desarrollo$event_id[base_desarrollo$year == a]))

configuracion_comun <- list(
  semilla = 3407,
  umbral_extremo_usd = unname(quantile(base_desarrollo$dano_usd, 0.99)),
  presupuesto_max_config = 20)

stopifnot(!anyDuplicated(base_desarrollo$event_id),
          all(tapply(base_desarrollo$year, base_desarrollo$episode_id, n_distinct) == 1),
          length(intersect(base_desarrollo$episode_id, base_prueba_sin_respuesta$episode_id)) == 0,
          !"dano_usd" %in% names(base_prueba_sin_respuesta))

colSums(is.na(base_desarrollo))

base_desarrollo %>% group_by(source) %>%
  summarise(n = n(), prop_dano = mean(dano_usd > 0), .groups = "drop") %>%
  arrange(desc(n)) %>% print(n = 50)

base_desarrollo %>% group_by(wfo) %>%
  summarise(n = n(), prop_dano = mean(dano_usd > 0), .groups = "drop") %>%
  arrange(desc(n)) %>% print(n = 50)

# niveles que aparecen en validación y no existían en el entrenamiento
for (a in 2019:2021) {
  tr <- filter(base_desarrollo, year <  a); va <- filter(base_desarrollo, year == a)
  cat(a, "| eventos con source nuevo:", sum(!va$source %in% tr$source),
      "| con wfo nuevo:", sum(!va$wfo %in% tr$wfo), "\n")
}

saveRDS(base_desarrollo,           "datos_comunes/base_desarrollo.rds")
saveRDS(base_prueba_sin_respuesta, "datos_comunes/base_prueba_sin_respuesta.rds")
saveRDS(particiones,               "datos_comunes/particiones_desarrollo.rds")
saveRDS(configuracion_comun,       "datos_comunes/configuracion_comun.rds")
saveRDS(respuesta_prueba,          "datos_prueba_sellado/respuesta_prueba.rds")  # no la lee ningún modelo
