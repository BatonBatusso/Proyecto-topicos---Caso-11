library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
dir.create("salidas", showWarnings = FALSE)

storm <- read_csv(
  "eventos.csv",
  col_types = cols(
    .default = col_character(),
    
    # Enteros / IDs numéricos
    BEGIN_YEARMONTH = col_integer(), BEGIN_DAY = col_integer(), BEGIN_TIME = col_integer(),
    END_YEARMONTH   = col_integer(), END_DAY   = col_integer(), END_TIME   = col_integer(),
    EPISODE_ID = col_double(), EVENT_ID = col_double(),
    STATE_FIPS = col_integer(), YEAR = col_integer(), CZ_FIPS = col_integer(),
    INJURIES_DIRECT = col_integer(), INJURIES_INDIRECT = col_integer(),
    DEATHS_DIRECT   = col_integer(), DEATHS_INDIRECT   = col_integer(),
    TOR_OTHER_CZ_FIPS = col_integer(),
    
    # Numéricas continuas
    MAGNITUDE = col_double(), TOR_LENGTH = col_double(), TOR_WIDTH = col_double(),
    BEGIN_RANGE = col_double(), END_RANGE = col_double(),
    BEGIN_LAT = col_double(), BEGIN_LON = col_double(),
    END_LAT = col_double(), END_LON = col_double(),
    
    # Categóricas
    STATE = col_factor(), MONTH_NAME = col_factor(), EVENT_TYPE = col_factor(),
    CZ_TYPE = col_factor(), WFO = col_factor(), CZ_TIMEZONE = col_factor(),
    SOURCE = col_factor(), MAGNITUDE_TYPE = col_factor(), FLOOD_CAUSE = col_factor(),
    TOR_F_SCALE = col_factor(), BEGIN_AZIMUTH = col_factor(), END_AZIMUTH = col_factor()
  )
)

names(storm) <- tolower(names(storm))

log_muestra <- tibble()
registrar <- function(df, paso, nota = "") {
  log_muestra <<- bind_rows(log_muestra,
                            tibble(paso = paso, filas = nrow(df),
                                   episodios = n_distinct(df$episode_id), nota = nota))
  df
}

invisible(registrar(storm, "Paquete entregado"))



####### VERIFICAR CORTE #######
table(storm$state,useNA = "ifany")
table(storm$event_type, useNA = "ifany")
table(storm$year, useNA = "ifany")

storm <- storm %>%
  filter(event_type %in% c("Hail", "Thunderstorm Wind"),
         state %in% c("TEXAS", "OKLAHOMA", "KANSAS"),
         year >= 2018, year <= 2023) %>%
  registrar("Recorte de tipo, estado y años", "debería coincidir con el paquete")


###### ########
# --- Registro de construcción de la muestra ----------------------------------

####### ID DUPLICADO #######
sum(duplicated(storm$event_id))  #0 ID duplicados 

####### FILAS DUPLICDAS EXCEPTO ID ########
dup_cols <- setdiff(names(storm), c("event_id", "episode_id"))
duplicados <- storm %>%
  group_by(across(all_of(dup_cols))) %>%
  filter(n() > 1) %>%
  arrange(across(all_of(dup_cols))) %>%
  select(event_id, episode_id, event_type, state, cz_name, begin_date_time, magnitude, damage_property)

#INSPECCION ANTES DE ELIMINAR
dup_cols <- setdiff(names(storm), c("event_id", "episode_id"))

dups <- storm %>%
  group_by(across(all_of(dup_cols))) %>%
  mutate(grupo = cur_group_id(), n_grupo = n(), n_episodios = n_distinct(episode_id)) %>%
  ungroup() %>%
  filter(n_grupo > 1) %>%
  arrange(grupo, event_id)

dups %>% summarise(filas = n(), grupos = n_distinct(grupo),
  grupos_en_episodios_distintos = n_distinct(grupo[n_episodios > 1]),
  filas_con_coordenadas = sum(!is.na(begin_lat)),
  filas_con_dano = sum(!is.na(damage_property) & damage_property != "0.00K"))

dups %>% select(grupo, event_id, episode_id, year, event_type, cz_name,
  begin_date_time, begin_lat, begin_lon, magnitude, damage_property)

write_csv(dups, "salidas/duplicados_exactos.csv")

###### ELIMINAR DUPLICADOS #####
storm_limpio <- storm %>%
  arrange(event_id) %>% 
  distinct(across(all_of(dup_cols)), .keep_all = TRUE) %>%
  registrar("Eliminación de filas idénticas salvo ID", "se conserva el menor event_id")


#Función Para convertir a dolares
to_usd <- function(x) {
  x <- str_trim(toupper(x))
  x[x %in% c("", "NA")] <- NA
  num <- as.numeric(str_remove(x, "[KMB]$"))
  suf <- str_extract(x, "[KMB]$")
  num * case_when(suf == "K" ~ 1e3, suf == "M" ~ 1e6, suf == "B" ~ 1e9, TRUE ~ 1)
}

####### Variables de daño Monetario con distinción NA #######
storm_limpio <- storm_limpio %>%
  mutate(dano_usd = to_usd(damage_property),
         dano_informado = !is.na(dano_usd))

invisible(storm_limpio %>% filter(dano_informado) %>%
            registrar("Con respuesta informada", "ausentes excluidos, sin imputar"))

readr::write_csv(log_muestra, "salidas/tabla_construccion_muestra.csv")

##### Tabla de la muestra por período y tipo #####
tabla_muestra_periodo <- storm_limpio %>%
  mutate(periodo = if_else(year <= 2021, "desarrollo", "reserva"),
         respuesta = case_when(
           is.na(dano_usd)      ~ "ausente",
           periodo == "reserva" ~ "informada",   # en la reserva no se desglosa cero/positivo
           dano_usd == 0        ~ "cero",
           TRUE                 ~ "positivo")) %>%
  count(periodo, event_type, respuesta) %>%
  pivot_wider(names_from = respuesta, values_from = n, values_fill = 0) %>%
  mutate(total = rowSums(across(where(is.numeric))))
#En conjunto de reserva no se describen ceros y positivos para evitar mirar la respuesta
write_csv(tabla_muestra_periodo, "salidas/tabla_muestra_periodo.csv")

#Filas con falla en formato en variable respuesta 
storm_limpio %>%
  filter(
    !is.na(damage_property),
    damage_property != "",
    is.na(dano_usd)
  ) # Cero filas fuera de formato

# --- 4. Fechas y duración -----------------------------------------------------
parse_dt <- function(x) parse_date_time(x, orders = c("dmy HMS", "mdy HMS"), quiet = TRUE)
storm_limpio <- storm_limpio %>%
  mutate(inicio = parse_dt(begin_date_time),
         fin    = parse_dt(end_date_time),
         dur_min = as.numeric(difftime(fin, inicio, units = "mins")))
sum(is.na(storm_limpio$inicio)); sum(storm_limpio$dur_min < 0, na.rm = TRUE)   # auditar
sum(is.na(storm_limpio$fin))
summary(storm_limpio$dur_min)
#No hay fechas de inicio faltante
#No hay duraciones negativas

# --- 5. Magnitud: separar por tipo, no mezclar unidades -----------------------
storm_limpio <- storm_limpio %>%
  mutate(mag_hail_in  = if_else(event_type == "Hail", magnitude, NA_real_), #SEPARACION DE VARIABLES POR UNIDAD METRICA
         mag_wind_kt  = if_else(event_type == "Thunderstorm Wind", magnitude, NA_real_),
         mag_ausente  = is.na(magnitude),
         mag_medida   = magnitude_type %in% c("MG", "MS"))  # medida vs estimada

summary(storm_limpio$mag_hail_in); summary(storm_limpio$mag_wind_kt)  # buscar imposibles (hail > 6 in, viento > 150 kt)
storm_limpio <- storm_limpio %>%
  mutate(mag_medida = case_when(
    event_type == "Thunderstorm Wind" & magnitude_type %in% c("MG", "MS") ~ TRUE,
    event_type == "Thunderstorm Wind"                                       ~ FALSE,
    TRUE                                                                    ~ NA))  # granizo: no aplica 
table(storm_limpio$event_type, storm_limpio$mag_medida, useNA = "ifany")
table(storm_limpio$event_type, storm_limpio$magnitude_type, useNA = "ifany")

#Agregar indicadores geográficos y horarios

storm_limpio <- storm_limpio %>%
  mutate(mes_num     = month(inicio),
         hora_inicio = hour(inicio) + minute(inicio) / 60,
         mag_origen  = factor(case_when(event_type == "Hail" ~ "granizo",
                                        mag_medida           ~ "viento_medido",
                                        TRUE                 ~ "viento_estimado")),
         coord_ok    = !is.na(begin_lat) & !is.na(begin_lon) &
           begin_lat > 25 & begin_lat < 41 &
           begin_lon > -107 & begin_lon < -93)

table(storm_limpio$mag_origen, useNA = "ifany")
table(storm_limpio$coord_ok, useNA = "ifany")

# --- 6. Estructura de episodios ----------------------------------------------
ep <- storm_limpio %>% group_by(episode_id) %>%
  summarise(n_eventos = n(), anio_min = min(year), anio_max = max(year),
            n_estados = n_distinct(state), .groups = "drop")
summary(ep$n_eventos)
ep %>% filter(anio_min != anio_max) %>% nrow()   # episodios que cruzan años
ep %>% filter(anio_min <= 2021, anio_max >= 2022) %>% nrow()  # cruzan el límite dev/prueba


# --- 7. Calidad: ausentes y distribución de la respuesta ----------------------

ausentes <- storm_limpio %>%
  summarise(across(everything(),
                   ~ if (is.character(.x)) mean(is.na(.x) | .x == "") else mean(is.na(.x)))) %>%
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "prop_ausente") %>%
  arrange(desc(prop_ausente))

print(ausentes, n = 70)
readr::write_csv(ausentes, "salidas/tabla_ausentes.csv")

storm_limpio %>% summarise(
  n = n(), informado = mean(dano_informado),
  cero = mean(dano_usd == 0, na.rm = TRUE),
  q50 = median(dano_usd, na.rm = TRUE),
  q99 = quantile(dano_usd, .99, na.rm = TRUE),
  max = max(dano_usd, na.rm = TRUE))


# concentración: qué parte del daño total aportan los mayores eventos #SIN PARTICION (No implica ninguna decisión)
storm_limpio %>% filter(dano_informado) %>% arrange(desc(dano_usd)) %>%
  summarise(top10_share = sum(head(dano_usd, 10)) / sum(dano_usd),
            top1pct_share = sum(head(dano_usd, ceiling(.01 * n()))) / sum(dano_usd))
#El 58%.2 del daño económico. Para el 1% superior el porcentaje es 97.9%


# concentración (solo desarrollo)
dev <- storm_limpio %>% filter(dano_informado, year <= 2021)

conc <- function(d) {
  d <- d %>% arrange(desc(dano_usd))
  tibble(n = nrow(d),
         total_usd = sum(d$dano_usd),
         top10_share   = sum(head(d$dano_usd, 10)) / sum(d$dano_usd),
         top1pct_share = sum(head(d$dano_usd, ceiling(.01 * nrow(d)))) / sum(d$dano_usd),
         prop_cero = mean(d$dano_usd == 0),
         mediana = median(d$dano_usd))
}

bind_rows(todos = conc(dev),
          dev %>% group_split(event_type) %>%
            setNames(sort(unique(dev$event_type))) %>% lapply(conc) %>% bind_rows(.id = "grupo"),
          .id = NULL)
#Concentración de los daños económicos según categoría MAYOR ASIMETRIA
# EN DAÑOS POR GRANIZO

top10 <- dev %>% slice_max(dano_usd, n = 10) %>%
  select(event_id, episode_id, year, event_type, state, cz_name, magnitude, dano_usd, event_narrative)

readr::write_csv(top10, "salidas/top10.csv")

# 2) Cuánto del daño positivo está en valores repetidos dentro de un mismo episodio
dev %>% filter(dano_usd > 0) %>%
  group_by(episode_id, dano_usd) %>% mutate(n_rep = n()) %>% ungroup() %>%
  summarise(eventos_positivos = n(),
            prop_eventos_repetidos = mean(n_rep > 1),
            prop_dano_en_repetidos = sum(dano_usd[n_rep > 1]) / sum(dano_usd))

# 3) Concentración por episodio, con suma y con máximo
dev %>% group_by(episode_id) %>%
  summarise(n = n(), dano_suma = sum(dano_usd), dano_max = max(dano_usd)) %>%
  arrange(desc(dano_suma)) %>%
  mutate(share_suma = dano_suma / sum(dano_suma), acum = cumsum(share_suma)) %>%
  head(10)

# 4) Qué tan distintas son las ventanas de validación
dev %>% group_by(year) %>%
  summarise(n = n(), prop_cero = mean(dano_usd == 0),
            media = mean(dano_usd), maximo = max(dano_usd))

# 5) Ausentes de respuesta por tipo y año (para la tabla de construcción de la muestra)
storm_limpio %>% filter(year <= 2021) %>%
  count(year, event_type, dano_informado) %>%
  tidyr::pivot_wider(names_from = dano_informado, values_from = n, values_fill = 0)

######################################################



# c) ¿puede el daño mediano ser distinto de cero en algún grupo?
dev %>% filter(event_type == "Hail") %>%
  mutate(tam = cut(magnitude, c(0, 1, 1.75, 2.5, Inf), right = FALSE)) %>%
  group_by(tam) %>% summarise(n = n(), prop_pos = mean(dano_usd > 0), mediana = median(dano_usd))

dev %>% filter(event_type == "Thunderstorm Wind") %>%
  mutate(vel = cut(magnitude, c(0, 50, 58, 65, Inf), right = FALSE)) %>%
  group_by(vel) %>% summarise(n = n(), prop_pos = mean(dano_usd > 0), mediana = median(dano_usd))

menor50 <- dev %>% filter(event_type == "Thunderstorm Wind") %>% 
  filter(magnitude < 50)
sum(menor50$dano_informado)

# SESGO DE SELECCIÓN, CON MAGNITUD MENORES A 50 SOLO SE REGISTRARON EVENTOS QUE 
# SI PRODUJERON DAÑOS

#####

ep_dev <- dev %>% group_by(episode_id) %>%
  summarise(n = n(), n_pos = sum(dano_usd > 0), .groups = "drop")

ep_dev %>% summarise(episodios = n(),
                     todos_cero = mean(n_pos == 0),
                     mixtos     = mean(n_pos > 0 & n_pos < n),
                     todos_pos  = mean(n_pos == n))

summary(lm(log1p(dano_usd) ~ factor(episode_id), data = dev))$adj.r.squared



resumen_episodios <- storm_limpio %>% 
  group_by(episode_id) %>% 
  summarise(
    eventos = n(),
    sum_dano = sum(dano_usd),
    max_dano = max(dano_usd)
  )

resumen_episodios <- storm_limpio %>%
  group_by(episode_id) %>%
  summarise(
    anio = first(year),                       # permite filtrar desarrollo sin recalcular
    eventos = n(),
    eventos_dano_informado = sum(!is.na(dano_usd)),
    eventos_dano_positivo  = sum(dano_usd > 0, na.rm = TRUE),
    sum_dano = sum(dano_usd, na.rm = TRUE),
    max_dano = if (all(is.na(dano_usd))) NA_real_ else max(dano_usd, na.rm = TRUE),
    # eventos cuyo monto positivo aparece 2 o más veces en el episodio
    eventos_rep_pos = {
      x <- dano_usd[!is.na(dano_usd) & dano_usd > 0]
      sum(x %in% x[duplicated(x)])
    },
    montos_distintos_pos = n_distinct(dano_usd[!is.na(dano_usd) & dano_usd > 0]),
    .groups = "drop"
  ) %>%
  mutate(prop_rep_pos = if_else(eventos_dano_positivo > 0,
                                eventos_rep_pos / eventos_dano_positivo, NA_real_))

resumen_episodios %>% filter(anio <= 2021) %>%
  write_csv("salidas/resumen_episodios_dev.csv")

sum(resumen_episodios$prop_rep_pos == 1, na.rm = TRUE)


###### CUANTIFICAR CUANTO VARÍAN EVENTOS CON UN MISMO DANO_USD OTORGADO ######
dev <- storm_limpio %>% filter(dano_informado, year <= 2021)

dist_max_km <- function(lat, lon) {
  ok <- !is.na(lat) & !is.na(lon)
  lat <- lat[ok] * pi/180; lon <- lon[ok] * pi/180
  if (length(lat) < 2) return(NA_real_)
  a <- sin(outer(lat, lat, "-")/2)^2 +
    outer(cos(lat), cos(lat)) * sin(outer(lon, lon, "-")/2)^2
  max(2 * 6371 * asin(sqrt(pmin(1, a))))
}

metricas <- function(d) {
  d %>% summarise(
    n_ev = n(),
    n_tipos = n_distinct(event_type),
    n_condados = n_distinct(paste(state, cz_name)),
    rango_hail_in   = if (sum(event_type == "Hail") >= 2)
      diff(range(magnitude[event_type == "Hail"], na.rm = TRUE)) else NA_real_,
    rango_viento_kt = if (sum(event_type == "Thunderstorm Wind") >= 2)
      diff(range(magnitude[event_type == "Thunderstorm Wind"], na.rm = TRUE)) else NA_real_,
    span_min    = as.numeric(difftime(max(inicio), min(inicio), units = "mins")),
    dist_max_km = dist_max_km(begin_lat, begin_lon),
    .groups = "drop")
}

# Episodios con daño positivo, al menos 2 montos distintos y algún monto repetido
eps <- dev %>% filter(dano_usd > 0) %>% group_by(episode_id) %>%
  filter(n_distinct(dano_usd) >= 2, any(duplicated(dano_usd))) %>%
  pull(episode_id) %>% unique()

# A) Grupos: mismo episodio y mismo monto (2 o más eventos)
a <- dev %>% filter(dano_usd > 0, episode_id %in% eps) %>%
  group_by(episode_id, dano_usd) %>% filter(n() >= 2) %>% metricas() %>%
  mutate(nivel = "A. mismo monto dentro del episodio")

sum(a$n_ev)

# B) Referencia: el episodio completo (eventos con daño positivo)
b <- dev %>% filter(dano_usd > 0, episode_id %in% eps) %>%
  group_by(episode_id) %>% metricas() %>%
  mutate(nivel = "B. episodio completo (eventos con daño)")

resumen <- bind_rows(a, b) %>% group_by(nivel) %>%
  summarise(unidades = n(),
            eventos_mediana = median(n_ev),
            pct_ambos_tipos = round(100 * mean(n_tipos > 1), 1),
            pct_mas_de_un_condado = round(100 * mean(n_condados > 1), 1),
            hail_rango_med_in = median(rango_hail_in, na.rm = TRUE),
            hail_rango_p75_in = quantile(rango_hail_in, .75, na.rm = TRUE),
            viento_rango_med_kt = median(rango_viento_kt, na.rm = TRUE),
            viento_rango_p75_kt = quantile(rango_viento_kt, .75, na.rm = TRUE),
            span_med_min = median(span_min, na.rm = TRUE),
            dist_med_km = median(dist_max_km, na.rm = TRUE),
            dist_p75_km = quantile(dist_max_km, .75, na.rm = TRUE))


dir.create("salidas", showWarnings = FALSE)
write_csv(bind_rows(a, b), "salidas/heterogeneidad_mismo_dano.csv")

mixtos <- dev %>% group_by(episode_id) %>%
  filter(any(dano_usd > 0), any(dano_usd == 0)) %>% ungroup() %>%
  mutate(con_dano = dano_usd > 0)

mixtos %>% group_by(event_type, con_dano) %>%
  summarise(n = n(), episodios = n_distinct(episode_id),
            mag_p25 = quantile(magnitude, .25, na.rm = TRUE),
            mag_mediana = median(magnitude, na.rm = TRUE),
            mag_p75 = quantile(magnitude, .75, na.rm = TRUE),
            dur_mediana_min = median(dur_min, na.rm = TRUE), .groups = "drop")

# Comparación dentro del mismo episodio (controla el episodio)
mixtos %>% group_by(episode_id, event_type, con_dano) %>%
  summarise(mag = median(magnitude, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = con_dano, values_from = mag, names_prefix = "dano_") %>%
  filter(!is.na(dano_TRUE), !is.na(dano_FALSE)) %>%
  group_by(event_type) %>%
  summarise(episodios = n(),
            dif_mediana = median(dano_TRUE - dano_FALSE),
            pct_con_mayor_mag = round(100 * mean(dano_TRUE > dano_FALSE), 1))


dir.create("datos_prueba_sellado", showWarnings = FALSE)
saveRDS(storm_limpio, "datos_prueba_sellado/storm_limpio.rds")