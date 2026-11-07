library(readr)
library(dplyr)
library(ps)
library(tidyverse)

eventos <- read_csv(
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

names(eventos) <- tolower(names(eventos))


####### VERIFICAR CORTE #######
table(eventos$state,useNA = "ifany")
table(eventos$event_type, useNA = "ifany")
table(eventos$year, useNA = "ifany")

# --- Registro de construcción de la muestra ----------------------------------
log_muestra <- tibble()
registrar <- function(df, paso, nota = "") {
  log_muestra <<- bind_rows(log_muestra,
                            tibble(paso = paso, filas = nrow(df),
                                   episodios = n_distinct(df$episode_id), nota = nota))
  df
}

eventos <- eventos %>%
  filter(event_type %in% c("Hail", "Thunderstorm Wind"),
         state %in% c("TEXAS", "OKLAHOMA", "KANSAS"),
         year >= 2018, year <= 2023) %>%
  registrar("Recorte de tipo, estado y años", "debería coincidir con el paquete")


####### ID DUPLICADO #######
sum(duplicated(eventos$event_id))  #0 ID duplicados 

####### FILAS DUPLICDAS EXCEPTO ID ########
dup_cols <- setdiff(names(eventos), c("event_id", "episode_id"))
duplicados <- eventos %>%
  group_by(across(all_of(dup_cols))) %>%
  filter(n() > 1) %>%
  arrange(across(all_of(dup_cols))) %>%
  select(event_id, episode_id, event_type, state, cz_name, begin_date_time, magnitude, damage_property)

#Función Para convertir a dolares
to_usd <- function(x) {
  x <- str_trim(toupper(x))
  x[x %in% c("", "NA")] <- NA
  num <- as.numeric(str_remove(x, "[KMB]$"))
  suf <- str_extract(x, "[KMB]$")
  num * case_when(suf == "K" ~ 1e3, suf == "M" ~ 1e6, suf == "B" ~ 1e9, TRUE ~ 1)
}

####### Variables de daño Monetario con distinción NA #######
eventos <- eventos %>%
  mutate(dano_usd = to_usd(damage_property),
         dano_informado = !is.na(dano_usd))

#Filas con falla en formato en variable respuesta 
eventos %>%
  filter(
    !is.na(damage_property),
    damage_property != "",
    is.na(dano_usd)
  ) # Cero filas fuera de formato

# --- 4. Fechas y duración -----------------------------------------------------
parse_dt <- function(x) parse_date_time(x, orders = c("dmy HMS", "mdy HMS"), quiet = TRUE)
eventos <- eventos %>%
  mutate(inicio = parse_dt(begin_date_time),
         fin    = parse_dt(end_date_time),
         dur_min = as.numeric(difftime(fin, inicio, units = "mins")),
         dur_seg = as.numeric(difftime(fin, inicio, units = "secs")),
         hora_inicio = hour(inicio),
         mes_num = month(inicio))
sum(is.na(eventos$inicio)); sum(eventos$dur_min < 0, na.rm = TRUE)   # auditar
sum(is.na(eventos$fin))
summary(eventos$dur_min)
summary(eventos$dur_seg)
#No hay fechas de inicio faltante
#No hay duraciones negativas

# --- 5. Magnitud: separar por tipo, no mezclar unidades -----------------------
eventos <- eventos %>%
  mutate(mag_hail_in  = if_else(event_type == "Hail", magnitude, NA_real_), #SEPARACION DE VARIABLES POR UNIDAD METRICA
         mag_wind_kt  = if_else(event_type == "Thunderstorm Wind", magnitude, NA_real_),
         mag_ausente  = is.na(magnitude),
         mag_medida   = magnitude_type %in% c("MG", "MS"))  # medida vs estimada

summary(eventos$mag_hail_in); summary(eventos$mag_wind_kt)  # buscar imposibles (hail > 6 in, viento > 150 kt)
eventos <- eventos %>%
  mutate(mag_medida = case_when(
    event_type == "Thunderstorm Wind" & magnitude_type %in% c("MG", "MS") ~ TRUE,
    event_type == "Thunderstorm Wind"                                       ~ FALSE,
    TRUE                                                                    ~ NA))  # granizo: no aplica 
table(eventos$event_type, eventos$mag_medida, useNA = "ifany")
table(eventos$event_type, eventos$magnitude_type, useNA = "ifany")


# --- Variables derivadas faltantes y origen de magnitud ---
eventos <- eventos %>%
  mutate(
    # Indicador lógico para granizo
    is_hail = if_else(event_type == "Hail", TRUE, FALSE),
    
    # Verificación de coordenadas (excluyendo NAs)
    coordenadas_validas = !is.na(begin_lat) & !is.na(begin_lon),
    
    # Agrupación de tipo de magnitud basándonos en los códigos de medición del viento vs estimación
    mag_type_grupo = factor(case_when(
      magnitude_type %in% c("MG", "MS") ~ "Medido",
      magnitude_type %in% c("EG", "ES") ~ "Estimado",
      TRUE ~ "No_Aplica"
    )),
    
    # Variable de origen adaptada al objeto "eventos"
    mag_origen = factor(case_when(
      event_type == "Hail" ~ "granizo",
      mag_medida           ~ "viento_medido",
      TRUE                 ~ "viento_estimado"
    ))
  )

# --- 5.1. Variables Espaciales ------------------------------------------------
eventos <- eventos %>%
  mutate(
    dist_km = sqrt((end_lon - begin_lon)^2 + (end_lat - begin_lat)^2) * 111 # Aprox km
  )


eventos_sin_na <- eventos %>% filter(!is.na(damage_property))

conjunto_prueba <- eventos_sin_na %>% filter(year >= 2022)
conjunto_entrenamiento <- eventos_sin_na %>% filter(year < 2022)

base_prueba <- conjunto_prueba %>%
  select(-c(injuries_direct, injuries_indirect,
            deaths_direct, deaths_indirect, flood_cause, category,
            tor_f_scale, tor_length, tor_width, tor_other_wfo,
            tor_other_cz_state, tor_other_cz_fips, tor_other_cz_name,
            episode_narrative, event_narrative, damage_crops))


base_entrenamiento <- conjunto_entrenamiento %>%
  select(-c( injuries_direct, injuries_indirect,deaths_direct, 
             deaths_indirect, flood_cause, category,
             tor_f_scale, tor_length, tor_width, tor_other_wfo,
             tor_other_cz_state, tor_other_cz_fips, tor_other_cz_name,
             episode_narrative, event_narrative, damage_crops))

base_entrenamiento_sin_respuesta <- base_entrenamiento %>% 
  select(-damage_property)




################################################################################
# SEMILLA Y PARTICIONES COMUNES
################################################################################

# --- Semilla global ------------------------------------------------------------
SEMILLA <- 3407
set.seed(SEMILLA)

# --- Ventanas de validación (expanding window) ---------------------------------
# Cada ventana entrena con los años pasados y valida en el año siguiente.
def_ventanas <- tibble::tribble(
  ~nombre,                          ~anios_train,       ~anio_valid,
  "V1: Entrenamiento 2018 -> Validacion 2019",      2018:2018, 2019L,
  "V2: Entrenamiento 2018-2019 -> Validacion 2020", 2018:2019, 2020L,
  "V3: Entrenamiento 2018-2020 -> Validacion 2021", 2018:2020, 2021L
)

ventanas <- lapply(seq_len(nrow(def_ventanas)), function(i) {
  # 1. Separar los conjuntos de la ventana
  train_raw <- base_entrenamiento %>% filter(year %in% def_ventanas$anios_train[[i]])
  valid_raw <- base_entrenamiento %>% filter(year == def_ventanas$anio_valid[i])
  
  # 2. Aprender qué categorías de 'source' superan el 2% SOLO en entrenamiento
  frec_source <- train_raw %>%
    count(source) %>%
    mutate(porcentaje = (n / sum(n)) * 100)
  
  source_retener <- frec_source %>%
    filter(porcentaje > 2) %>%
    pull(source)
  
  # 3. Aplicar la transformación tanto a train como a valid
  train_final <- train_raw %>%
    mutate(source = if_else(source %in% source_retener, as.character(source), "Otros"),
           source = factor(source))
  
  valid_final <- valid_raw %>%
    mutate(source = if_else(source %in% source_retener, as.character(source), "Otros"),
           source = factor(source, levels = levels(train_final$source)))
  
  # 4. Retornar la lista
  list(
    nombre = def_ventanas$nombre[i],
    train  = train_final,
    valid  = valid_final
  )
})
names(ventanas) <- c("V1", "V2", "V3")

# --- Resumen impreso al hacer source() -----------------------------------------
cat("\n==================== CONFIGURACION COMUN ====================\n")
cat("Semilla utilizada:", SEMILLA, "\n\n")
cat("Conjuntos de entrenamiento / validacion:\n")
for (w in ventanas) {
  cat(sprintf("  %-50s  train = %5d filas | valid = %5d filas\n",
              w$nombre, nrow(w$train), nrow(w$valid)))
}
cat(sprintf("\nConjunto de entrenamiento completo (2018-2021): %d filas\n",
            nrow(base_entrenamiento)))
cat(sprintf("Conjunto de prueba (2022-2023) reservado, NO TOCAR: %d filas\n",
            nrow(base_prueba)))
cat("==============================================================\n\n")


