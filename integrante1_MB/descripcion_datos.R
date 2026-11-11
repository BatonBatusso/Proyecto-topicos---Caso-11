# R/02_descripcion_datos.R
# ENTRADAS: datos_prueba_sellado/storm_limpio.rds, salidas/tabla_construccion_muestra.csv
# SALIDAS:  salidas/tabla_variables.csv, salidas/tabla_variables_informe.csv, resultados/figuras/*.png
library(readr); library(dplyr); library(tidyr); library(ggplot2); library(scales)

storm_limpio <- readRDS("datos_prueba_sellado/storm_limpio.rds")
log_muestra  <- read_csv("salidas/tabla_construccion_muestra.csv", show_col_types = FALSE)
dir.create("salidas", showWarnings = FALSE)
dir.create("resultados/figuras", recursive = TRUE, showWarnings = FALSE)

AL_EVENTO <- "Conocida al reconstruir el evento"
POSTERIOR <- "Posterior al evento"

d <- function(variable, interpretacion, rol, disponibilidad, decision, justificacion){
  tibble(variable, interpretacion, rol, disponibilidad, decision, justificacion)}

diccionario <- bind_rows(
  d("event_id", "ID del evento (clave primaria del NWS)", "Identificador", "No aplica",
    "Clave (no es predictor)", "Identifica la fila y permite auditar duplicados; no describe el fenómeno."),
  d("episode_id", "ID del episodio (agrupa los eventos de una misma tormenta)", "Identificador", "No aplica",
    "Clave (no es predictor)", "Mantiene unidos los eventos de un episodio en particiones y remuestreo."),
  
  d("damage_property", "Daño a la propiedad como texto (10.00K, 2.5M)", "Respuesta", POSTERIOR,
    "Respuesta (original)", "Se convierte a dólares en dano_usd."),
  d("dano_usd", "Daño a la propiedad en dólares nominales", "Respuesta", POSTERIOR,
    "Respuesta", "Variable objetivo; los vacíos no se imputan ni se recodifican como cero."),
  d("dano_informado", "Indica si el daño viene informado", "Respuesta", POSTERIOR,
    "Auditoría", "Define la población con respuesta observada."),
  
  d("event_type", "Tipo de evento (granizo o viento de tormenta)", "Predictor", AL_EVENTO,
    "Incluida", "Determina la unidad de la magnitud y define la referencia por tipo."),
  d("magnitude", "Tamaño del granizo (pulgadas) o velocidad del viento (nudos)", "Predictor", AL_EVENTO,
    "Incluida", "Sin NA; las unidades difieren por tipo, por lo que se estandariza dentro de cada tipo con estadísticos de entrenamiento."),
  d("mag_origen", "Granizo, viento medido o viento estimado", "Predictor", AL_EVENTO,
    "Incluida", "Reúne tipo y calidad de la medición sin valores faltantes."),
  d("dur_min", "Duración del evento (minutos)", "Predictor", AL_EVENTO,
    "Incluida", "Se calcula como fin menos inicio."),
  d("mes_num", "Mes de inicio (1 a 12)", "Predictor", AL_EVENTO,
    "Incluida", "Captura la estacionalidad del fenómeno."),
  d("hora_inicio", "Hora de inicio en horas decimales (0 a 24)", "Predictor", AL_EVENTO,
    "Incluida", "Continua; reemplaza a begin_time, que viene como hhmm y no es lineal."),
  d("state", "Estado (Texas, Oklahoma o Kansas)", "Predictor", AL_EVENTO,
    "Incluida", "Ubicación gruesa, tres niveles."),
  d(c("begin_lat", "begin_lon"), "Latitud y longitud del inicio del evento (grados)", "Predictor", AL_EVENTO,
    "Incluida", "Ubicación fina."),
  d("coord_ok", "Indicador de coordenadas dentro del rango plausible", "Auxiliar de construcción", AL_EVENTO,
    "Excluida", "Control de calidad; si es constante (todas TRUE) no aporta y se descarta."),
  d("cz_type", "Tipo de zona (condado, zona o marino)", "Predictor", AL_EVENTO,
    "Excluida", "Probablemente casi constante en el recorte; se descarta si tiene un solo nivel."),
  d("wfo", "Oficina del NWS responsable del área", "Predictor", AL_EVENTO,
    "Excluida", "Redundancia de localización."),
  d("source", "Fuente que reportó el evento", "Predictor", AL_EVENTO,
    "Excluida", "Refleja cómo se reportó y no el fenómeno físico; puede asociarse a que el daño esté informado. Evaluar sensibilidad sin esta variable."),
  
  d("magnitude_type", "Código de medición del viento (EG, ES, MG, MS)", "Auxiliar de construcción", AL_EVENTO,
    "Excluida", "Vacía en todo el granizo; su información queda en mag_origen."),
  d(c("mag_hail_in", "mag_wind_kt", "mag_medida", "mag_ausente"), "Magnitud separada por tipo y marcas de medición",
    "Auxiliar de construcción", AL_EVENTO, "Excluida",
    "Tienen NA estructural (no aplican al otro tipo) y eliminarían filas en los modelos; se usan solo en la auditoría."),
  d(c("inicio", "fin"), "Fecha y hora de inicio y de fin como fecha-hora", "Auxiliar de construcción", AL_EVENTO,
    "Excluida", "Insumo para dur_min, mes_num y hora_inicio."),
  d("year", "Año del evento", "Partición", AL_EVENTO,
    "Solo para particionar", "La reserva son años no vistos; usarlo como predictor exigiría extrapolar en el tiempo."),
  
  d(c("month_name", "begin_yearmonth", "begin_day", "begin_time", "end_yearmonth", "end_day", "end_time",
      "begin_date_time", "end_date_time"), "Fechas y horas en texto o en partes", "Redundante", AL_EVENTO,
    "Excluida", "Duplican inicio y fin; se reemplazan por mes_num, hora_inicio y dur_min."),
  d(c("state_fips", "cz_fips", "cz_timezone"), "Códigos de estado y de condado o zona, y zona horaria",
    "Redundante", AL_EVENTO, "Excluida", "Duplican state y la ubicación."),
  d("cz_name", "Nombre del condado o zona", "Predictor", AL_EVENTO,
    "Excluida", "Alta cardinalidad; la ubicación ya está en state, wfo y las coordenadas."),
  d(c("begin_range", "begin_azimuth", "begin_location", "end_range", "end_azimuth", "end_location"),
    "Distancia, dirección y localidad de referencia del inicio y del fin", "Redundante", AL_EVENTO,
    "Excluida", "Alta cardinalidad; la posición ya está en las coordenadas."),
  d(c("end_lat", "end_lon"), "Latitud y longitud del final del evento", "Predictor", AL_EVENTO,
    "Excluida (a decidir)", "Permitiría derivar la distancia recorrida; hoy no se usa en el paquete común."),
  d("data_source", "Metadato de procedencia del registro", "Redundante", "No aplica",
    "Excluida", "No describe el fenómeno."),
  
  d(c("injuries_direct", "injuries_indirect", "deaths_direct", "deaths_indirect", "damage_crops"),
    "Lesiones, muertes y daño a cultivos", "Consecuencia posterior", POSTERIOR,
    "Excluida", "Son resultados del mismo evento y se asocian al daño; revelarían la respuesta."),
  d(c("event_narrative", "episode_narrative"), "Texto descriptivo del evento y del episodio",
    "Texto con consecuencias", POSTERIOR, "Excluida", "Puede mencionar pérdidas; se usa solo para auditar registros."),
  d(c("tor_f_scale", "tor_length", "tor_width", "tor_other_wfo", "tor_other_cz_state",
      "tor_other_cz_fips", "tor_other_cz_name"), "Campos de tornados", "No aplica al recorte", "No aplica",
    "Excluida", "El recorte solo tiene granizo y viento de tormenta."),
  d("flood_cause", "Causa de la inundación", "No aplica al recorte", "No aplica",
    "Excluida", "No aplica a granizo ni a viento."),
  d("category", "Categoría (sin uso en la fuente)", "No aplica al recorte", "No aplica",
    "Excluida", "Vacía en la fuente según el diccionario.")
)

# Cobertura: ninguna columna de la base puede quedar sin definir
faltan <- setdiff(names(storm_limpio), diccionario$variable)
sobran <- setdiff(diccionario$variable, names(storm_limpio))
if (length(faltan)) message("SIN DEFINIR en el diccionario: ", paste(faltan, collapse = ", "))
if (length(sobran)) message("En el diccionario pero NO en la base: ", paste(sobran, collapse = ", "))

estad <- tibble(
  variable    = names(storm_limpio),
  tipo        = vapply(storm_limpio, function(x) class(x)[1], character(1)),
  n_distintos = vapply(storm_limpio, n_distinct, integer(1)),
  prop_na     = vapply(storm_limpio, function(x)
    if (is.character(x)) mean(is.na(x) | x == "") else mean(is.na(x)), numeric(1)))

orden_rol <- c("Identificador", "Respuesta", "Predictor", "Auxiliar de construcción", "Partición",
               "Consecuencia posterior", "Texto con consecuencias", "Redundante", "No aplica al recorte")

tabla_variables <- diccionario %>%
  left_join(estad, by = "variable") %>%
  mutate(rol = factor(rol, levels = orden_rol), prop_na = round(prop_na, 4)) %>%
  arrange(rol, variable) %>%
  select(variable, interpretacion, tipo, rol, disponibilidad, decision, prop_na, n_distintos, justificacion)

# Para el cuerpo del informe: una fila por grupo con la misma decisión y justificación
tabla_variables_informe <- tabla_variables %>%
  group_by(rol, decision, justificacion) %>%
  summarise(variables = paste(variable, collapse = ", "), n = n(), .groups = "drop")
write_excel_csv(diccionario, "salidas/diccionario_variables.csv")
write_excel_csv(tabla_variables,         "salidas/tabla_variables.csv")
write_excel_csv(tabla_variables_informe, "salidas/tabla_variables_informe.csv")

tabla_variables %>% filter(decision == "Incluida si varía", n_distintos <= 1)   # variables constantes: descartar



##### FIGURAS DESCRIPTIVAS #####
tema <- theme_minimal(base_size = 11)
guardar <- function(p, nombre, w = 7, h = 4.5){
  ggsave(file.path("resultados/figuras", nombre), p, width = w, height = h, dpi = 200, bg = "white")
}
todo <- storm_limpio %>%
  mutate(tipo = recode(as.character(event_type), "Hail" = "Granizo", "Thunderstorm Wind" = "Viento de tormenta"))
dev  <- todo %>% filter(year <= 2021, dano_informado)       # respuesta: SOLO desarrollo

# f1. Ceros frente a positivos por tipo
f1 <- dev %>%
  mutate(clase = if_else(dano_usd > 0, "Con daño", "Daño cero")) %>%
  count(tipo, clase) %>% group_by(tipo) %>% mutate(prop = n / sum(n)) %>% ungroup() %>%
  ggplot(aes(tipo, prop, fill = clase)) +
  geom_col() +
  geom_text(aes(label = paste0(percent(prop, accuracy = 0.1), " (n = ", comma(n), ")")),
            position = position_stack(vjust = 0.5), size = 3.2) +
  scale_y_continuous(labels = percent) +
  labs(title = "Eventos con daño a la propiedad según tipo, desarrollo 2018–2021",
       x = "Tipo de evento", y = "Proporción de eventos", fill = NULL) + tema
guardar(f1, "f1_ceros_positivos_tipo.png")

# f2. Distribución del daño positivo (escala logarítmica)
f2 <- dev %>% filter(dano_usd > 0) %>%
  ggplot(aes(dano_usd, fill = tipo)) +
  geom_histogram(bins = 40, position = "identity", alpha = 0.6) +
  scale_x_log10(labels = dollar) +
  labs(title = "Distribución del daño positivo, desarrollo 2018–2021",
       x = "Daño a la propiedad (USD nominales, escala log)", y = "Número de eventos", fill = "Tipo de evento") + tema
guardar(f2, "f2_dano_positivo_log.png")

# f3. Respuesta por año y tipo (desarrollo)
f3 <- todo %>% filter(year <= 2021) %>%
  mutate(clase = factor(case_when(is.na(dano_usd) ~ "Sin daño informado", dano_usd == 0 ~ "Daño cero",
                                  TRUE ~ "Con daño"),
                        levels = c("Sin daño informado", "Daño cero", "Con daño"))) %>%
  count(year, tipo, clase) %>%
  ggplot(aes(factor(year), n, fill = clase)) +
  geom_col() + facet_wrap(~ tipo) + scale_y_continuous(labels = comma) +
  labs(title = "Estado de la respuesta por año y tipo, desarrollo 2018–2021",
       x = "Año", y = "Número de eventos", fill = NULL) + tema
guardar(f3, "f3_respuesta_por_anio.png", w = 8)

# f4. Cobertura temporal (cuenta eventos, no mira la respuesta)
f4 <- todo %>% mutate(periodo = if_else(year <= 2021, "Desarrollo", "Reserva")) %>%
  count(year, tipo, periodo) %>%
  ggplot(aes(factor(year), n, fill = tipo)) +
  geom_col(position = "dodge") +
  facet_grid(~ periodo, scales = "free_x", space = "free_x") +
  scale_y_continuous(labels = comma) +
  labs(title = "Eventos registrados por año, tipo y período",
       x = "Año", y = "Número de eventos", fill = "Tipo de evento") + tema
guardar(f4, "f4_cobertura_temporal.png", w = 8)
todo %>% count(state, tipo) %>% write_csv("salidas/eventos_por_estado_tipo.csv")

# f5. Cobertura geográfica
f5 <- todo %>% filter(coord_ok) %>%
  ggplot(aes(begin_lon, begin_lat, color = state)) +
  geom_point(alpha = 0.35, size = 0.6) + coord_quickmap() + facet_wrap(~ tipo) +
  labs(title = "Ubicación de los eventos registrados", x = "Longitud (grados)", y = "Latitud (grados)", color = "Estado") + tema
guardar(f5, "f5_cobertura_geografica.png", w = 9, h = 4.5)

# f6. Concentración del daño (desarrollo)
lorenz <- function(x) {
  x <- sort(x, decreasing = TRUE)
  tibble(prop_unidades = seq_along(x) / length(x), prop_dano = cumsum(x) / sum(x))
}
por_episodio <- dev %>% group_by(episode_id) %>% summarise(d = sum(dano_usd), .groups = "drop")
conc <- bind_rows(mutate(lorenz(dev$dano_usd), unidad = "Eventos"),
                  mutate(lorenz(por_episodio$d), unidad = "Episodios"))
f6 <- ggplot(conc, aes(prop_unidades, prop_dano, color = unidad)) +
  geom_line(linewidth = 0.9) +
  scale_x_log10(labels = percent) + scale_y_continuous(labels = percent) +
  labs(title = "Concentración del daño, desarrollo 2018–2021",
       x = "Proporción acumulada de unidades, de mayor a menor daño (escala log)",
       y = "Proporción acumulada del daño total", color = "Unidad") + tema
guardar(f6, "f6_concentracion_dano.png")

# f7. Proporción con daño según tramo de magnitud, por tipo (desarrollo)
tramos <- function(d, cortes, etiquetas) d %>%
  mutate(tramo = as.character(cut(magnitude, cortes, right = FALSE, labels = etiquetas))) %>%
  group_by(tipo, tramo) %>%
  summarise(n = n(), prop_dano = mean(dano_usd > 0), .groups = "drop")

mag_tramos <- bind_rows(
  tramos(filter(dev, event_type == "Hail") %>% mutate(tipo = "Granizo (pulgadas)"),
         c(0, 1, 1.75, 2.5, Inf), c("<1", "1–1,75", "1,75–2,5", "≥2,5")),
  tramos(filter(dev, event_type == "Thunderstorm Wind") %>% mutate(tipo = "Viento de tormenta (nudos)"),
         c(0, 50, 58, 65, Inf), c("<50", "50–58", "58–65", "≥65"))) %>%
  mutate(tramo = factor(tramo, levels = unique(tramo)))

f7 <- ggplot(mag_tramos, aes(tramo, prop_dano)) +
  geom_col(fill = "#2c7fb8") +
  geom_text(aes(label = paste0("n = ", comma(n))), vjust = -0.4, size = 3) +
  facet_wrap(~ tipo, scales = "free_x") +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Eventos con daño según la magnitud, desarrollo 2018–2021",
       x = "Tramo de magnitud", y = "Eventos con daño (%)") + tema
guardar(f7, "f7_dano_segun_magnitud.png", w = 8)

# f8. Eventos por episodio
f8 <- todo %>% count(episode_id, name = "eventos") %>%
  ggplot(aes(eventos)) +
  geom_histogram(bins = 40, fill = "#2c7fb8") +
  scale_x_log10(labels = comma) + scale_y_continuous(labels = comma) +
  labs(title = "Número de eventos por episodio", x = "Eventos por episodio (escala log)", y = "Número de episodios") + tema
guardar(f8, "f8_eventos_por_episodio.png")

# Diagrama de ventanas de validación
vent <- tibble(y = 3:1, entr_ini = 2018, entr_fin = c(2018, 2019, 2020), val = c(2019, 2020, 2021))
fig_vent <- ggplot(vent) +
  geom_rect(aes(xmin = entr_ini - 0.45, xmax = entr_fin + 0.45, ymin = y - 0.3, ymax = y + 0.3, fill = "Entrenamiento")) +
  geom_rect(aes(xmin = val - 0.45, xmax = val + 0.45, ymin = y - 0.3, ymax = y + 0.3, fill = "Validación")) +
  annotate("rect", xmin = 2021.55, xmax = 2023.45, ymin = 0.5, ymax = 3.5, fill = "grey70", alpha = 0.5) +
  annotate("text", x = 2022.5, y = 2, label = "Reserva de prueba\n(se evalúa una sola vez)") +
  scale_y_continuous(breaks = 3:1, labels = paste("Ventana", 1:3)) +
  scale_x_continuous(breaks = 2018:2023) +
  scale_fill_manual(values = c(Entrenamiento = "#2c7fb8", Validación = "#f0a30a")) +
  labs(title = "Esquema de validación temporal", x = "Año", y = NULL, fill = NULL) + tema
guardar(fig_vent, "diagrama_ventanas.png")

# Diagrama de construcción de la muestra
flujo <- log_muestra %>% mutate(i = row_number(),
                                etiqueta = paste0(paso, "\n", comma(filas), " eventos | ", comma(episodios), " episodios"))
fig_flujo <- ggplot(flujo, aes(x = 0, y = -i)) +
  geom_tile(width = 1.9, height = 0.7, fill = "grey92", color = "grey40") +
  geom_text(aes(label = etiqueta), size = 3.2) +
  geom_segment(data = filter(flujo, i < max(i)),
               aes(x = 0, xend = 0, y = -i - 0.35, yend = -i - 0.65), arrow = arrow(length = unit(0.12, "cm"))) +
  xlim(-1, 1) + theme_void() + labs(title = "Construcción de la muestra analítica")
guardar(fig_flujo, "diagrama_construccion_muestra.png", w = 6, h = 5)
