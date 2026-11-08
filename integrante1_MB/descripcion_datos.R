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

d <- function(variable, interpretacion, rol, disponibilidad, decision, justificacion)
  tibble(variable, interpretacion, rol, disponibilidad, decision, justificacion)

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
    "Incluida si varía", "Control de calidad; si es constante (todas TRUE) no aporta y se descarta."),
  d("cz_type", "Tipo de zona (condado, zona o marino)", "Predictor", AL_EVENTO,
    "Incluida si varía", "Probablemente casi constante en el recorte; se descarta si tiene un solo nivel."),
  d("wfo", "Oficina del NWS responsable del área", "Predictor", AL_EVENTO,
    "Incluida (a justificar)", "Aproxima la ubicación, pero también el mecanismo de reporte; evaluar sensibilidad sin esta variable."),
  d("source", "Fuente que reportó el evento", "Predictor", AL_EVENTO,
    "Incluida (a justificar)", "Refleja cómo se reportó y no el fenómeno físico; puede asociarse a que el daño esté informado. Evaluar sensibilidad sin esta variable."),
  
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

write_excel_csv(tabla_variables,         "salidas/tabla_variables.csv")
write_excel_csv(tabla_variables_informe, "salidas/tabla_variables_informe.csv")

tabla_variables %>% filter(decision == "Incluida si varía", n_distintos <= 1)   # variables constantes: descartar
