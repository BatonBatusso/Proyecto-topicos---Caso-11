suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(ggplot2)
})

# =============================================================================
# CARGA DE PREDICCIONES DE LOS MODELOS EXTERNOS
# (Asume que el orquestador ya corrió los modelos de los otros integrantes)
# =============================================================================
predicciones_m2 <- read_csv("integrante2_PE/resultados_m2/predicciones_m2.csv")
predicciones_m1 <- readRDS("integrante3_LM/Resultados/m1/predicciones_desarrollo_m1.rds")
predicciones_m3 <- readRDS("integrante4_GT/resultados/m3/m3_predicciones_desarrollo.rds")

predicciones_m3 <- predicciones_m3 %>%
  filter(uso_wfo == "sin_wfo")

dir.create("resultados/predicciones", recursive = TRUE, showWarnings = FALSE)
saveRDS(predicciones_m1,"resultados/predicciones/predicciones_desarrollo_m1.rds")
saveRDS(predicciones_m2,"resultados/predicciones/predicciones_desarrollo_m2.rds")
saveRDS(predicciones_m3,"resultados/predicciones/predicciones_desarrollo_m3.rds")

# =============================================================================
# R/07_comparacion_incertidumbre_diagnostico.R
# Caso 11 (NOAA Storm Events) - Referencia (mediana = 0), M1, M2 y M3
# =============================================================================

# ------------------------------- CONFIGURACION -------------------------------
RUTAS_PRED <- c(                                  
  referencia = "resultados/referencia/predicciones_desarrollo_referencia.rds",
  M1         = "resultados/predicciones/predicciones_desarrollo_m1.rds",
  M2         = "resultados/predicciones/predicciones_desarrollo_m2.rds",
  M3         = "resultados/predicciones/predicciones_desarrollo_m3.rds"
)
VARIANTES <- c("original", "log1p")  
B_BOOT  <- 2000                    
SEMILLA <- 3407
REF     <- "Referencia (mediana = 0)"

dir.create("resultados/comparacion",   recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/incertidumbre", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/figuras",       recursive = TRUE, showWarnings = FALSE)

# ------------------------ FUNCIONES DE BOOTSTRAP -----------------------------
# Se cargan las funciones asumiendo que "analisis_incertidumbre.R" ya no tiene 
# código de prueba al final que ensucie el entorno.
source("integrante1_MB/analisis_incertidumbre.R", encoding = "UTF-8")

stopifnot(exists("bootstrap_episodios"), exists("concentracion_error"),
          exists("diagnostico_grupos"))

# =============================================================================
# A. CARGA, NORMALIZACION Y CONTROL DE FILAS COMUNES
# =============================================================================
umbral_extremo      <- configuracion_comun$umbral_extremo_usd   

# IDs como texto SIN notacion cientifica (as.character(100000) daria "1e+05")
id_chr <- function(x) if (is.numeric(x)) sprintf("%.0f", x) else as.character(x)

leer_pred <- function(ruta) {
  x <- as.data.frame(readRDS(unname(ruta)))
  names(x) <- tolower(names(x))
  req <- c("event_id", "episode_id", "anio_validacion", "y_observado",
           "y_predicho", "modelo", "variante_respuesta")
  if (length(setdiff(req, names(x))))
    stop(ruta, ": faltan columnas: ", paste(setdiff(req, names(x)), collapse = ", "))
  tibble(event_id           = id_chr(x$event_id),
         episode_id         = id_chr(x$episode_id),
         anio_validacion    = as.integer(as.character(x$anio_validacion)),
         y_observado        = as.numeric(x$y_observado),
         y_predicho         = as.numeric(x$y_predicho),
         modelo             = as.character(x$modelo),
         variante_respuesta = as.character(x$variante_respuesta),
         configuracion      = if ("configuracion" %in% names(x))
           as.character(x$configuracion) else NA_character_)
}

atributos <- base_desarrollo %>%
  transmute(event_id = id_chr(event_id), state, event_type, mag_origen)
stopifnot(!anyDuplicated(atributos$event_id))

# ---- Referencias: con mediana = 0, referencia_global y referencia_tipo coinciden
pred_ref <- leer_pred(RUTAS_PRED["referencia"])
ref_g <- filter(pred_ref, modelo == "referencia_global")
ref_t <- filter(pred_ref, modelo == "referencia_tipo")
cmp <- inner_join(select(ref_g, event_id, anio_validacion, g = y_predicho),
                  select(ref_t, event_id, anio_validacion, t = y_predicho),
                  by = c("event_id", "anio_validacion"))
ref_identicas <- nrow(cmp) == nrow(ref_g) && isTRUE(all.equal(cmp$g, cmp$t))
message("Referencia global == referencia por tipo: ", ref_identicas,
        " | predicciones distintas de 0: ", sum(ref_g$y_predicho != 0))

if (ref_identicas) {                      
  pred_ref <- mutate(ref_g, modelo = REF)
  refs <- REF
} else {
  refs <- c("referencia_global", "referencia_tipo")
}
ref_principal <- refs[1]

# ---- Modelos
pred_mod <- bind_rows(lapply(c("M1", "M2", "M3"), function(k) leer_pred(RUTAS_PRED[k]))) %>%
  mutate(modelo = paste0(modelo, " [", variante_respuesta, "]"))

pred <- bind_rows(pred_ref, pred_mod)
modelos <- setdiff(unique(pred$modelo), refs)
stopifnot(all(pred$event_id %in% atributos$event_id))

# ---- Control de filas comunes
k_ref <- pred %>% filter(modelo == ref_principal) %>%
  transmute(k = paste(event_id, anio_validacion)) %>% pull(k)
y_ref <- pred %>% filter(modelo == ref_principal) %>%
  select(event_id, anio_validacion, y_ref = y_observado)

control_filas <- pred %>%
  group_by(modelo) %>%
  summarise(n_filas = n(),
            n_eventos = n_distinct(event_id),
            n_episodios = n_distinct(episode_id),
            duplicados = n() - n_distinct(event_id, anio_validacion),
            mismas_filas_que_ref = setequal(paste(event_id, anio_validacion), k_ref),
            n_pred_NA = sum(is.na(y_predicho)),
            n_pred_negativas = sum(y_predicho < 0, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(pred %>% inner_join(y_ref, by = c("event_id", "anio_validacion")) %>%
              group_by(modelo) %>%
              summarise(y_obs_coincide = all(abs(y_observado - y_ref) < 1e-6),
                        .groups = "drop"),
            by = "modelo")
print(control_filas)
write_csv(control_filas, "resultados/comparacion/control_filas_comunes.csv")
stopifnot(all(control_filas$mismas_filas_que_ref), all(control_filas$duplicados == 0),
          all(control_filas$y_obs_coincide), all(control_filas$n_pred_NA == 0),
          all(control_filas$n_pred_negativas == 0))

# =============================================================================
# B. COMPARACION DE METRICAS 
# =============================================================================
met <- function(g) summarise(g,
                             n               = n(),
                             n_episodios     = n_distinct(episode_id),
                             MAE             = mean(abs(y_observado - y_predicho)),
                             RMSE            = sqrt(mean((y_observado - y_predicho)^2)),
                             sesgo           = mean(y_predicho - y_observado),          
                             MAE_ceros       = mean(abs(y_observado - y_predicho)[y_observado == 0]),
                             MAE_positivos   = mean(abs(y_observado - y_predicho)[y_observado > 0]),
                             .groups = "drop")

metricas <- bind_rows(
  pred %>% group_by(modelo, variante_respuesta, ventana = as.character(anio_validacion)) %>% met(),
  pred %>% group_by(modelo, variante_respuesta) %>% met() %>% mutate(ventana = "agrupado"))

ref_m <- metricas %>% filter(modelo == ref_principal) %>%
  select(ventana, MAE_ref = MAE, RMSE_ref = RMSE)

tabla_comparativa <- metricas %>%
  left_join(ref_m, by = "ventana") %>%
  mutate(dif_MAE    = MAE - MAE_ref,            
         dif_RMSE   = RMSE - RMSE_ref,
         skill_MAE  = 1 - MAE / MAE_ref,        
         skill_RMSE = 1 - RMSE / RMSE_ref) %>%
  arrange(factor(ventana, c(sort(setdiff(unique(ventana), "agrupado")), "agrupado")), MAE)

write_csv(tabla_comparativa, "resultados/comparacion/tabla_comparativa_larga.csv")

tabla_ancha <- tabla_comparativa %>%
  select(modelo, ventana, MAE, RMSE) %>%
  pivot_wider(names_from = ventana, values_from = c(MAE, RMSE))
write_csv(tabla_ancha, "resultados/comparacion/tabla_comparativa_ancha.csv")

estabilidad <- tabla_comparativa %>%
  filter(ventana != "agrupado") %>%
  group_by(modelo) %>%
  summarise(n_ventanas = n(),
            MAE_medio = mean(MAE), MAE_sd = sd(MAE),
            MAE_min = min(MAE), MAE_max = max(MAE),
            ventanas_mejor_que_ref = sum(dif_MAE < 0),
            .groups = "drop")
write_csv(estabilidad, "resultados/comparacion/estabilidad_ventanas.csv")

sens_log1p <- tabla_comparativa %>%
  filter(ventana == "agrupado", modelo != ref_principal) %>%
  mutate(familia = sub(" \\[.*$", "", modelo)) %>%
  select(familia, variante_respuesta, MAE, RMSE, sesgo, MAE_ceros, MAE_positivos) %>%
  pivot_wider(names_from = variante_respuesta,
              values_from = c(MAE, RMSE, sesgo, MAE_ceros, MAE_positivos))
write_csv(sens_log1p, "resultados/comparacion/sensibilidad1_original_vs_log1p.csv")

# Costo computacional
f_t <- list.files("resultados", pattern = "^tiempos_.*\\.csv$", recursive = TRUE, full.names = TRUE)
if (length(f_t)) {
  tiempos <- tryCatch(
    bind_rows(lapply(f_t, function(f)
      mutate(read_csv(f, col_types = cols(.default = "c"), show_col_types = FALSE),
             archivo = basename(f), .before = 1))),
    error = function(e) NULL)
  if (!is.null(tiempos)) write_csv(tiempos, "resultados/comparacion/tiempos_modelos.csv")
}

print(tabla_ancha); print(estabilidad); print(sens_log1p)

# =============================================================================
# C. INCERTIDUMBRE
# =============================================================================
ic_ref <- bind_rows(lapply(modelos, function(m)
  bind_rows(lapply(refs, function(r)
    bootstrap_episodios(pred, m, modelo_ref = r, B = B_BOOT, seed = SEMILLA))))) %>%
  mutate(conclusion = case_when(ic95_sup < 0 ~ "mejor que la referencia",
                                ic95_inf > 0 ~ "peor que la referencia",
                                TRUE         ~ "no distinguible de la referencia"))
write_csv(ic_ref, "resultados/incertidumbre/ic_diferencia_mae_vs_referencia.csv")

ic_pares <- bind_rows(lapply(VARIANTES, function(v) {
  # Corrección: agregar las dobles barras invertidas antes de los corchetes
  mv <- modelos[grepl(paste0("\\[", v, "\\]$"), modelos)]
  
  if (length(mv) < 2) return(NULL)
  bind_rows(lapply(combn(mv, 2, simplify = FALSE), function(p)
    bootstrap_episodios(pred, p[1], modelo_ref = p[2], B = B_BOOT, seed = SEMILLA)))
})) %>%
  mutate(conclusion = case_when(ic95_sup < 0 ~ "el primero tiene menos error",
                                ic95_inf > 0 ~ "el primero tiene mas error",
                                TRUE         ~ "no distinguibles"))

write_csv(ic_pares, "resultados/incertidumbre/ic_diferencia_mae_entre_modelos.csv")

fam <- unique(sub(" \\[.*$", "", modelos))
ic_sens1 <- bind_rows(lapply(fam, function(k)
  bootstrap_episodios(pred, paste0(k, " [log1p]"), modelo_ref = paste0(k, " [original]"),
                      B = B_BOOT, seed = SEMILLA))) %>%
  mutate(conclusion = case_when(ic95_sup < 0 ~ "log1p tiene menos error",
                                ic95_inf > 0 ~ "log1p tiene mas error",
                                TRUE         ~ "no distinguibles"))
write_csv(ic_sens1, "resultados/incertidumbre/ic_sensibilidad1_log1p_vs_original.csv")

# =============================================================================
# D. DIAGNOSTICO
# =============================================================================
diag_g <- diagnostico_grupos(pred, atributos, umbral_extremo)
write_csv(diag_g, "resultados/incertidumbre/diagnostico_grupos.csv")

MODELO_ELEGIDO <- "M3_random_forest [log1p]"

diag_elegido <- function(pred, atributos, m_sel, m_ref, umbral,
                         vars = c("state", "event_type", "mag_origen")) {
  d <- pred %>%
    filter(modelo %in% c(m_sel, m_ref)) %>%
    left_join(atributos, by = "event_id") %>%
    mutate(ae = abs(y_observado - y_predicho),
           periodo = as.character(anio_validacion))
  bind_rows(lapply(c(vars, "periodo"), function(v) {
    d %>%
      mutate(nivel = as.character(.data[[v]])) %>%
      group_by(variable = v, nivel) %>%
      summarise(n = n_distinct(event_id),
                n_episodios = n_distinct(episode_id),
                pct_ceros = mean(y_observado[modelo == m_ref] == 0),
                n_extremos = sum(y_observado[modelo == m_ref] >= umbral),
                MAE_ref = mean(ae[modelo == m_ref]),
                MAE_sel = mean(ae[modelo == m_sel]),
                dif_MAE = MAE_sel - MAE_ref,
                sesgo_sel = mean((y_predicho - y_observado)[modelo == m_sel]),
                err_total_ref = sum(ae[modelo == m_ref]),
                err_total_sel = sum(ae[modelo == m_sel]),
                .groups = "drop")
  })) %>%
    group_by(variable) %>%
    mutate(share_error_ref = err_total_ref / sum(err_total_ref),
           share_error_sel = err_total_sel / sum(err_total_sel)) %>%
    ungroup() %>% select(-err_total_ref)
}

diag_sel <- diag_elegido(pred, atributos, MODELO_ELEGIDO, ref_principal, umbral_extremo)
write_csv(diag_sel, "resultados/incertidumbre/diagnostico_modelo_elegido.csv")

conc <- bind_rows(lapply(unique(pred$modelo), function(m)
  concentracion_error(pred, m, p = c(0.01, 0.05))))
write_csv(conc, "resultados/incertidumbre/concentracion_error.csv")

diag_clases <- pred %>%
  mutate(clase_dano = case_when(y_observado == 0 ~ "cero",
                                y_observado >= umbral_extremo ~ "extremo",
                                TRUE ~ "positivo moderado"),
         ae = abs(y_observado - y_predicho)) %>%
  group_by(modelo) %>% mutate(ae_total = sum(ae)) %>%
  group_by(modelo, clase_dano) %>%
  summarise(n = n(), n_episodios = n_distinct(episode_id),
            dano_obs_medio  = mean(y_observado),
            dano_pred_medio = mean(y_predicho),
            cociente_pred_obs = if_else(sum(y_observado) > 0,
                                        sum(y_predicho) / sum(y_observado), NA_real_),
            MAE = mean(ae), RMSE = sqrt(mean((y_observado - y_predicho)^2)),
            share_error_total = sum(ae) / first(ae_total),
            .groups = "drop")
write_csv(diag_clases, "resultados/incertidumbre/diagnostico_clases_extremos.csv")

# =============================================================================
# D4. FIGURAS
# =============================================================================
ord_modelos <- c(refs, sort(modelos))
usd_corto <- scales::label_dollar(scale_cut = scales::cut_short_scale())

d1 <- diag_g %>% filter(variable == "clase_dano") %>%
  mutate(nivel = factor(nivel, c("cero", "positivo moderado", "extremo")),
         modelo = factor(modelo, ord_modelos)) %>%
  group_by(nivel) %>%
  mutate(panel = paste0(nivel, " (n = ", format(first(n), big.mark = ".", decimal.mark = ",", scientific = FALSE), ")")) %>%
  ungroup()
f1 <- ggplot(d1, aes(modelo, MAE, fill = modelo)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ panel, scales = "free_y") +
  scale_y_continuous(labels = usd_corto) +
  labs(title = "Error absoluto medio por clase de daño (validación 2019-2021)",
       subtitle = paste0("Extremo: daño ≥ ", format(umbral_extremo, big.mark = ".", scientific = FALSE),
                         " USD (umbral definido en desarrollo)"),
       x = NULL, y = "MAE (USD nominales)") +
  theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 40, hjust = 1))
ggsave("resultados/figuras/f9_diagnostico.png", f1, width = 10, height = 5, dpi = 200, bg = "white")

d2 <- ic_ref %>% filter(referencia == ref_principal) %>%
  mutate(familia  = sub(" \\[.*$", "", modelo),
         variante = sub("^.*\\[(.*)\\]$", "\\1", modelo))
orden2 <- d2 %>% filter(ventana == "agrupado") %>% arrange(dif_MAE) %>% pull(familia) %>% unique()
d2 <- d2 %>% mutate(familia  = factor(familia, orden2),
                    variante = factor(variante, c("original", "log1p")),
                    ventana  = factor(ventana, c(sort(setdiff(unique(ventana), "agrupado")), "agrupado")))
f2 <- ggplot(d2, aes(dif_MAE, familia)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(aes(xmin = ic95_inf, xmax = ic95_sup)) +
  facet_wrap(vars(variante, ventana), nrow = 2, scales = "free") +
  scale_x_continuous(labels = usd_corto, n.breaks = 4) +
  labs(title = "Diferencia de MAE frente a la referencia (modelo − referencia)",
       subtitle = paste0("Fila superior: respuesta original. Fila inferior: log1p. ",
                         "IC 95% percentil, B = ", B_BOOT, ". Valores > 0: más error."),
       x = "Diferencia de MAE (USD nominales)", y = NULL) +
  theme_minimal(base_size = 11) + theme(panel.spacing.x = grid::unit(1.2, "lines"))
ggsave("resultados/figuras/f10_incertidumbre_dif_mae.png", f2, width = 12, height = 6, dpi = 200, bg = "white")

f3 <- conc %>% mutate(modelo = factor(modelo, ord_modelos),
                      top = factor(paste0("Top ", 100 * top, "%"))) %>%
  ggplot(aes(top, share_error, fill = modelo)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Concentración del error absoluto total en los eventos más destructivos",
       x = "Eventos ordenados por daño observado", y = "Proporción del error absoluto total",
       fill = NULL) +
  theme_minimal(base_size = 11)
ggsave("resultados/figuras/f11_concentracion_error.png", f3, width = 9, height = 4.5, dpi = 200, bg = "white")

f4 <- pred %>%
  filter(modelo %in% modelos, y_observado > 0, y_predicho > 0) %>%
  ggplot(aes(y_observado, y_predicho)) +
  geom_point(alpha = 0.25, size = 0.7) +
  geom_abline(slope = 1, intercept = 0, colour = "red") +
  scale_x_log10(labels = usd_corto) + scale_y_log10(labels = usd_corto) +
  facet_wrap(~ modelo, ncol = 3) +
  labs(title = "Daño observado vs predicho (escala logarítmica)",
       subtitle = "Línea roja: predicción perfecta. Puntos bajo la línea: subestimación.",
       x = "Daño observado (USD)", y = "Daño predicho (USD)") +
  theme_minimal(base_size = 11)
ggsave("resultados/figuras/f12_observado_vs_predicho.png", f4, width = 11, height = 7, dpi = 200, bg = "white")

message("Listo: tablas en resultados/comparacion y resultados/incertidumbre; figuras en resultados/figuras")
