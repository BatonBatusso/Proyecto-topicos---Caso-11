# ENTRADAS: resultados/referencia/predicciones_desarrollo_referencia.rds, datos_comunes/*.rds,
#           (opcional) resultados/integracion/predicciones_desarrollo.rds  [confirmar la ruta con la integrante 3]
library(dplyr); library(tidyr); library(readr); library(ggplot2)

source("integrante1_MB/analisis_incertidumbre.R", encoding = "UTF-8")
dir.create("resultados/incertidumbre", recursive = TRUE, showWarnings = FALSE)
dir.create("resultados/figuras", recursive = TRUE, showWarnings = FALSE)

base_desarrollo     <- readRDS("datos_comunes/base_desarrollo.rds")
configuracion_comun <- readRDS("datos_comunes/configuracion_comun.rds")
f_integrada <- "resultados/integracion/predicciones_desarrollo.rds"
pred <- readRDS(if (file.exists(f_integrada)) f_integrada
                else "resultados/referencia/predicciones_desarrollo_referencia.rds")

refs      <- c("referencia_global", "referencia_tipo")
modelos   <- setdiff(unique(pred$modelo), refs)       # una fila por modelo, evento y ventana
atributos <- base_desarrollo %>% select(event_id, state, event_type, mag_origen)

diag_g <- diagnostico_grupos(pred, atributos, configuracion_comun$umbral_extremo_usd)
write_csv(diag_g, "resultados/incertidumbre/diagnostico_grupos.csv")

conc <- bind_rows(lapply(unique(pred$modelo), function(m) concentracion_error(pred, m)))
write_csv(conc, "resultados/incertidumbre/concentracion_error.csv")

if (length(modelos) > 0) {                             # solo cuando existan modelos
  ic <- bind_rows(lapply(modelos, function(m)
    bind_rows(lapply(refs, function(r) bootstrap_episodios(pred, m, modelo_ref = r)))))
  write_csv(ic, "resultados/incertidumbre/ic_diferencia_mae.csv")
}

d9 <- diag_g %>% filter(variable == "clase_dano") %>%
  mutate(nivel = factor(nivel, c("cero", "positivo moderado", "extremo"))) %>%
  group_by(nivel) %>%
  mutate(panel = paste0(nivel, " (n = ", format(first(n), big.mark = "."), ")")) %>% ungroup()
f9 <- ggplot(d9, aes(modelo, MAE, fill = modelo)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ panel, scales = "free_y") +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Error absoluto medio por clase de daño, desarrollo 2018–2021",
       x = NULL, y = "MAE (USD nominales)") +
  theme_minimal(base_size = 11) + theme(axis.text.x = element_text(angle = 25, hjust = 1))
ggsave("resultados/figuras/f9_diagnostico.png", f9, width = 8, height = 4.2, dpi = 200, bg = "white")
