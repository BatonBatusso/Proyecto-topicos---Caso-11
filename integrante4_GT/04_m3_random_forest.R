################################################################################
# M3: RANDOM FOREST (ranger)  |
################################################################################

source("../preparacion_base_datos.R", chdir = TRUE) 
library(ranger)
library(lubridate)

n_combinaciones <- 20       
metrica_seleccion <- "mae"  
dir.create("resultados/m3", recursive = TRUE, showWarnings = FALSE)

# --- Subgrupos (Sensibilidad 2) -----------------------------------------------
# Si el equipo la define en otro lado, reemplazar esta funcion por esa definicion.
asignar_subgrupo <- function(df) {
  dplyr::case_when(
    df$mag_origen == "granizo"         ~ "Hail",
    df$mag_origen == "viento_medido"   ~ "Viento medido (MG/MS)",
    df$mag_origen == "viento_estimado" ~ "Viento estimado (EG/ES)")
}

# --- Preprocesamiento: SE APRENDE SOLO EN TRAIN -------------------------------
ajustar_prep <- function(train, usar_source = TRUE) {
  tmp <- train                                
  num <- c("magnitude", "begin_range", "begin_lat", "begin_lon",
           "dur_seg", "hora_inicio", "mes_num")
  fac <- c("event_type", "state", "wfo", "cz_type", "magnitude_type",
           if (usar_source) "source")
  list(num = num, fac = fac,
       medianas = sapply(tmp[num], median, na.rm = TRUE),
       niveles  = lapply(tmp[fac], function(x) levels(droplevels(factor(x)))))
}

aplicar_prep <- function(df, prep) {
  out <- as.data.frame(df[prep$num])
  for (v in prep$num) out[[v]][is.na(out[[v]])] <- prep$medianas[[v]]
  out$mag_ausente <- as.numeric(df$mag_ausente)
  for (v in prep$fac) {
    x <- as.character(df[[v]])
    x[is.na(x)] <- "ND"
    x[!x %in% c(prep$niveles[[v]], "ND")] <- "OTRO"   # nivel nunca visto en train
    out[[v]] <- factor(x, levels = unique(c(prep$niveles[[v]], "ND", "OTRO")))
  }
  out
}

# --- Metricas -----------------------------------------------------------------
metricas <- function(obs, pred) {
  err <- abs(obs - pred)
  ext <- obs >= quantile(obs, 0.95)
  c(mae = mean(err), rmse = sqrt(mean((obs - pred)^2)),
    rmsle = sqrt(mean((log1p(obs) - log1p(pred))^2)),
    prop_ceros = mean(obs == 0), mae_ceros = mean(err[obs == 0]),
    mae_positivos = mean(err[obs > 0]),
    mae_extremos = mean(err[ext]), sesgo_extremos = mean(pred[ext] - obs[ext]),
    pct_error_extremos = sum(err[ext]) / sum(err))
}

# --- Ajuste de un RF ----------------------------------------------------------
ajustar_rf <- function(train, valid, hp, log_resp = FALSE, usar_source = TRUE,
                       importancia = "none") {
  prep <- ajustar_prep(train, usar_source)
  Xtr  <- aplicar_prep(train, prep)
  Xva  <- aplicar_prep(valid, prep)
  Xtr$y <- if (log_resp) log1p(train$dano_usd) else train$dano_usd
  t0 <- proc.time()[["elapsed"]]
  m  <- ranger(y ~ ., data = Xtr, num.trees = n_arboles,
               mtry = hp$mtry, min.node.size = hp$min.node.size,
               sample.fraction = hp$sample.fraction,
               importance = importancia, seed = SEMILLA, num.threads = 1)
  tiempo <- proc.time()[["elapsed"]] - t0
  pred <- predict(m, Xva)$predictions
  if (log_resp) pred <- expm1(pred)            # retransformar ANTES de guardar
  list(pred = pmax(pred, 0), importancia = m$variable.importance, tiempo = tiempo)
}


# --- Revisión n arboles -------------------------------------------------------
bs       <- seq(50, 1000, 50)
semillas <- SEMILLA + 0:4     

# El preprocesamiento se aprende una vez por ventana
datos_v <- lapply(ventanas, function(w) {
  prep <- ajustar_prep(w$train)
  Xtr  <- aplicar_prep(w$train, prep)
  Xtr$y <- w$train$dano_usd
  list(Xtr = Xtr, Xva = aplicar_prep(w$valid, prep), yva = w$valid$dano_usd)
})

curva_todas <- do.call(rbind, lapply(semillas, function(s) {
  do.call(rbind, lapply(names(datos_v), function(w) {
    d <- datos_v[[w]]
    m <- ranger(y ~ ., data = d$Xtr, num.trees = 1000,
                mtry = max(1, round((ncol(d$Xtr) - 1) / 2)),   # config provisoria
                min.node.size = 5, sample.fraction = 0.7, replace = TRUE,
                seed = s, num.threads = 1)
    P <- predict(m, d$Xva, predict.all = TRUE)$predictions     # n x 1000
    data.frame(semilla = s, ventana = w, B = bs,
               mae = sapply(bs, function(b) mean(abs(d$yva - rowMeans(P[, 1:b, drop = FALSE])))))
  }))
}))
write.csv(curva_todas, "resultados/m3/curva_num_arboles_semillas.csv", row.names = FALSE)

# Curva promedio entre semillas 
curva_B <- curva_todas %>%
  group_by(ventana, B) %>%
  summarise(mae_medio = mean(mae), mae_sd = sd(mae), .groups = "drop") %>%
  rename(mae = mae_medio)
write.csv(curva_B, "resultados/m3/curva_num_arboles.csv", row.names = FALSE)

umbral <- 0.5   

estab <- curva_B %>%
  group_by(ventana) %>%
  mutate(ref = mean(mae[B >= 500]),                 # nivel estable de la ventana
         dif = abs(mae - ref) / ref * 100) %>%
  group_by(B) %>%
  summarise(max_dif = max(dif), .groups = "drop") %>%
  mutate(ok = rev(cummax(rev(max_dif))) <= umbral)  # cumple desde aquí hasta el final

B_sugerido <- min(estab$B[estab$ok])
print(estab, n = Inf)
cat("B sugerido:", B_sugerido, "\n")
n_arboles <- B_sugerido

# --- Busqueda de hiperparametros ----------------------------------------------
p <- ncol(aplicar_prep(ventanas$V1$train, ajustar_prep(ventanas$V1$train))) 
grilla <- expand.grid(mtry = unique(pmax(1, round(p * c(.2, .4, .6, .8, 1)))),
                      min.node.size   = c(1, 5, 10, 25, 50),
                      sample.fraction = c(.5, .7, .9))
set.seed(SEMILLA)
pool <- grilla[grilla$mtry < p, ]                    # excluye mtry = p, que se agrega aparte
grilla <- pool[sample(nrow(pool), min(n_combinaciones - 1, nrow(pool))), ]
grilla <- rbind(grilla, data.frame(mtry = p, min.node.size = 5, sample.fraction = 1))  # bagging (m = p)
grilla$id_hp <- seq_len(nrow(grilla))

res_hp <- list()
for (i in grilla$id_hp) {
  for (w in names(ventanas)) {
    r <- ajustar_rf(ventanas[[w]]$train, ventanas[[w]]$valid, grilla[i, ])
    res_hp[[length(res_hp) + 1]] <- data.frame(
      grilla[i, ], ventana = w, t(metricas(ventanas[[w]]$valid$dano_usd, r$pred)),
      tiempo_seg = r$tiempo)
  }
}
res_hp <- do.call(rbind, res_hp)
write.csv(res_hp, "resultados/m3/hiperparametros_busqueda.csv", row.names = FALSE)

resumen_hp <- aggregate(res_hp[[metrica_seleccion]],
                        by = list(id_hp = res_hp$id_hp), FUN = mean)
mejor_id <- resumen_hp$id_hp[which.min(resumen_hp$x)]
hp_seleccionados <- as.list(grilla[grilla$id_hp == mejor_id, c("mtry", "min.node.size", "sample.fraction")])
saveRDS(hp_seleccionados, "resultados/m3/hiperparametros_congelados.rds")

# --- Funcion final con hiperparametros congelados -----------------------------
m3_final <- function(train, test, log_resp = FALSE, usar_source = TRUE,
                     importancia = "permutation") {
  ajustar_rf(train, test, hp_seleccionados, log_resp, usar_source, importancia)
}

# --- Ejecucion: original vs log, con y sin SOURCE, en las 3 ventanas ----------
escenarios <- expand.grid(log_resp = c(FALSE, TRUE), usar_source = c(TRUE, FALSE))
preds <- list() 
imps <- list()
tiempos <- list() 
mets <- list()

for (e in seq_len(nrow(escenarios))) {
  for (w in names(ventanas)) {
    v <- ventanas[[w]]$valid
    r <- m3_final(ventanas[[w]]$train, v, escenarios$log_resp[e], escenarios$usar_source[e])
    etq <- list(ventana = w,
                respuesta = ifelse(escenarios$log_resp[e], "log1p", "original"),
                source = ifelse(escenarios$usar_source[e], "con_source", "sin_source"))
    preds[[length(preds) + 1]] <- data.frame(
      event_id = v$event_id, etq, subgrupo = asignar_subgrupo(v),
      y_obs = v$dano_usd, y_pred = r$pred)
    imps[[length(imps) + 1]] <- data.frame(variable = names(r$importancia), etq,
                                           importancia = unname(r$importancia))
    tiempos[[length(tiempos) + 1]] <- data.frame(etq, tiempo_seg = r$tiempo)
  }
}
preds   <- do.call(rbind, preds)
imps    <- do.call(rbind, imps)
tiempos <- do.call(rbind, tiempos)
write.csv(preds,   "resultados/m3/predicciones_m3.csv",  row.names = FALSE)
write.csv(imps,    "resultados/m3/importancia_m3.csv",   row.names = FALSE)
write.csv(tiempos, "resultados/m3/tiempos_m3.csv",       row.names = FALSE)

# --- Sensibilidad 1: importancia y extremos (original vs log) -----------------
base_s <- preds[preds$source == "con_source", ]
met_s1 <- do.call(rbind, lapply(split(base_s, list(base_s$ventana, base_s$respuesta)), function(d)
  data.frame(ventana = d$ventana[1], respuesta = d$respuesta[1],
             t(metricas(d$y_obs, d$y_pred)))))
write.csv(met_s1, "resultados/m3/sensibilidad1_metricas.csv", row.names = FALSE)

imp_s <- imps[imps$source == "con_source", ]
imp_med <- aggregate(importancia ~ variable + respuesta, imp_s, mean)
imp_ancho <- reshape(imp_med, idvar = "variable", timevar = "respuesta", direction = "wide")
write.csv(imp_ancho, "resultados/m3/sensibilidad1_importancia.csv", row.names = FALSE)
cat("Spearman entre rankings de importancia (original vs log):",
    round(cor(imp_ancho$importancia.original, imp_ancho$importancia.log1p,
              method = "spearman"), 3), "\n")

# --- Sensibilidad 2: metricas por subgrupo ------------------------------------
met_s2 <- do.call(rbind, lapply(split(preds, list(preds$ventana, preds$respuesta,
                                                  preds$source, preds$subgrupo), drop = TRUE), function(d)
  data.frame(d[1, c("ventana", "respuesta", "source", "subgrupo")],
             n = nrow(d), t(metricas(d$y_obs, d$y_pred)))))
write.csv(met_s2, "resultados/m3/sensibilidad2_subgrupos.csv", row.names = FALSE)

cat("\nM3 terminado. Hiperparametros congelados:\n"); print(hp_seleccionados)
