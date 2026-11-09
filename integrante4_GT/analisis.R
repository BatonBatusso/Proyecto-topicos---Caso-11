# Ejecuta todo el analisis en orden. Uso:  Rscript analisis.R   (desde la raiz del proyecto)
stopifnot(file.exists("eventos.csv"))
scripts <- c("R/00_preparacion_comun.R",
             "R/01_m1.R", "R/02_m2.R",              # ajustar nombres reales de los companeros
             "R/04_m3_random_forest.R")
scripts <- scripts[file.exists(scripts)]
t_ini <- Sys.time()
for (s in scripts) {
  cat("\n>>> Ejecutando", s, "\n")
  t0 <- Sys.time()
  source(s, echo = FALSE)
  cat(sprintf("<<< %s terminado en %.1f s\n", s, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
cat(sprintf("\nTiempo total: %.1f min\n", as.numeric(difftime(Sys.time(), t_ini, units = "mins"))))
