# Ejecuta todo el analisis en orden.
stopifnot(file.exists("eventos.csv"))
scripts <- c("preparacion_base_datos.R",
             "integrante1_MB/Análisis inicial.R",
             "integrante1_MB/Creación Paquete Común.R",
             "integrante1_MB/descripcion_datos.R",
             "integrante1_MB/resumen_tablas_informe.R",
             "integrante1_MB/Función de referencia.R",
             "integrante1_MB/analisis_incertidumbre.R",
             "integrante1_MB/incertidumbre_diagnostico.R",
             "integrante1_MB/resumen_tablas_informe.R",
             "integrante2_PE/M2_patito.R", 
             "integrante3_LM/archivo_inicial.R",             
             "integrante4_GT/04_m3_random_forest.R",
             "integrante1_MB/comparacion_modelos_new.R")
scripts <- scripts[file.exists(scripts)]
t_ini <- Sys.time()

for (s in scripts) {
  cat("\n>>> Ejecutando", s, "\n")
  t0 <- Sys.time()
  
  # Captura de errores para indicar qué archivo falló
  tryCatch({
    source(s, echo = FALSE)
  }, error = function(e) {
    stop(sprintf("\n[ERROR] La ejecución falló en el archivo: '%s'\nDetalle del error: %s", s, conditionMessage(e)), call. = FALSE)
  })
  
  cat(sprintf("<<< %s terminado en %.1f s\n", s, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

cat(sprintf("\nTiempo total: %.1f min\n", as.numeric(difftime(Sys.time(), t_ini, units = "mins"))))