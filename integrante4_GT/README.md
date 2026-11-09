# Proyecto: prediccion de dano en eventos de Hail y Thunderstorm Wind (TX, OK, KS, 2018-2023)

## Requisitos
- R version: <completar con R.version.string>
- Paquetes (version): readr, dplyr, tidyverse (incluye lubridate, stringr), ranger, <otros del equipo>

## Entradas
- `eventos.csv` (NOAA Storm Events) en la raiz del proyecto.

## Estructura
- `R/00_preparacion_comun.R`: limpieza, semilla (3407) y ventanas. Archivo congelado, no modificar.
- `R/04_m3_random_forest.R`: M3 random forest (ranger) y sensibilidades 1 y 2.
- `analisis.R`: ejecuta todos los scripts en orden.
- `resultados/`: salidas (CSV/RDS).

## Ejecucion (comando unico)
```
Rscript analisis.R
```

## Prueba en entorno limpio
| Fecha | Sistema | Comando | Tiempo | Resultado |
|---|---|---|---|---|
| | | | | |
