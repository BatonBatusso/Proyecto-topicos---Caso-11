# Trabajo final EYP3407 – Caso 11: Daños materiales en eventos meteorológicos (NOAA Storm Events)

Pontificia Universidad Católica de Chile · Facultad de Matemáticas · Departamento de Estadística
Curso: Tópicos Aplicados en Estadística (EYP3407) · Segundo semestre 2026
Profesor: Eloy Alvarado Narváez · Ayudante: Andrés Díaz

## 1. Integrantes y caso

| N.º | Integrante |
|---|---|
| 1 | Maximiliano Bustos Mesias |
| 2 | Patricio Espinoza Amaya |
| 3 | Lukas Mancilla Pezoa |
| 4 | Gastón Torres Sanhueza |

> **Nota sobre numeración (según el plan de trabajo):** Lukas Mancilla es el Integrante 2 (M1) y Patricio Espinoza el Integrante 3 (M2). Los nombres de las carpetas quedaron con los números cambiados (`integrante3_LM` contiene M1 y `integrante2_PE` contiene M2). Se mantuvieron así para no alterar rutas del código.

**Caso asignado:** `caso11` – Daño material (`DAMAGE_PROPERTY`, en dólares nominales) de eventos `Hail` y `Thunderstorm Wind` en Texas, Oklahoma y Kansas, 2018–2023. El análisis es retrospectivo: no es un pronóstico de daños ni un análisis causal.

## 2. Datos

- **Paquete utilizado:** `caso11_noaa_tormentas.zip` (obtenido desde Canvas; copia preparada por el curso el 12 de octubre de 2026).
- **Productor original:** NOAA / NCEI, Storm Events Database.
- **Enlace oficial:** https://www.ncei.noaa.gov/pub/data/swdi/stormevents/csvfiles/
- **Archivos originales (tabla de detalles):** `StormEvents_details-ftp_v1.0_dAAAA_cFECHA.csv.gz`, con versión `c20260323` para 2018, 2019, 2020, 2021 y 2023, y `c20260625` para 2022. Diccionario: `Storm-Data-Bulk-csv-Format.pdf`.
- **Subconjunto entregado:** 26.519 eventos agrupados en 3.490 episodios, 51 variables, años 2018–2023, Texas, Oklahoma y Kansas, solo `Hail` y `Thunderstorm Wind`.
- **Ubicación en el repositorio:** el paquete ya está descomprimido y los archivos de entrada están **en la raíz del repo** (no en `datos/`): `eventos.csv`, `LEEME.txt` y `Storm-Data-Bulk-csv-Format.pdf`. No hace falta descargar nada de nuevo.
- **Condiciones de uso:** datos públicos de NOAA/NCEI. Los portales pueden revisar sus datos después de la descarga, por lo que se conserva la copia entregada.
- **Archivos derivados:** la base de desarrollo, la reserva de prueba y las particiones (`.rds`) **no vienen precalculados**: los genera el código al ejecutarse.

**Decisiones principales sobre los datos:** `DAMAGE_PROPERTY` ausente se excluye (no se imputa ni se asume cero); los ceros registrados se conservan; los sufijos K, M y B se convierten a dólares; `MAGNITUDE` se separa por unidad física (granizo en pulgadas, viento en nudos); se excluyen narrativas y variables de consecuencias (cultivos, lesiones, muertes); los extremos válidos se conservan; los eventos de un mismo `EPISODE_ID` permanecen siempre juntos.

## 3. Entorno

- **Lenguaje:** R **4.4.1** (2026-03-11, Windows, ucrt).
- **Gestión de paquetes:** Este proyecto utiliza `renv` para garantizar la reproducibilidad de las versiones de los paquetes. 
- **Paquetes principales utilizados:** `glmnet`, `dplyr`, `tidyr`, `ggplot2`, `readr`, `stringr`, `lubridate`, `tidyverse`, `scales`, `rpart`, `ranger`.

## 4. Ejecución

Para ejecutar correctamente el análisis completo de manera reproducible, se deben seguir estos pasos:

1. Abrir el archivo **`Proyecto-topicos---Caso-11.Rproj`** en RStudio. Esto asegurará que el directorio de trabajo (working directory) sea el correcto.
2. En la consola de R, ejecutar el siguiente comando para restaurar los paquetes exactos utilizados en el entorno de desarrollo:
   ```r
   renv::restore()
   ```
3. Ejecutar el script principal `analisis.R`. Esto puede hacerse desde la terminal de R o consola con el siguiente comando:
   ```bash
   Rscript --vanilla analisis.R
   ```

El archivo `analisis.R` se encarga de ejecutar orquestadamente los siguientes scripts en este orden exacto:
1. `"preparacion_base_datos.R"`
2. `"integrante1_MB/Análisis inicial.R"`
3. `"integrante1_MB/Creación Paquete común.R"`
4. `"integrante1_MB/descripcion_datos.R"`
5. `"integrante1_MB/resumen_tablas_informe.R"`
6. `"integrante1_MB/Función de Referencia.R"`
7. `"integrante1_MB/analisis_incertidumbre.R"`
8. `"integrante1_MB/incertidumbre_diagnostico.R"`
9. `"integrante1_MB/resumen_tablas_informe.R"`
10. `"integrante2_PE/M2_patito.R"`
11. `"integrante3_LM/archivo_inicial.r"`
12. `"integrante4_GT/04_m3_random_forest.R"`
13. `"integrante1_MB/comparacion_modelos_new.R"`

## 5. Estructura del repositorio

El repositorio está organizado respetando las carpetas de trabajo de cada integrante y las salidas consolidadas.

```text
Proyecto-topicos---Caso-11/
├── .Rproj.user/
├── datos_comunes/                  # Bases de datos derivadas congeladas (.rds)
├── datos_prueba_sellado/
├── integrante1_MB/                 # Integrante 1 (Maximiliano): Referencias, datos, incertidumbre
│   ├── Análisis inicial.R
│   ├── analisis_incertidumbre.R
│   ├── archivo_inicial.R
│   ├── comparacion_modelos_new.R
│   ├── Creación Paquete común.R
│   ├── descripcion_datos.R
│   ├── Función de Referencia.R
│   ├── incertidumbre_diagnostico.R
│   └── resumen_tablas_informe.R
├── integrante2_PE/                 # Integrante 3 (Patricio): M2 Árbol podado
│   ├── resultados_m2/
│   ├── archivo_inicial.r
│   └── M2_patito.R
├── integrante3_LM/                 # Integrante 2 (Lukas): M1 Elastic Net
│   ├── Resultados/
│   ├── archivo_inicial.r
│   ├── funcion_final_m1.R
│   ├── integrante2_diseno_m1.html
│   └── integrante2_diseno_m1.Rmd
├── integrante4_GT/                 # Integrante 4 (Gastón): M3 Random Forest y Reproducibilidad
│   ├── resultados/
│   ├── 04_m3_random_forest.R
│   ├── archivo_inicial.R
│   ├── m3_random_forest.R
│   └── README.md
├── pruebas/
├── renv/                           # Archivos de entorno virtual de R
├── resultados/                     # Salidas de tablas y modelos consolidados
├── salidas/                        # Gráficos y figuras exportadas
├── analisis_exporatorio.r
├── analisis.R                      # Script integrador principal (Punto de entrada)
├── eventos.csv                     # Dataset original (Input)
├── LEEME.txt                       # Documentación original del dataset
├── preparacion_base_datos.R
├── preparacion_base_datos_respaldo.R
├── Proyecto-topicos---Caso-11.Rproj # Archivo de proyecto RStudio
├── prueba 1.R
├── renv.lock                       # Lockfile con versiones exactas de dependencias
└── Storm-Data-Bulk-csv-Format.pdf  # Diccionario de datos original
```

**Formato de las predicciones y salidas:** 
Las predicciones de cada modelo en desarrollo y prueba se generan y almacenan en las respectivas carpetas `resultados/` de cada integrante (en `.rds` o `.csv`). Luego, el script integrador unifica estas columnas manteniendo los nombres de identificación originales (`EVENT_ID`, `EPISODE_ID`) para hacer la evaluación comparativa final en la carpeta central de `resultados/` y `salidas/`.

## 6. Diseño del análisis (resumen)

- **Referencias:** mediana global y mediana por `EVENT_TYPE` (con respaldo global).
- **Modelos:** M1 regresión elastic net (`glmnet`), M2 árbol de regresión podado (`rpart`) y M3 random forest (`ranger`). Cada uno se ajusta con respuesta original y con `log(1 + daño)`.
- **Validación:** ventana creciente en desarrollo (2018–2021): V1 entrena 2018 y valida 2019; V2 entrena 2018–2019 y valida 2020; V3 entrena 2018–2020 y valida 2021. Máximo 20 configuraciones por familia. Los episodios no se separan entre entrenamiento y validación.
- **Reserva de prueba:** 2022–2023, evaluada una sola vez, después de fijar el procedimiento.
- **Métricas:** MAE (principal) y RMSE, en dólares; diagnóstico separado para daño cero, positivo moderado y extremo, y concentración del error en el 1 % y 5 % de los eventos más destructivos.
- **Incertidumbre:** bootstrap por `EPISODE_ID` (B = 2000) de la diferencia de MAE entre cada modelo y la referencia.
- **Sensibilidades:** (1) respuesta original frente a `log(1 + Y)`; (2) tipo y calidad de medición (Hail, viento medido, viento estimado).

## 7. Informe y entrega

El informe (`informe.pdf`) se redactó en LaTeX en Overleaf y la presentación en `presentacion.pdf`. Las tablas descriptivas, de desempeño y figuras de diagnóstico que se citan en los documentos son generadas automáticamente al ejecutar `analisis.R` y se depositan en las carpetas `resultados/` y `salidas/`.

## 8. Control de ejecución

- **Semilla:** 3407 (fijada globalmente en `preparacion_base_datos.R` y en scripts de modelado).
- **Sistema operativo de prueba:** Windows 11 (64-bit).
- **Recursos empleados:** 16 GB RAM, procesador multi-núcleo estándar.
- **Tiempo aproximado de ejecución completa:** ~10-15 minutos (dependiendo de la capacidad de procesamiento paralelo en la búsqueda de hiperparámetros de M3).

### Prueba desde copia limpia

| Campo | Registro |
|---|---|
| Fecha | 11 de noviembre de 2026 |
| Computador / sistema | PC local, Windows 11, R 4.4.1 |
| Comando | `renv::restore()` seguido de `Rscript --vanilla analisis.R` |
| Resultado | Ejecución exitosa de inicio a fin. Generación correcta de sub-bases, modelos ajustados, integraciones y exportación de gráficos a la carpeta de salidas. |

## 9. Contribuciones del equipo

| Integrante | Aporte |
|---|---|
| **Maximiliano Bustos (1)** | Paquete común (base de desarrollo, reserva de prueba y particiones). Auditoría de datos y tratamiento de NA. Referencias (mediana global y por tipo). Tablas de construcción de la muestra y de variables incluidas/excluidas. Figuras descriptivas. Bootstrap por episodio, concentración del error y diagnóstico por subgrupos. Redacción de problema, datos y referencia. |
| **Lukas Mancilla (2)** | M1: elastic net con `glmnet` (original y log1p), preprocesamiento dentro del entrenamiento, búsqueda de hiperparámetros y función final. Diseño de validación: diagrama temporal, justificación de mantener juntos los episodios y control de filas comunes entre modelos. Redacción de diseño de evaluación y M1. |
| **Patricio Espinoza (3)** | M2: árbol podado con `rpart` (original y log1p), selección por desempeño, estabilidad y parsimonia, interpretación del árbol final y función final. Integración de resultados de desarrollo: tabla comparativa y Sensibilidad 1. Redacción de M2 y de la comparación de complejidad e interpretabilidad. |
| **Gastón Torres (4)** | M3: random forest con `ranger` (original y log1p), búsqueda de hiperparámetros, importancia de variables y función final. Sensibilidad 2 (tipo y calidad de medición). `analisis.R`, estructura del repositorio y prueba desde copia limpia. Redacción de M3, Sensibilidad 2 y reproducibilidad. |

## 10. Declaración de uso de inteligencia artificial

- **Herramienta:** Claude (Anthropic) / Gemini.
- **Fechas:** 4 al 11 de noviembre de 2026.
- **Quiénes:** los cuatro integrantes.
- **Propósito y partes afectadas:** generación de código de R, estructuración del repositorio reproducible, revisión ortográfica del informe y redacción del README.
- **Comprobaciones:** el código se ejecutó y se revisó mediante auditorías iterativas. El equipo verificó los resultados y la concordancia de la lógica estadística implementada.
- La defensa se realiza sin asistencia de IA.