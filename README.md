# TFG Shiny Pádel

## Descripción

Este proyecto corresponde al Trabajo Fin de Grado desarrollado por **Hugo Leon** y consiste en una aplicación web interactiva implementada con **R Shiny** para el análisis y visualización de datos históricos de la competición **Four Seasons Pádel**.

La aplicación permite transformar información almacenada en hojas de cálculo Excel en una plataforma de análisis accesible e intuitiva, proporcionando estadísticas, rankings, visualizaciones interactivas y simulaciones predictivas de enfrentamientos entre parejas de jugadores.

---

## Funcionalidades principales

* Visualización de rankings de jugadores.
* Análisis de la evolución temporal del rendimiento.
* Consulta de estadísticas individuales.
* Comparación de métricas entre jugadores.
* Filtrado por temporada, trimestre y jornada.
* Visualización interactiva mediante gráficos y tablas dinámicas.
* Simulación de enfrentamientos entre parejas.
* Estimación probabilística de resultados utilizando simulación Monte Carlo.

---

## Tecnologías utilizadas

* **R**
* **Shiny**
* **dplyr**
* **tidyr**
* **ggplot2**
* **plotly**
* **DT**
* **readxl**
* **stringr**
* **purrr**
* **lubridate**

---

## Estructura del proyecto

```text
tfg_padel_shiny/
│
├── R/
│   ├── app.R
│   ├── app_run.R
│   ├── app_ui.R
│   ├── app_server.R
│   ├── functions_data.R
│   └── functions_plot.R
│
├── data/
│   └── Archivos .rds generados por el proceso ETL
│
└── README.md
```

### Componentes principales

| Archivo          | Descripción                                          |
| ---------------- | ---------------------------------------------------- |
| app.R            | Punto de entrada principal de la aplicación.         |
| app_run.R        | Inicialización y lanzamiento de la aplicación Shiny. |
| app_ui.R         | Definición de la interfaz gráfica de usuario.        |
| app_server.R     | Implementación de la lógica reactiva del servidor.   |
| functions_data.R | Procesamiento de datos y generación de métricas.     |
| functions_plot.R | Construcción de gráficos y visualizaciones.          |

---

## Requisitos

Para ejecutar la aplicación es necesario disponer de:

* R 4.0 o superior.
* RStudio.
* Paquetes indicados en la sección de dependencias.

---

## Instalación de dependencias

Ejecutar en la consola de R:

```r
install.packages(c(
  "shiny",
  "DT",
  "dplyr",
  "tidyr",
  "stringr",
  "stringi",
  "tibble",
  "purrr",
  "ggplot2",
  "plotly",
  "readxl",
  "lubridate"
))
```

---

## Ejecución de la aplicación

Situarse en la carpeta raíz del proyecto y ejecutar:

```r
setwd("C:/Users/Hugo/Desktop/TFG/tfg_padel_shiny")
source("R/app.R")
```

Durante la inicialización se cargan los conjuntos de datos procesados, se construyen las estructuras analíticas necesarias y se inicia automáticamente la aplicación Shiny.

---

## Datos

La aplicación utiliza conjuntos de datos procesados en formato `.rds`, generados previamente a partir de los ficheros Excel históricos de la competición mediante un proceso ETL de extracción, limpieza y transformación de datos.

---

## Autor

**Hugo Leon**

Trabajo Fin de Grado
Grado en Ciencia e Ingeniería de Datos
Universidad Rey Juan Carlos

---

## Licencia

Proyecto desarrollado con fines académicos como Trabajo Fin de Grado.
