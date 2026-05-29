
# R/app.R  (app dentro de la carpeta R)

library(shiny)
library(DT)

library(dplyr)
library(tidyr)
library(stringr)
library(stringi)
library(tibble)
library(purrr)
library(ggplot2)



source("R/functions_data.R")
source("R/app_ui.R")
source("R/app_server.R")
source("R/app_run.R")
source("R/functions_plot.R")



app_run()


