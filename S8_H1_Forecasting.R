#########################################
# S8 Hypothesis 1: Store level forecast #
#########################################
# Fecha: 2023-09-12
# Creador: FR
# La idea de fondo es reducir la complejidad del forecasting a nivel
# store-family y reducirlo a solo store level. En total serían 54 forecasteo. 
# Se hara posteriormente un top-down approach para las familias, para eso 
# hay que investigar que tan volatil son los % de ventas del total del store por
# familia. Si esta volatilidad es baja entonces es posible forecastear con tranquilidad
# y tener resultados aceptables. 

# Cargando funciones ------------------------------------------------------
setwd("~/falrrema@gmail.com - Google Drive/My Drive/Proyectos DS/kgl_Store_Sales")
source("Helper.R")

# Seteando ambiente -------------------------------------------------------
# Específicos para el proyecto
# sc <- SparkR::sparkR.session(master="local")
datawrang_libs() # cargando librerias de datawrangling
ml_libs() # cargando librarias para ML
library(plotly)
library(slider)

# Data --------------------------------------------------------------------
df_list <- read_rds("Data/df_list_kgl.RDS")
train <- df_list$train
transactions <- df_list$transactions
oil <- df_list$oil
holidays <- df_list$holidays_events
stores <- df_list$stores
test <- df_list$test