#############################################
# S8 Hypothesis 1: Store level forecast EDA #
#############################################
# Fecha: 2023-09-11
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

# Feature engineering -----------------------------------------------------
# Oil delta precios
oil <- oil %>%
  mutate(delta_oil = dcoilwtico - lag(dcoilwtico))

hdays <- holidays %>%
  select(date, city = locale) %>%
  mutate(holiday = 1)

# Vamos a simplificar el problema buscando predecir el store completo
train_df <- train %>%
  group_by(date, store_nbr) %>%
  summarise(sales = sum(sales),
            onpromotion = sum(onpromotion)) %>%
  ungroup() %>%
  left_join(transactions, by = c("date", "store_nbr")) %>%
  left_join(oil, by = "date") %>%
  left_join(stores, by = "store_nbr") %>%
  left_join(hdays, c("date", "city")) %>%
  mutate(holiday = coalesce_0(holiday)) %>%
  select(-city, -state)

