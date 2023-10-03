############################################
# S11 Hypothesis 4: Store-Family Forecast  #
############################################
# Fecha: 2023-10-03
# Creador: FR
# En esta hipótesis vamos a fabricar una serie de modelos por
# par store-familia. De esta forma maximizar la personalización con los 
# modelos que disponemos. No se va hacer crossvalidación dado que desconozco
# como hacerlo según model time. Pero se ocupara el test set de 15 días más 
# cercano a la fecha de término. 

# Cargando funciones ------------------------------------------------------
setwd("~/falrrema@gmail.com - Google Drive/My Drive/Proyectos DS/kgl_Store_Sales")
source("Helper.R")

# Seteando ambiente -------------------------------------------------------
# Específicos para el proyecto
packages <- c("plotly", "slider", "tidymodels", "modeltime", "modeltime.resample", "timetk",
              "tidyverse", "tidyquant", "bonsai", "vip", "doParallel")
requirements_libs(packages)

# Data --------------------------------------------------------------------
df_tbl <- read_csv("Data/kgl_h4_features.csv")
df_tbl <- df_tbl %>% 
  select(id:slice)
glimpse(df_tbl)

train <- df_tbl %>% filter(slice == "train") %>% select(-slice)
test <- df_tbl %>% filter(slice == "test") %>% select(-slice)

# Nested table ------------------------------------------------------------
nested_train <- train %>% 
  unite(store_family, store_nbr, family) %>% 
  extend_timeseries(
    .id_var        = store_family,
    .date_var      = date,
    .length_future = 16
  ) %>%
  nest_timeseries(
    .id_var        = store_family,
    .length_future = 16,
    .length_actual = 375
  ) %>%
  split_nested_timeseries(
    .length_test = 16
  )

extract_nested_train_split(nested_data_tbl, .row_id = 1) %>% 
  summarise(min_date = min(date),
            max_date = max(date),
            diff = max_date - min_date)
  
nested_train$.splits[[1]]$idx_train
  
extract_nested_train_split(nested_train, .row_id = 1) 
extract_nested_test_split(nested_train, .row_id = 1) %>% View
extract_nested_error_report(nested_train)
