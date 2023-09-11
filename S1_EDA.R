###############################################
# EDA - Store Sales - Time Series Forecasting #
###############################################
# Fecha: 2023-07-28
# Creador: FR

# Objetivo ----------------------------------------------------------------
# El objetivo es comenzar a explorar el data set de la competencia de kaggle
# Queremos responder las siguientes preguntas:
# ¿Cuánta información tenemos? Días de información.
# ¿Cuántos stores tenemos? 
# ¿Tenemos data para todos los stores en el periodo del tiempo? Completeness.
# ¿Cuántas categorías existen de productos? 
# ¿Existe una categoría sin ventas en toda la data? Completeness.
# ¿Cuál es el tiempo del test set?

# Cargando funciones ------------------------------------------------------
options(scipen = 9999)
source("analytics_inversiones/Funciones_Compartidas/helper.R")

delta_to_tibble <- function(path) {
  df <- read.df(path, source="delta")
  df_local <- collect(df)
  as_tibble(df_local)
}

# Seteando ambiente -------------------------------------------------------
# # Específicos para el proyecto
library(SparkR)
library(lubridate)
sc <- sparkR.session(master="local")
sq <- sparkR.session(sc)

connect_libs()
datawrang_libs()

# Data --------------------------------------------------------------------
transactions <- delta_to_tibble("dbfs:/user/hive/warehouse/analytics_inversiones.db/kgl_transactions")
oil <- delta_to_tibble("dbfs:/user/hive/warehouse/analytics_inversiones.db/kgl_oil")
holidays <- delta_to_tibble("dbfs:/user/hive/warehouse/analytics_inversiones.db/kgl_holidays_events")
stores <- delta_to_tibble("dbfs:/user/hive/warehouse/analytics_inversiones.db/kgl_stores")
train <-  delta_to_tibble("dbfs:/user/hive/warehouse/analytics_inversiones.db/kgl_train")
test <-  delta_to_tibble("dbfs:/user/hive/warehouse/analytics_inversiones.db/kgl_test")
sparkR.stop()

# df_list <- map(list.files("Data/"), function(x) read_csv(paste0("Data/", x)))
# names(df_list) <- gsub("\\.csv", "", list.files("Data/"))

# df_list <- list(transactions = transactions,
#                 oil = oil,
#                 holidays = holidays,
#                 stores = stores,
#                 train = train,
#                 test = test)
#write_rds(df_list, "Data/df_list_kgl.RDS")

# ¿Cuánta información tenemos? Días de información. -----------------------
# Se va a analizar el archivo Train
glimpse(train) # 3 MM de filas y 6 columnas

# ¿Cuantas fechas? 4.6 años aprox
train %>% 
  count(date) %>% 
  summarise(date_min = min(date),
            date_max = max(date),
            n_max = max(n),
            n_min = min(n)) %>% 
  mutate(ventana_yr = time_length(difftime(date_max, date_min), "years"))

# Fechas de corrido? Las navidades no hay datos
tibble(vector_fecha = seq(as.Date("2013-01-01"), as.Date("2017-08-15"), by = "day")) %>% 
  left_join(train %>% 
              count(date), by = c("vector_fecha" = "date")) %>% 
  filter(is.na(n))

# ¿Cuántos stores tenemos?  ---------------------------------------------
train %>% 
  count(store_nbr) %>% 
  fun_print()

# ¿Cuántas categorías existen de productos? -----------------------------
train %>% 
  count(family) %>% 
  fun_print()


# ¿Tenemos data para todos los stores en el periodo del tiempo?  ----------
# Completeness.
train %>% 
  count(store_nbr, date) %>% 
  mutate(n_familia_ok = ifelse(n == 33, 1, 0)) %>% 
  filter(n_familia_ok == 0)

# Filtramos por venta
train %>% 
  filter(sales > 0) %>% 
  count(store_nbr, date) %>%
  group_by(store_nbr) %>% 
  summarise(date_min = min(date),
            date_max = max(date),
            n_date = n_distinct(date)) %>% 
  View()

# ¿Existe una categoría sin ventas en toda la data? ---------------------
# Completeness
train %>% 
  group_by(family) %>% 
  summarise(total = sum(sales),
            total_zero = sum(ifelse(sales == 0, 1, 0)),
            n = n()) %>% 
  mutate(pct = total_zero/n) %>% 
  arrange(desc(pct)) %>% 
  fun_print()

# ¿Cuál es el tiempo del test set? --------------------------------------
test %>% 
  count(date) %>% 
  summarise(date_min = min(date),
            date_max = max(date),
            n_max = max(n),
            n_min = min(n)) %>% 
  mutate(ventana_test = time_length(difftime(date_max, date_min), "day"))


