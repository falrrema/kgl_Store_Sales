############################
# Crossvalidation function #
############################
# Fecha: 2023-08-11
# Creador: FR

# Cargando funciones ------------------------------------------------------
setwd("~/falrrema@gmail.com - Google Drive/My Drive/Proyectos DS/kgl_Store_Sales")
source("Helper.R")
#source("analytics_inversiones/Funciones_Compartidas/helper.R")

# Seteando ambiente -------------------------------------------------------
# Específicos para el proyecto
# sc <- SparkR::sparkR.session(master="local")
# datawrang_libs() # cargando librerias de datawrangling
#ml_libs() # cargando librarias para ML
packages <- c("plotly", "tidymodels", "modeltime", "modeltime.resample", "timetk",
              "tidyverse", "tidyquant")
requirements_libs(packages)

# Data --------------------------------------------------------------------
df_list <- read_rds("Data/df_list_kgl.RDS")
train <- df_list$train

train_df <- train %>%
  group_by(date, store_nbr) %>%
  summarise(sales = sum(sales),
            onpromotion = sum(onpromotion)) %>%
  ungroup()

train_df %>% 
  summarise(ini = min(date),
            fin = max(date))

# Desarrollando función Crossvalidación -----------------------------------
# Me estoy basando en la libreria ModelTime
ventana_train <- 150
ventana_test <- 15
splits <- sliding_period (
  data = train_df, 
  index = "date", 
  period = "day", 
  lookback = ventana_train, # una prueba
  assess_start = 1,
  assess_stop = ventana_test + 1, 
  step = ventana_train + ventana_test + 1 # Saltarse 165 días para el siguiente split
)

# Me genera un total de 14 splits con 100 días
# Revisemos data por split
# OJO recien hay data de promociones desde el split 4
split_res <- splits %>%
  tk_time_series_cv_plan() %>% 
  group_by(.id, .key) %>% 
  summarise(min_date = min(date),
            max_date = max(date),
            n = n(),
            n_stores = n_distinct(store_nbr),
            sum_sales = sum(sales),
            sum_prom = sum(onpromotion)) %>% 
  mutate(diff = max_date - min_date) %>% 
  fun_print()

# Como referencia las fechas del dataset original
train %>% 
  summarise(min_date = min(date),
            max_date = max(date)) %>% 
  mutate(diff = max_date - min_date)

# Visualizemos el CV
plt <- splits %>%
  tk_time_series_cv_plan() %>% 
  group_by(date) %>% 
  summarise(sales_sum = sum(sales)) %>% 
  ggplot(aes(x = date, y = sales_sum)) +
  geom_line() +
  geom_rect(data = split_res %>% filter(.key == "testing"), 
            mapping = aes(xmin=min_date, 
                          xmax=max_date, 
                          ymin=0, 
                          ymax=1.5e6), 
            fill='salmon', alpha=0.9, inherit.aes = FALSE) +
  theme_bw() +
  xlab("") + ylab("Venta diaria total") +
  scale_y_continuous(labels = label_number(suffix = " K", scale = 1e-3))
ggplotly(plt)  

# Probando para nested forecasting ----------------------------------------
train_df <- train %>%
  group_by(date, store_nbr) %>%
  summarise(sales = sum(sales),
            onpromotion = sum(onpromotion)) %>%
  ungroup()

splits2 <- train_df %>%
  time_series_cv(
    date_var    = date, 
    initial     = ventana_train + 1,
    assess      = ventana_test + 1,
    skip        = ventana_train + ventana_test,
    cumulative  = FALSE
  )

split_res2 <- splits2 %>%
  tk_time_series_cv_plan() %>% 
  group_by(.id, .key) %>% 
  summarise(min_date = min(date),
            max_date = max(date),
            n = n(),
            n_stores = n_distinct(store_nbr),
            sum_sales = sum(sales),
            sum_prom = sum(onpromotion)) %>% 
  mutate(diff = max_date - min_date) %>% 
  fun_print()

split_res %>% filter(.id == "Slice10")
split_res2 %>% filter(.id == "Slice01")


nested_data_tbl <- train_df %>%
  select(-onpromotion) %>% 
  
  # 1. Extending: We'll predict 15 days to the future.
  extend_timeseries(
    .id_var        = store_nbr,
    .date_var      = date,
    .length_future = ventana_test + 1
  ) %>%
  
  # 2. Nesting: We'll group by id, and create a future dataset
  #    that forecasts 52 weeks of extended data and
  #    an actual dataset that contains 104 weeks (2-years of data)
  nest_timeseries(
    .id_var        = store_nbr,
    .length_future = ventana_test + 1,
    .length_actual = n_distinct(train_df$date) # 1684 dias
  ) %>%
  
  # 3. Splitting: We'll take the actual data and create splits
  #    for accuracy and confidence interval estimation of 52 weeks (test)
  #    and the rest is training data
  split_nested_timeseries(
    .length_test = NULL,
    initial     = ventana_train + 1,
    assess      = ventana_test + 1,
    skip        = ventana_train + ventana_test,
    cumulative  = FALSE
  )

nested_data_tbl$.splits[[1]]$idx_train

