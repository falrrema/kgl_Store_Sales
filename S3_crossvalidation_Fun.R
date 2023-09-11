############################
# Crossvalidation function #
############################
# Fecha: 2023-08-11
# Creador: FR

# Cargando funciones ------------------------------------------------------
source("analytics_inversiones/Funciones_Compartidas/helper.R")

# Seteando ambiente -------------------------------------------------------
# Específicos para el proyecto
# sc <- SparkR::sparkR.session(master="local")
datawrang_libs() # cargando librerias de datawrangling
ml_libs() # cargando librarias para ML

# Data --------------------------------------------------------------------
df_list <- read_rds("Data/df_list_kgl.RDS")
train <- df_list$train

# Desarrollando función Crossvalidación -----------------------------------
# Me estoy basando en la libreria ModelTime
ventana_train <- 150
ventana_test <- 15
splits <- sliding_period (
  data = train, 
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

