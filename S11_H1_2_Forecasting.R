################################################
# S11 Hypothesis 1.2: by Store level forecast  #
################################################
# Fecha: 2023-10-03
# Creador: FR
# Una extensión de la hipótesis 1, pero esta vez se hacen modelos por store. Aprovechando
# los aprendizajes de hipotesis 2, 3 y 4. 

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

# Feature Engineering: Lag and rolling ventas  ----------------------------
# Genero un lag de ventas de hace 15 días y un rolling promedio anterior a eso de una semana
df_agg <- df_tbl %>%
  group_by(date, store_nbr) %>%
  summarise(sales = sum(sales),
            onpromotion = sum(onpromotion)) %>%
  ungroup() %>% 
  left_join(df_tbl %>% select(date, month:is_holiday), by = "date")


df_tbl <- df_tbl %>% 
  group_by(store_nbr, family) %>% 
  mutate(sales_15d = lag(sales, 15),
         sales_15d_r7d = slide_dbl(sales_15d, mean, .before = 6, .after = 0, .complete = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(sales_15d) & !is.na(sales_15d_r7d))


train <- df_tbl %>% filter(slice == "train") %>% select(-slice)
test <- df_tbl %>% filter(slice == "test") %>% select(-slice)
