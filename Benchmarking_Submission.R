#########################################
# S8 Hypothesis 1: Store level forecast #
#########################################
# Fecha: 2023-10-02
# Creador: FR

# Cargando funciones ------------------------------------------------------
setwd("~/falrrema@gmail.com - Google Drive/My Drive/Proyectos DS/kgl_Store_Sales")
source("Helper.R")

# Seteando ambiente -------------------------------------------------------
# Específicos para el proyecto
packages <- c("plotly", "slider", "tidymodels", "modeltime", "modeltime.resample", "timetk",
              "tidyverse", "tidyquant", "bonsai", "vip", "doParallel")
requirements_libs(packages)

# Data --------------------------------------------------------------------
# Formateamos la data para poder analizarla completamente
train <- read_csv("Data/train.csv")
h1 <- read_csv("Data/H1_test_models.csv") %>% 
  gather(model, preds, h1_ENSEMBLE:h1_XGBOOST) %>% 
  mutate(ventana_train = 150) %>% 
  select(id, ventana_train, model, preds) 
h2 <- read_csv("Data/H2_test_models.csv") %>% 
  select(id, ventana_train, starts_with("h2")) %>% 
  gather(model, preds, h2_ENSEMBLE:h2_XGBOOST)
h3 <- read_csv("Data/H3_test_models.csv") %>% 
  gather(model, preds, h3_ENSEMBLE:h3_LIGHTGBM) %>% 
  mutate(ventana_train = 360) %>% 
  select(id, ventana_train, model, preds) 
h4 <- read_csv("Data/H4_test_models.csv") %>% 
  mutate(ventana_train = 360, 
         model = "h4_MULTIPLE",
         preds = exponenciador(preds)) %>% 
  select(id, ventana_train, model, preds) 

subs <- bind_rows(h1, h2, h3, h4) %>% 
  left_join(train, by = "id") %>% 
  select(id, date, store_nbr, family, ventana_train, model, sales, preds)

# Performance comparativo -------------------------------------------------
# Mejor modelo hasta ahora h2_lightgbm
subs %>% 
  group_by(ventana_train, model) %>% 
  summarise(n = n(),
            sales_sum = sum(sales),
            preds_sum = sum(preds),
            rmsle = rmsle_vec(truth = sales, estimate = preds)) %>% 
  arrange(rmsle) %>% fun_print()

# mejor modelo por Store-Familia
subs %>% 
  group_by(store_nbr, family, ventana_train, model) %>% 
  summarise(rmsle = rmsle_vec(truth = sales, estimate = preds)) %>% 
  group_by(store_nbr, family) %>% 
  filter(rmsle == min(rmsle)) %>% 
  ungroup() %>% 
  unite(store_family, store_nbr, family, remove = FALSE) %>% 
  filter(!duplicated(store_family)) %>% 
  arrange(desc(rmsle)) %>% 
  fun_print()

subs %>% 
  group_by(store_nbr, family, ventana_train, model) %>% 
  summarise(rmsle = rmsle_vec(truth = sales, estimate = preds)) %>% 
  ungroup() %>% 
  filter(store_nbr == 32, family == "LIQUOR,WINE,BEER") %>% 
  arrange(rmsle) %>% 
  fun_print()


# MetaEnsamblaje ----------------------------------------------------------
train <- subs %>% 
  select(id, model, preds) %>% 
  spread(model, preds)

