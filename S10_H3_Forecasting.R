####################################################
# S10 Hypothesis 3: Complete store-family forecast #
####################################################
# Fecha: 2023-09-29
# Creador: FR
# Hipotesis 3
# Buscando mantener la estrategia de entrenar un modelo global y 
# no multiples modelos por jerarquía, se va transformar el tablón 
# de entrenamiento y generar features que permitan capturar las 
# relaciones stores-familias al convertir estas caracteristicas 
# en variables dummies. Se mejorará la determinación de feriados que se 
# hizo parcialmente en H1 y H2, y se dummificarán también variables de 
# fechas y carácterísticas de tiendas, dado que antes se entrenaron muchas 
# de estas variables como enteros continuos.

# Cargando funciones ------------------------------------------------------
setwd("~/falrrema@gmail.com - Google Drive/My Drive/Proyectos DS/kgl_Store_Sales")
source("Helper.R")

# Seteando ambiente -------------------------------------------------------
# Específicos para el proyecto
packages <- c("plotly", "slider", "tidymodels", "modeltime", "modeltime.resample", "timetk",
              "tidyverse", "tidyquant", "bonsai", "vip", "doParallel")
requirements_libs(packages)

# Data --------------------------------------------------------------------
df_tbl <- read_csv("Data/kgl_tbl_dummy.csv")
df_tbl <- df_tbl %>% 
  select(id:f_SEAFOOD)
glimpse(df_tbl)

train <- df_tbl %>% filter(slice == "train") %>% select(-slice)
test <- df_tbl %>% filter(slice == "test") %>% select(-slice)
rm(df_tbl)

# Para extraer columnas
df_train <- read_csv("Data/train.csv")

# Crossvalidation ---------------------------------------------------------
# Sliding window train and test set
ventana_train <- 360
ventana_test <- 15
splits <- time_series_cv(
  data        = train,
  date_var    = date, 
  initial     = ventana_train,
  assess      = ventana_test,
  skip        = ventana_train + ventana_test + 1,
  cumulative  = FALSE)

# Estamos OK
# split_res <- splits %>%
#   tk_time_series_cv_plan() %>% 
#   group_by(.id, .key) %>% 
#   summarise(min_date = min(date),
#             max_date = max(date),
#             n = n(),
#             # n_stores = n_distinct(store_nbr),
#             sum_sales = sum(sales),
#             sum_prom = sum(onpromotion)) %>% 
#   mutate(diff = max_date - min_date) 

split_list <- map2(splits$splits, splits$id, function(split, slice) {
  train <- split$data[split$in_id,]
  test <- split$data[split$out_id,]
  train$type <- "train"
  test$type <- "test"
  bind_rows(train, test) %>% 
    mutate(id = slice)
})

split_list %>% 
  map(function(split) {
    split %>% 
      group_by(type) %>%
      summarise(min_date = min(date),
                max_date = max(date),
                n = n(),
                # n_stores = n_distinct(store_nbr),
                sum_sales = sum(sales),
                sum_prom = sum(onpromotion)) %>%
      mutate(diff = max_date - min_date)
  })

# Recipes -----------------------------------------------------------------
template <- training(splits$splits[[1]]) # template for recipe

# Recipes for models with extra regresors
recipe_spec <- recipe(sales ~ ., template) %>% 
  step_scale(oil_price) %>% 
  step_rm(id)
recipe_spec %>% prep %>% bake(NULL) %>% glimpse
recipe_spec %>% prep %>% bake(new_data = test) %>% glimpse

# Modeling and fit --------------------------------------------------------
rm(split_list);gc() # limpiando entorno

# Model 1: lasso
wf_glmnet <- workflow() %>% 
  add_model(linear_reg(penalty = 0.001, mixture = 1) %>% 
              set_engine("glmnet")) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>% 
  fit(training(splits$splits[[1]]))
gc()

# Model 2: Prophet Boost
# wf_prophet_xgboost <-  workflow() %>% 
#   add_model(prophet_boost(seasonality_yearly = "auto",
#                           seasonality_weekly = "auto",
#                           seasonality_daily = "auto", 
#                           season = 'multiplicative',
#                           trees = 5000,
#                           tree_depth = 6, 
#                           learn_rate = 0.01, 
#                           min_n = 5) %>% 
#               set_engine("prophet_xgboost")) %>% 
#   add_recipe(recipe_spec) %>% 
#   fit(training(splits$splits[[1]]))
# gc()

# Model 3: lightgbm
wf_ltboost <- workflow() %>% 
  add_model(boost_tree(mode = "regression",
                       trees = 5000, 
                       tree_depth = 6, 
                       learn_rate = 0.01, 
                       min_n = 5) %>% 
              set_engine("lightgbm"))  %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>% 
  fit(training(splits$splits[[1]]))
gc()

# Model Time Table and Refit ----------------------------------------------
model_tbl <- modeltime_table(
  wf_glmnet,
  # wf_prophet_xgboost,
  wf_ltboost
)

# Resampling Refit
# cluster <- makeCluster(detectCores())
# registerDoParallel(cluster)
resample_results <- model_tbl %>%
  modeltime_fit_resamples(
    resamples = splits,
    control   = control_resamples(verbose = TRUE)
  )
# stopCluster(cluster)

# Results - RMSLE ---------------------------------------------------------
resample_results %>%
  plot_modeltime_resamples(
    .summary_fn  = mean, 
    .metric_set  = rmse,
    .point_size  = 3,
    .interactive = TRUE
  )


# Refitting ---------------------------------------------------------------
# Forecasting on split of test
# hare dos ensamblajes
# ENSEMBLE1 = promedio todos los modelos
forecast_train <- model_tbl %>% 
  modeltime_forecast(new_data = testing(splits$splits[[1]]), keep_data = TRUE) %>% 
  left_join(df_train %>% select(id, store_nbr, family), by = "id") %>% 
  select(id, date, store_nbr, family, model = .model_desc, sales, preds = .value) %>% 
  spread(model, preds) %>% 
  rowwise() %>%
  mutate(ENSEMBLE = mean(c_across(GLMNET:LIGHTGBM))) %>% 
  gather(model, preds, GLMNET:ENSEMBLE) %>% 
  mutate(preds = exponenciador(preds),
         sales = exponenciador(sales),
         preds = ifelse(preds < 0, 0, preds)) # hay sales negativos

# Calculando RMSLE
# LIGHTGBM el mejor modelo seguido por el ensamblaje
forecast_train %>% 
  group_by(model) %>% 
  summarise(rmsle = rmsle_vec(truth = sales, estimate = preds)) # %>% knitr::kable()

# Mirando por fecha
# Los errores son estables por fecha
forecast_train %>% 
  group_by(date, model) %>% 
  summarise(rmsle = rmsle_vec(truth = sales, estimate = preds)) %>% 
  spread(model, rmsle) %>% 
  arrange(date) %>% fun_print()

# Mirando por store
# Store_nbr == 52, 18, 25 los mas complicado
forecast_train %>% 
  group_by(store_nbr, model) %>% 
  summarise(rmsle = rmsle_vec(truth = sales, estimate = preds)) %>% 
  spread(model, rmsle) %>% 
  arrange(LIGHTGBM) %>% fun_print()

# Mirando por familia
# No es tan critico como store, pero hay potenciales a reducir
# LAWN AND GARDEN y GROCERY II 
forecast_train %>% 
  group_by(family, model) %>% 
  summarise(rmsle = rmsle_vec(truth = sales, estimate = preds)) %>% 
  spread(model, rmsle) %>% 
  arrange(LIGHTGBM) %>% fun_print()

# Conclusion:
# El mejor modelo individual LIGHTGBM
# Posiblemente sea mejor hacer un modelo solo para store 53 y 18
# Se 
forecast_train %>% 
  filter(store_nbr == 52, model == 'LIGHTGBM') %>%
  mutate(error = (logaritmizador(sales) - logaritmizador(preds))^2) %>% View

# Forecasting test set - Submission ---------------------------------------
models_fitted <- resample_results %>% 
  pull(.model)

# Last split training + testing
last_fit <- bind_rows(training(splits$splits[[1]]), testing(splits$splits[[1]]))
refit_tbl <- model_tbl %>%
  modeltime_refit(model, data = last_fit) # Fit the last slice 

# Forecasting on real test
forecast_test <- refit_tbl %>% 
  modeltime_forecast(new_data = test, keep_data = TRUE) %>% 
  select(id, date, .model_desc, preds = .value) %>% 
  mutate(preds = exponenciador(preds),
         preds = ifelse(preds < 0, 0, preds)) # hay sales negativos

# Preparando el submission
forecast_test %>% 
  filter(.model_desc == "LIGHTGBM") %>% # escogiendo el mejor modelo
  select(id, sales = preds) %>% 
  write_csv("Data/submission_H3_lightgbm_dummy.csv")

# Guardando datasets claves -----------------------------------------------
write_rds(list(splits = splits,
               recipe_spec = recipe_spec,
               model_tbl = model_tbl,
               resample_results = resample_results),
          "Data/H3.RDS")

# df_list <- read_rds("Data/H3.RDS")
# splits <- df_list$splits
# recipe_spec = df_list$recipe_spec
# model_tbl = df_list$model_tbl
# resample_results = df_list$resample_results

# Data set comparativo 
forecast_train %>% 
  mutate(model = paste0("h3_", model)) %>% 
  select(id, date, store_nbr, family, sales, model, preds) %>% 
  spread(model, preds) %>% 
  write_csv("Data/H3_test_models.csv")
