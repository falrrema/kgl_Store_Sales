#######################################################
# S9 Hypothesis 2: Store level forecast Ventana Train #
#######################################################
# Fecha: 2023-09-12
# Creador: FR
# La segunda hipótesis es que siguiendo la linea de pensamiento de la H1
# posiblemente hayan otras ventanas de entrenamiento que los modelos podrían ocupar 
# para tener mejor desempeño. Quiero investigar ventanas de entrenamiento desde 
# 50 dias a 1 año de data. 

# Cargando funciones ------------------------------------------------------
setwd("~/falrrema@gmail.com - Google Drive/My Drive/Proyectos DS/kgl_Store_Sales")
source("Helper.R")

# Seteando ambiente -------------------------------------------------------
# Específicos para el proyecto
packages <- c("plotly", "slider", "tidymodels", "modeltime", "modeltime.resample", "timetk",
              "tidyverse", "tidyquant", "bonsai", "vip")
requirements_libs(packages)

# Data --------------------------------------------------------------------
df_list <- read_rds("Data/df_list_kgl.RDS")
train <- df_list$train
# transactions <- df_list$transactions
oil <- df_list$oil
holidays <- df_list$holidays_events
stores <- df_list$stores
test <- df_list$test

# Feature engineering -----------------------------------------------------
# Oil delta precios
oil_complete <- tibble(date = seq.Date(from = min(oil$date), to = max(oil$date), by = "day")) %>% 
  left_join(oil, by = "date") %>% 
  arrange(date) %>% 
  mutate(price_oil = dcoilwtico) %>% 
  padr::pad() %>% fill(price_oil) %>% 
  mutate(delta_oil_1d = price_oil - lag(price_oil),
         delta_oil_7d = price_oil - lag(price_oil, 7)) %>% 
  select(-price_oil, -dcoilwtico)

hdays <- holidays %>%
  select(date, city = locale) %>%
  mutate(holiday = 1)

# We are going to simplify things and predict all stores sales, 
# forget for now the family
store_train <- train %>%
  group_by(date, store_nbr) %>%
  summarise(sales = sum(sales),
            onpromotion = sum(onpromotion)) %>%
  ungroup() %>%
  left_join(oil_complete, by = "date") %>%
  left_join(stores, by = "store_nbr") %>%
  left_join(hdays, c("date", "city")) %>%
  mutate(holiday = coalesce_0(holiday), 
         cluster = factor(cluster, ordered = FALSE)) %>%
  select(-city, -state) %>% 
  filter(date > ymd("2013-01-10"))

# Repeat for test
store_test <- test %>%
  group_by(date, store_nbr) %>%
  summarise(onpromotion = sum(onpromotion)) %>%
  ungroup() %>%
  left_join(oil_complete, by = "date") %>%
  left_join(stores, by = "store_nbr") %>%
  left_join(hdays, c("date", "city")) %>%
  mutate(holiday = coalesce_0(holiday), 
         cluster = factor(cluster, ordered = FALSE),
         sales = NA) %>%
  select(-city, -state)

# Rolling Family Percentage -----------------------------------------------
# Aquí vamos a calcular el porcentaje de la venta de cada familia con
# con ventanas desde la mas reciente a trimestral 
df_strs <- train %>% 
  select(-onpromotion) %>%
  arrange(date) %>% 
  # Generando ventas totales por store 
  group_by(date, store_nbr) %>% 
  mutate(sales_total = sum(sales)) %>% 
  group_by(store_nbr, family) %>% 
  # Generando otros features
  mutate(pct = sales/sales_total, # Porcentaje diario
         pct_rm_3d = slide_dbl(pct, mean, .before = 2, .after = 0, .complete = TRUE), # 3 dias
         pct_rm_7d = slide_dbl(pct, mean, .before = 6, .after = 0, .complete = TRUE), # semana 
         pct_rm_14d = slide_dbl(pct, mean, .before = 13, .after = 0, .complete = TRUE), # bisemanal
         pct_rm_30d = slide_dbl(pct, mean, .before = 29, .after = 0, .complete = TRUE)) %>%  # mensual
  ungroup()

# Crossvalidation ---------------------------------------------------------
# Vamos a generar distintos periodos de entrenamiento
# Sliding window train and test set
ventana_train <- c(50, 100, 150, 200, 250, 300, 360)
ventana_test <- 15

df_splits <- map(ventana_train, function(x) {
  splits <- time_series_cv(
    data        = store_train,
    date_var    = date, 
    initial     = x,
    assess      = ventana_test,
    skip        = x + ventana_test + 1,
    cumulative  = FALSE)
})

split_res <- df_splits %>% 
  map(function(splits) {
    splits %>% 
      tk_time_series_cv_plan() %>% 
      group_by(.id, .key) %>% 
      summarise(min_date = min(date),
                max_date = max(date),
                n = n(),
                # n_stores = n_distinct(store_nbr),
                sum_sales = sum(sales),
                sum_prom = sum(onpromotion)) %>% 
      mutate(diff = max_date - min_date)
  })

# Eliminamos el 2013 para evitar tantos splits en ventanas cortas
# además el último split de 360 dias comienza en el 2014
# Además el 2013 tiene problemas con onpromotion y tiendas en cero
df_splits <- map(ventana_train, function(x) {
  splits <- time_series_cv(
    data        = store_train %>% filter(year(date) >= 2014),
    date_var    = date, 
    initial     = x,
    assess      = ventana_test,
    skip        = x + ventana_test + 1,
    cumulative  = FALSE) %>% 
    mutate(ventana_train = x)
})

# Recipes -----------------------------------------------------------------
template <- training(df_splits[[1]]$splits[[1]]) # template for recipe

# Recipes for models with extra regresors
recipe_spec <- recipe(sales ~ ., template) %>%
  step_timeseries_signature(date) %>%
  step_select(sales, date, store_nbr, onpromotion, delta_oil_1d, delta_oil_7d,
              type, cluster, holiday, date_half, date_quarter, date_month,
              date_day, date_wday, date_week, type, cluster) %>%
  step_mutate(store_nbr = factor(store_nbr)) %>%
  step_dummy(all_nominal()) %>% 
  step_log(sales, offset = 1, base = 10)

recipe_spec %>% prep %>% bake(NULL) %>% glimpse
recipe_spec %>% prep %>% bake(new_data = store_test) %>% glimpse

# Modeling and fit --------------------------------------------------------
# Vamos a hacer un loop por las distintas ventanas de entrenamiento
workflow_list <- df_splits %>% 
  map(function(splits) {
    bla("Workflow para:", unique(splits$ventana_train))
    # Model 1: lm
    bla("    Entrenando LM")
    wf_linear <- workflow() %>% 
      add_model(linear_reg() %>% 
                  set_engine("lm")) %>%
      add_recipe(recipe_spec %>% step_rm(date)) %>% 
      fit(training(splits$splits[[1]]))
    
    # Model 2: lasso
    bla("    Entrenando LASSO")
    wf_glmnet <- workflow() %>% 
      add_model(linear_reg(penalty = 0.001, mixture = 1) %>% 
                  set_engine("glmnet")) %>%
      add_recipe(recipe_spec %>% step_rm(date)) %>% 
      fit(training(splits$splits[[1]]))
    
    # Model 3 Xgboost
    bla("    Entrenando XGBOOST")
    wf_xgboost <- workflow() %>% 
      add_model(boost_tree(mode = "regression",
                           trees = 8000, 
                           tree_depth = 7, 
                           learn_rate = 0.01, 
                           min_n = 10) %>% 
                  set_engine("xgboost"))  %>%
      add_recipe(recipe_spec %>% step_rm(date)) %>% 
      fit(training(splits$splits[[1]]))
    
    # Model 4 Prophet Boost
    bla("    Entrenando PROPHET BOOST")
    wf_prophet_xgboost <-  workflow() %>% 
      add_model(prophet_boost(seasonality_yearly = "auto",
                              seasonality_weekly = "auto",
                              seasonality_daily = "auto", 
                              trees = 8000,
                              tree_depth = 7, 
                              learn_rate = 0.01, 
                              min_n = 10) %>% 
                  set_engine("prophet_xgboost")) %>% 
      add_recipe(recipe_spec) %>% 
      fit(training(splits$splits[[1]]))
    
    # Model 5 lightgbm
    bla("    Entrenando LIGHTGBM")
    wf_ltboost <- workflow() %>% 
      add_model(boost_tree(mode = "regression",
                           trees = 8000, 
                           tree_depth = 7, 
                           learn_rate = 0.01, 
                           min_n = 10) %>% 
                  set_engine("lightgbm"))  %>%
      add_recipe(recipe_spec %>% step_rm(date)) %>% 
      fit(training(splits$splits[[1]]))
    
    # Fabricamos un modeltime table
    model_tbl <- modeltime_table(
      wf_linear,
      wf_glmnet,
      wf_xgboost,
      wf_prophet_xgboost,
      wf_ltboost
    )
  })

# Model Time Table and Refit ----------------------------------------------
# Vamos a ver el desempeño usando crossvalidation
# Resampling Refit
df_resample <- workflow_list %>% 
  map2(df_splits, function(model_tbl, splits) {
    model_tbl %>%
      modeltime_fit_resamples(
        resamples = splits,
        control   = control_resamples(verbose = TRUE)) %>% 
      mutate(ventana_train = unique(splits$ventana_train))
  })

# Results - RMSLE ---------------------------------------------------------
# Miramos los resultados por ventana de entrenamiento
# Ventanas interesante:
# 50 días
# 360 días (minima mediana)
# 150 días dado que subi ese resultado lo ocupare de comparacion
df_resample %>% 
  map_df(function(resample_results) {
    resample_results %>%
      rename(ventana = ventana_train) %>% 
      modeltime_resample_accuracy(summary_fns = list(mean = mean, 
                                                     median = median, 
                                                     sd = sd), 
                                  metric_set = rmsle) %>% 
      mutate(ventana_train = unique(resample_results$ventana_train))
  }) %>% 
  select(.model_desc, rmsle_mean, ventana_train) %>% 
  pivot_wider(names_from = .model_desc, values_from = rmsle_mean) %>% 
  knitr::kable()

# Por Slice
df_resample %>% 
  map(function(resample_results) {
    resample_results %>% 
      rename(ventana = ventana_train) %>% 
      modeltime_resample_accuracy(summary_fns = NULL, 
                                  metric_set = rmsle) %>% 
      group_by(.model_desc, .resample_id) %>% 
      summarise(rmsle = mean(rmsle)) %>% 
      spread(.model_desc, rmsle) %>% 
      mutate(ventana_train = unique(resample_results$ventana_train))
  })

modelos_escoger <- c(1,3,7) # 50, 150, 360
df_resample_fil <- df_resample[modelos_escoger]

# Family-wise forecasting -------------------------------------------------
# Tenemos el modeltime table entrenado con el último slice (ultima fecha)
# cuando armamos el workflow
# Forecasting on split of test
# hare dos ensamblajes
# ENSEMBLE = promedio todos los modelos
forecast_train <- map2(workflow_list[modelos_escoger], 
                       df_splits[modelos_escoger], 
                       function(model_tbl, splits) {
                         model_tbl %>% 
                           modeltime_forecast(new_data = testing(splits$splits[[1]]), keep_data = TRUE) %>% 
                           select(date, store_nbr, .model_desc, sales_total = sales, preds = .value) %>% 
                           spread(.model_desc, preds) %>% 
                           rowwise() %>%
                           mutate(ENSEMBLE = mean(c_across(GLMNET:XGBOOST))) %>% 
                           gather(.model_desc, preds, GLMNET:ENSEMBLE) %>% 
                           mutate(preds = exponenciador(preds),
                                  ventana_train = unique(splits$ventana_train))
                       })


# Me quedo con la última fecha de entrenamiento
# tomare el último día de entrenamiento de cualquier split (son todos iguales)
(fecha_final <- max(df_splits[[7]]$splits[[1]]$data[df_splits[[7]]$splits[[1]]$in_id,]$date))
rolling_pct_train <- df_strs %>% 
  filter(date == fecha_final) %>% # 2017-07-31 ultimo día del entrenamiento para todos los splits
  gather(roll_pct, value, pct:pct_rm_30d) %>% 
  select(store_nbr, family, roll_pct, value)

forecast_rolling_pct <- forecast_train %>% 
  map(function(df) {
    df %>% 
      left_join(rolling_pct_train, by = "store_nbr") %>% 
      left_join(train, by = c("date", "store_nbr", "family")) %>% 
      select(id, date, store_nbr, family, sales, sales_total, ventana_train, model = .model_desc, 
             preds_total = preds, roll_pct, value) %>% 
      mutate(preds = value * preds_total)
  })


# Calculando RMSLE
# Hasta ahora el porcentaje del bisemanal da el mejor resultado 
# El mejor modelo es Lightgbm para 360 días
# ensamblajes no aportan
# En 50 días los modelos lineales le va mejor
forecast_rolling_pct %>% 
  map_df(function(df){
    df %>% 
      group_by(model, roll_pct, ventana_train) %>% 
      summarise(rmsle = rmsle_vec(truth = sales, estimate = preds)) %>% 
      spread(model, rmsle) %>% 
      arrange(XGBOOST)
  }) %>% 
  group_by(ventana_train) %>% 
  filter(XGBOOST == min(XGBOOST))


# Mirando por fecha
# Los errores son estables por fecha
forecast_rolling_pct %>% 
  map(function(df) {
    df %>% 
      group_by(date, model, roll_pct, ventana_train) %>% 
      summarise(rmsle = rmsle_vec(truth = sales, estimate = preds)) %>% 
      spread(model, rmsle) %>% 
      filter(roll_pct == "pct_rm_14d") %>% 
      arrange(date) %>% fun_print()
  })


# Mirando por store
# Store_nbr == 50 el mas complicado
forecast_rolling_pct %>% 
  map(function(df) {
    df %>% 
      group_by(store_nbr, model, roll_pct, ventana_train) %>% 
      summarise(rmsle = rmsle_vec(truth = sales, estimate = preds)) %>% 
      spread(model, rmsle) %>% 
      filter(roll_pct == "pct_rm_14d") %>% 
      arrange(desc(LIGHTGBM)) %>% fun_print()
  })

# Conclusion:
# El mejor modelo individual fue LIGHTGBM para 360 dias
# El mejor modelo individual fue GLMNET para 50 dias
# El mejor rolling pct fue el de las ultimas dos semanas pct_rm_14d

# Forecasting test set - Submission ---------------------------------------
modelos_finales <- c(1,3)

# Last split training + testing
df_refit <- map2(workflow_list[modelos_finales], 
                 df_splits[modelos_finales], 
                 function(model_tbl, splits) {
                   last_fit <- bind_rows(training(splits$splits[[1]]), testing(splits$splits[[1]])) %>% 
                     arrange(date, store_nbr)
                   
                   model_tbl %>%
                     modeltime_refit(model, data = last_fit) %>% # Fit the last slice 
                     mutate(ventana_train = unique(splits$ventana_train))
                 })

# Forecasting on real test
forecast_test <- df_refit %>%
  map(function(refit) {
    refit %>% 
      modeltime_forecast(new_data = store_test, keep_data = TRUE) %>% 
      select(date, store_nbr, .model_desc, ventana_train, preds = .value) %>% 
      mutate(preds = exponenciador(preds))
  })

# Me quedo con la última fecha del train test
# tomare el último día de testing de cualquier split (son todos iguales)
(fecha_final <- max(df_splits[[7]]$splits[[1]]$data[df_splits[[7]]$splits[[1]]$out_id,]$date))
rolling_pct_test <- df_strs %>% 
  filter(date == fecha_final) %>% # 2017-08-15 ultimo día del entrenamiento
  gather(roll_pct, value, pct:pct_rm_30d) %>% 
  select(store_nbr, family, roll_pct, value)

forecast_rolling_pct_test <- forecast_test %>% 
  map(function(df) {
    df %>% 
      left_join(rolling_pct_test, by = "store_nbr") %>% 
      select(date, store_nbr, family, ventana_train, model = .model_desc, 
             preds_total = preds, roll_pct, value) %>% 
      mutate(preds = value * preds_total)
  })

# Preparando el submission con 50 dias
# modelo GLMNET
# rolling pct_rm_14d
forecast_rolling_pct_test[[1]] %>% 
  filter(roll_pct == "pct_rm_14d") %>% # escogiendo el mejor rolling percentage
  filter(model == "GLMNET") %>% # escogiendo el mejor modelo
  left_join(test, by = c("date", "store_nbr", "family")) %>% 
  select(id, sales = preds) %>% 
  write_csv("Data/submission_H2_GLMNET_14d_50v.csv")

# Preparando el submission con 50 dias
# modelo GLMNET
# rolling pct_rm_14d
forecast_rolling_pct_test[[2]] %>% 
  filter(roll_pct == "pct_rm_14d") %>% # escogiendo el mejor rolling percentage
  filter(model == "LIGHTGBM") %>% # escogiendo el mejor modelo
  left_join(test, by = c("date", "store_nbr", "family")) %>% 
  select(id, sales = preds) %>% 
  write_csv("Data/submission_H2_LIGHTGBM_14d_360v.csv")

# Guardando datasets claves -----------------------------------------------
write_rds(list(df_splits = df_splits,
               recipe_spec = recipe_spec,
               workflow_list = workflow_list,
               df_resample = df_resample),
          "Data/H2.RDS")

df_list <- read_rds("Data/H2.RDS")
df_splits <- df_list$df_splits
workflow_list = df_list$workflow_list
df_resample = df_list$df_resample
