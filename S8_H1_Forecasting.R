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
# Sliding window train and test set
ventana_train <- 150
ventana_test <- 15
splits <- time_series_cv(
  data        = store_train,
  date_var    = date, 
  initial     = ventana_train,
  assess      = ventana_test,
  skip        = ventana_train + ventana_test + 1,
  cumulative  = FALSE)

# Estamos OK
split_res <- splits %>%
  tk_time_series_cv_plan() %>% 
  group_by(.id, .key) %>% 
  summarise(min_date = min(date),
            max_date = max(date),
            n = n(),
            # n_stores = n_distinct(store_nbr),
            sum_sales = sum(sales),
            sum_prom = sum(onpromotion)) %>% 
  mutate(diff = max_date - min_date) %>% 
  fun_print()

# Esta perfecto
splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, 
                           .facet_ncol = 2, .interactive = FALSE)

# Recipes -----------------------------------------------------------------
template <- training(splits$splits[[1]]) # template for recipe

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
# Model 1: lm
wf_linear <- workflow() %>% 
  add_model(linear_reg() %>% 
              set_engine("lm")) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>% 
  fit(training(splits$splits[[1]]))

# Model 2: lasso
wf_glmnet <- workflow() %>% 
  add_model(linear_reg(penalty = 0.01, mixture = 1) %>% 
              set_engine("glmnet")) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>% 
  fit(training(splits$splits[[1]]))

# Model 3 Xgboost
wf_xgboost <- workflow() %>% 
  add_model(boost_tree(mode = "regression",
                       trees = 5000, 
                       tree_depth = 6, 
                       learn_rate = 0.01, 
                       min_n = 5) %>% 
              set_engine("xgboost"))  %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>% 
  fit(training(splits$splits[[1]]))

# Model 4 Prophet Boost
wf_prophet_xgboost <-  workflow() %>% 
  add_model(prophet_boost(seasonality_yearly = "auto",
                          seasonality_weekly = "auto",
                          seasonality_daily = "auto", 
                          trees = 5000,
                          tree_depth = 6, 
                          learn_rate = 0.01, 
                          min_n = 5) %>% 
              set_engine("prophet_xgboost")) %>% 
  add_recipe(recipe_spec) %>% 
  fit(training(splits$splits[[1]]))

# Model 5 lightgbm
wf_ltboost <- workflow() %>% 
  add_model(boost_tree(mode = "regression",
                       trees = 5000, 
                       tree_depth = 6, 
                       learn_rate = 0.01, 
                       min_n = 5) %>% 
              set_engine("lightgbm"))  %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>% 
  fit(training(splits$splits[[1]]))

# Model Time Table and Refit ----------------------------------------------
model_tbl <- modeltime_table(
  wf_linear,
  wf_glmnet,
  wf_xgboost,
  wf_prophet_xgboost,
  wf_ltboost
)

# Resampling Refit
resample_results <- model_tbl %>%
  modeltime_fit_resamples(
    resamples = splits,
    control   = control_resamples(verbose = TRUE)
  )

# Results - RMSLE ---------------------------------------------------------
resample_results %>%
  plot_modeltime_resamples(
    .summary_fn  = mean, 
    .metric_set  = rmsle,
    .point_size  = 3,
    .interactive = TRUE
  )

resample_results %>%
  modeltime_resample_accuracy(summary_fns = list(mean = mean, median = median, sd = sd), 
                              metric_set = rmse) 

# Lets see training error versus test error
train_calibrate <- resample_results %>% 
  filter(!.model_desc %in% c("LM", "LIGHTGBM")) %>% # saco LM y lightgbm que tuvo problemas
  pull(.model) %>% 
  map(function(df) { # loopeando por modelo
    eng <- extract_spec_parsnip(df)
    bla("Calculando Train RMSLE para:", eng$engine)
    map_df(splits$splits, function(sp) { # loopeando por splits
      df %>% modeltime_calibrate(training(sp), quiet = TRUE)
    })
  })

train_scores <- train_calibrate %>% 
  map(function(df) modeltime_accuracy(df, metric_set = rmse))

test_calibrate <- resample_results %>% 
  filter(!.model_desc %in% c("LM", "LIGHTGBM")) %>% # saco LM que tuvo problemas
  pull(.model) %>% 
  map(function(df) { # loopeando por modelo
    eng <- extract_spec_parsnip(df)
    bla("Calculando Train RMSLE para:", eng$engine)
    map_df(splits$splits, function(sp) { # loopeando por splits
      df %>% modeltime_calibrate(testing(sp), quiet = TRUE)
    })
  })

test_scores <- test_calibrate %>% 
  map(function(df) modeltime_accuracy(df, metric_set = rmse))


mean_scores <- map2(train_scores, test_scores, function(x, y) {
  x %>% select(.model_id, .model_desc, rmse_train = rmse) %>% 
    bind_cols(y %>% select(rmse_test = rmse))
}) %>% 
  map_df(function(df) {
    df %>% group_by(.model_desc) %>% 
      summarise(rmse_train = mean(rmse_train),
                rmse_test = mean(rmse_test))
  }) # the results vary from the resampling.....

# Family-wise forecasting -------------------------------------------------
models_fitted <- resample_results %>% 
  filter(!.model_desc %in% c("LM")) %>% # saco LM 
  pull(.model)

# Last split training
last_fit <- training(splits$splits[[1]]) %>% 
  arrange(date, store_nbr)
refit_train <- model_tbl %>%
  modeltime_refit(model, data = last_fit) # Fit the last slice 

# Forecasting on split of test
# hare dos ensamblajes
# ENSEMBLE1 = promedio todos los modelos
# ENSEMBLE2 = promedio xgboost, light gbm y prophet boost
forecast_train <- refit_train %>% 
  filter(.model_desc != "LM") %>% 
  modeltime_forecast(new_data = testing(splits$splits[[1]]), keep_data = TRUE) %>% 
  select(date, store_nbr, .model_desc, sales_total = sales, preds = .value) %>% 
  spread(.model_desc, preds) %>% 
  rowwise() %>%
  mutate(ENSEMBLE1 = mean(c_across(GLMNET:XGBOOST)), # 
         ENSEMBLE2 = mean(c_across(LIGHTGBM:XGBOOST))) %>% 
  gather(.model_desc, preds, GLMNET:ENSEMBLE2) %>% 
  mutate(preds = exponenciador(preds))
  
# Me quedo con la última fecha de entrenamiento
rolling_pct_train <- df_strs %>% 
  filter(date == max(last_fit$date)) %>% # 2017-07-31 ultimo día del entrenamiento
  gather(roll_pct, value, pct:pct_rm_30d) %>% 
  select(store_nbr, family, roll_pct, value)

forecast_rolling_pct <- forecast_train %>% 
  left_join(rolling_pct_train, by = "store_nbr") %>% 
  left_join(train, by = c("date", "store_nbr", "family")) %>% 
  select(id, date, store_nbr, family, sales, sales_total, model = .model_desc, 
         preds_total = preds, roll_pct, value) %>% 
  mutate(preds = value * preds_total)

# Calculando RMSLE
# Hasta ahora el porcentaje del bisemanal da el mejor resultado 
# El mejor modelo es Prophet + XgBoost
# ensamblajes no ayudan
forecast_rolling_pct %>% 
  group_by(model, roll_pct) %>% 
  summarise(rmsle = rmsle_vec(truth = sales, estimate = preds)) %>% 
  spread(model, rmsle) %>% 
  arrange(LIGHTGBM)

# Mirando por fecha
# Los errores son estables por fecha
forecast_rolling_pct %>% 
  group_by(date, model, roll_pct) %>% 
  summarise(rmsle = rmsle_vec(truth = sales, estimate = preds)) %>% 
  spread(model, rmsle) %>% 
  filter(roll_pct == "pct_rm_14d") %>% 
  arrange(date) %>% fun_print()

# Mirando por store
# Store_nbr == 50 el mas complicado
forecast_rolling_pct %>% 
  group_by(store_nbr, model, roll_pct) %>% 
  summarise(rmsle = rmsle_vec(truth = sales, estimate = preds)) %>% 
  spread(model, rmsle) %>% 
  filter(roll_pct == "pct_rm_14d") %>% 
  arrange(ENSEMBLE1) %>% fun_print()

# Conclusion:
# El mejor modelo individual fue PROPHET W/ XGBOOST
# El mejor rolling pct fue el de las ultimas dos semanas

# Forecasting test set - Submission ---------------------------------------
models_fitted <- resample_results %>% 
  filter(!.model_desc %in% c("LM")) %>% # saco LM 
  pull(.model)

# Last split training + testing
last_fit <- bind_rows(training(splits$splits[[1]]), testing(splits$splits[[1]])) %>% 
  arrange(date, store_nbr)
refit_tbl <- model_tbl %>%
  modeltime_refit(model, data = last_fit) # Fit the last slice 

# Forecasting on real test
forecast_test <- refit_train %>% 
  filter(.model_desc != "LM") %>% 
  modeltime_forecast(new_data = store_test, keep_data = TRUE) %>% 
  select(date, store_nbr, .model_desc, preds = .value) %>% 
  mutate(preds = exponenciador(preds))

# Me quedo con la última fecha del train test
rolling_pct_test <- df_strs %>% 
  filter(date == max(last_fit$date)) %>% # 2017-08-15 ultimo día del entrenamiento
  gather(roll_pct, value, pct:pct_rm_30d) %>% 
  select(store_nbr, family, roll_pct, value)

forecast_rolling_pct_test <- forecast_test %>% 
  left_join(rolling_pct_test, by = "store_nbr") %>% 
  select(date, store_nbr, family, sales, sales_total, model = .model_desc, 
         preds_total = preds, roll_pct, value) %>% 
  mutate(preds = value * preds_total)

# Preparando el submission
forecast_rolling_pct_test %>% 
  filter(roll_pct == "pct_rm_14d") %>% # escogiendo el mejor rolling percentage
  filter(model == "PROPHET W/ XGBOOST ERRORS") %>% # escogiendo el mejor modelo
  left_join(test, by = c("date", "store_nbr", "family")) %>% 
  select(id, sales = preds) %>% 
  write_csv("Data/submission_H1_prophet_boost_pct_14d.csv")


# Guardando datasets claves -----------------------------------------------
write_rds(list(splits = splits,
               recipe_spec = recipe_spec,
               model_tbl = model_tbl,
               resample_results = resample_results),
          "Data/H1.RDS")

# df_list <- read_rds("Data/H1.RDS")
# splits <- df_list$splits
# recipe_spec = df_list$recipe_spec
# model_tbl = df_list$model_tbl
# resample_results = df_list$resample_results
