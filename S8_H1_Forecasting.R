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
# sc <- SparkR::sparkR.session(master="local")
packages <- c("modeldata", "tidymodels")
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
store_df <- train %>%
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

# Crossvalidation ---------------------------------------------------------
# Sliding window train and test set
ventana_train <- 150
ventana_test <- 15
splits <- time_series_cv(
  data        = store_df,
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

# Recipe Basic
recipe_basic <- recipe(sales ~ date, template) %>% 
  step_log(sales, offset = 1, base = 10)

# Recipes for models with extra regresors
recipe_spec <- recipe(sales ~ ., template) %>%
  step_timeseries_signature(date) %>%
  step_select(sales, date, store_nbr, onpromotion, delta_oil_1d, delta_oil_7d,
              type, cluster, holiday, date_half, date_quarter, date_month,
              date_day, date_wday, date_week, type, cluster) %>%
  step_mutate(store_nbr = factor(store_nbr)) %>%
  step_dummy(all_nominal()) %>% 
  step_log(sales, offset = 1, base = 10)

recipe_basic %>% prep %>% bake(NULL) %>% glimpse
recipe_spec %>% prep %>% bake(NULL) %>% glimpse

# Modeling and fit --------------------------------------------------------
# Lets train with 7 models
# Model 1: auto_arima
# wf_auto_arima <- workflow() %>% 
#   add_model(arima_reg() %>% set_engine(engine = "auto_arima")) %>%
#   add_recipe(recipe_basic) %>% 
#   fit(data = training(splits$splits[[1]]))
# 
# # Model 2: arima_boost
# wf_arima_boost <- workflow() %>% 
#   add_model(arima_boost(min_n = 5, 
#                         learn_rate = 0.01, 
#                         trees = 5000,
#                         tree_depth = 6) %>%
#               set_engine(engine = "auto_arima_xgboost")) %>% 
#   add_recipe(recipe_basic) %>%
#   fit(data = training(splits$splits[[1]]))
# 
# # Model 3: ets
# wf_ets <- workflow() %>% 
#   add_model(exp_smoothing() %>%
#               set_engine(engine = "ets")) %>%
#   add_recipe(recipe_basic) %>% 
#   fit(data = training(splits$splits[[1]]))

# Model 4: lm
wf_linear <- workflow() %>% 
  add_model(linear_reg() %>% 
              set_engine("lm")) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>% 
  fit(training(splits$splits[[1]]))

# Model 5: lasso
wf_glmnet <- workflow() %>% 
  add_model(linear_reg(penalty = 1, mixture = 1) %>% 
              set_engine("glmnet")) %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>% 
  fit(training(splits$splits[[1]]))

# Model 6: prophet
# wf_prophet <- workflow() %>% 
#   add_model(prophet_reg(seasonality_yearly = TRUE,
#                         seasonality_daily = TRUE) %>% 
#               set_engine("prophet")) %>%
#   add_recipe(recipe_basic) %>% 
#   fit(training(splits$splits[[1]]))

# Model 7 Xgboost
wf_xgboost <- workflow() %>% 
  add_model(boost_tree(mode = "regression",
                       trees = 5000, 
                       tree_depth = 6, 
                       learn_rate = 0.01, 
                       min_n = 5) %>% 
              set_engine("xgboost"))  %>%
  add_recipe(recipe_spec %>% step_rm(date)) %>% 
  fit(training(splits$splits[[1]]))

# Model 8 Prophet Boost
wf_prophet_xgboost <-  workflow() %>% 
  add_model(prophet_boost(seasonality_yearly = TRUE, 
                          seasonality_daily = TRUE, 
                          trees = 5000,
                          tree_depth = 6, 
                          learn_rate = 0.01, 
                          min_n = 5) %>% 
              set_engine("prophet_xgboost")) %>% 
  add_recipe(recipe_spec) %>% 
  fit(training(splits$splits[[1]]))

# Model Time Table and Refit ----------------------------------------------
model_tbl <- modeltime_table(
  # wf_auto_arima,
  # wf_arima_boost,
  # wf_ets,
  wf_linear,
  wf_glmnet,
  # wf_prophet, 
  wf_xgboost,
  wf_prophet_xgboost
)

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
  modeltime_resample_accuracy(summary_fns = NULL, 
                              metric_set = rmse) %>% View

typeof(resample_results$.model[[3]]$fit$fit$spec$)
