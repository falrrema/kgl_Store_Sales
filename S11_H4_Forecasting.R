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
test <- df_tbl %>% filter(slice == "test") %>% select(-slice) %>% 
  unite(store_family, store_nbr, family, remove = FALSE)

# Nested table ------------------------------------------------------------
nested_train <- train %>% 
  unite(store_family, store_nbr, family) %>% 
  extend_timeseries(
    .id_var        = store_family,
    .date_var      = date,
    .length_future = 15
  ) %>%
  nest_timeseries(
    .id_var        = store_family,
    .length_future = 15,
    .length_actual = 375
  ) %>%
  split_nested_timeseries(
    .length_test = 15
  )

extract_nested_train_split(nested_train) 

# Recipes -----------------------------------------------------------------
template <- extract_nested_train_split(nested_train)  # template for recipe

# Recipe simple
recipe_simple <- recipe(sales ~ date, template)

# Recipes for models with extra regresors
recipe_spec <- recipe(sales ~ ., template) %>%
  step_mutate_at(month:quarter, fn = as.factor) %>% 
  step_scale(oil_price) %>% 
  step_rm(id)

recipe_simple %>% prep %>% bake(NULL) %>% glimpse
recipe_spec %>% prep %>% bake(NULL) %>% glimpse
recipe_spec %>% prep %>% bake(new_data = test) %>% glimpse

# Modeling and fit --------------------------------------------------------
# Model 1: Auto ARIMA
wf_arima <- workflow() %>% 
  add_model(arima_reg(mode = "regression") %>% 
              set_engine(engine = "auto_arima")) %>%
  add_recipe(recipe_simple)

# Model 2: ARIMA Boost
wf_arima_boost <- workflow() %>% 
  add_model(arima_boost(trees = 3000, 
                        tree_depth = 6, 
                        learn_rate = 0.01, 
                        min_n = 5) %>%
      set_engine(engine = "auto_arima_xgboost")) %>%
  add_recipe(recipe_spec)

# Model 3: ETS
wf_ets <- workflow() %>% 
  add_model(exp_smoothing() %>%
              set_engine(engine = "ets")) %>%
  add_recipe(recipe_simple)

# # Model 4: lasso
# wf_glmnet <- workflow() %>% 
#   add_model(linear_reg(penalty = 0.001, mixture = 1) %>% 
#               set_engine("glmnet")) %>%
#   add_recipe(recipe_spec %>% step_rm(date) %>% step_dummy(all_nominal()))

# Model 5: Prophet Boost
wf_prophet_xgboost <-  workflow() %>%
  add_model(prophet_boost(seasonality_yearly = "auto",
                          seasonality_weekly = "auto",
                          seasonality_daily = "auto",
                          trees = 1000,
                          tree_depth = 6,
                          learn_rate = 0.01,
                          min_n = 5) %>%
              set_engine("prophet_xgboost")) %>%
  add_recipe(recipe_spec)

# Model 6: lightgbm
wf_ltboost <- workflow() %>% 
  add_model(boost_tree(mode = "regression",
                       trees = 3000, 
                       tree_depth = 6, 
                       learn_rate = 0.01, 
                       min_n = 5) %>% 
              set_engine("lightgbm"))  %>%
  add_recipe(recipe_spec %>% step_rm(date)) 

# Modeltime fit -----------------------------------------------------------
nested_modeltime_tbl <- modeltime_nested_fit(
  nested_data = nested_train,
  wf_arima,
  wf_arima_boost,
  wf_ets,
  wf_prophet_xgboost,
  wf_ltboost,
  metric_set = rmse,
  control = control_nested_fit(verbose = TRUE)
)

# Results - RMSLE ---------------------------------------------------------
# Observamos que el mejor modelo es un Exponencial smoothing 
nested_modeltime_tbl %>% 
  extract_nested_test_accuracy() %>% 
  mutate(.model_desc = ifelse(grepl("ETS", .model_desc), "ETS", .model_desc),
         rmse = round(rmse, 3)) %>% 
  group_by(.model_desc) %>% 
  summarise(prom = mean(rmse),
            med = median(rmse),
            min = min(rmse),
            max = max(rmse))

# Mejor modelo por par Store-Familia
# En general ETS es el modelo que más se ocupa en las series de tiempo (729)
# seguido por ARIMA (684) y después lightgbm (369)
nested_modeltime_tbl %>% 
  extract_nested_test_accuracy() %>% 
  mutate(.model_desc = ifelse(grepl("ETS", .model_desc), "ETS", .model_desc),
         rmse = round(rmse, 3)) %>% 
  group_by(store_family) %>% 
  filter(rmse == min(rmse)) %>% 
  ungroup() %>% 
  filter(!duplicated(store_family)) %>% # elimino modelos con mismo performance
  count(.model_desc)
  
# Selecting the Best model ------------------------------------------------
best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_select_best(
    metric                = "rmse", 
    minimize              = TRUE, 
    filter_test_forecasts = TRUE
  )

# El criterio de selección es muy semejante el sacado anteriormente:
# ARIMA: 684 series
# ETS: 729 series
# LightGBM: 369 series
best_nested_modeltime_tbl$.modeltime_tables %>% 
  bind_rows() %>% 
  mutate(.model_desc = ifelse(grepl("ETS", .model_desc), "ETS", .model_desc)) %>% 
  count(.model_desc)

# El error mas alto lo tiene:
# 47_SCHOOL AND OFFICE SUPPLIES: LightGBM 0.873
# 18_SCHOOL AND OFFICE SUPPLIES: ETSANA 0.78
best_nested_modeltime_tbl %>%
  extract_nested_best_model_report() %>% 
  mutate(rmse = round(rmse, 3)) %>% 
  arrange(desc(rmse))

# Extracción del forecast validacion
best_nested_modeltime_tbl$.future_data
best_nested_modeltime_tbl$.modeltime_tables[[1]]

forecast_train <- map2_df(best_nested_modeltime_tbl$store_family, 
     best_nested_modeltime_tbl$.modeltime_tables,
     function(store_family, modeltime_table) {
       modeltime_table$.calibration_data %>% 
         bind_rows() %>% 
         mutate(store_family = store_family, 
                model = modeltime_table$.model_desc)
     }) %>% 
  separate(col = store_family, into = c("store_nbr", "family")) %>% 
  mutate(store_nbr = as.double(store_nbr))

valid_tbl <- train %>% 
  select(id:sales) %>% 
  inner_join(forecast_train, by = c("date", "store_nbr", "family"))

# Aparentemente esto es una gran mejora
valid_tbl %>% 
  summarise(rmse = rmsle_vec(truth = exponenciador(sales), 
                             estimate = exponenciador(.prediction)))

# Refitting and Future Forecast -------------------------------------------
nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
  modeltime_nested_refit(control = control_nested_refit(verbose = TRUE))

nested_modeltime_refit_tbl %>% 
  modeltime_forecast(new_data = test, keep_data = TRUE)

a <- test %>% 
  filter(store_family == "1_AUTOMOTIVE")

nested_modeltime_refit_tbl$.modeltime_tables[[1]] %>% 
  modeltime_forecast(new_data = a)

forecast_test <- map2_df(nested_modeltime_refit_tbl$store_family, 
                      nested_modeltime_refit_tbl$.modeltime_tables,
                      function(sf, modeltime_table) {
                        df_test <- test %>%
                          filter(store_family == sf[[1]])
                        
                        modeltime_table %>%
                          bind_rows() %>%
                          modeltime_forecast(new_data = df_test) %>%
                          rename(date = .index) %>% 
                          left_join(df_test %>% select(id, date),
                                    by = "date")
                      }, .progress = TRUE) %>% 
  select(id, sales = .value) %>% 
  arrange(id)

forecast_test %>% 
  mutate(sales = exponenciador(sales)) %>% 
  write_csv("Data/submission_H4_by_store_family_360v.csv")

# Guardando datasets claves -----------------------------------------------
saveRDS(list(nested_train = nested_train,
             recipe_simple = recipe_simple,
             recipe_spec = recipe_spec,
             model_tbl = nested_modeltime_tbl,
             best_nested_modeltime_tbl = best_nested_modeltime_tbl),
        "Data/H4.RDS")

# df_list <- read_rds("Data/H4.RDS")

# Data set comparativo 
valid_tbl %>% 
  select(id, date, store_nbr, family, sales, model, preds = .prediction) %>% 
  write_csv("Data/H4_test_models.csv")
