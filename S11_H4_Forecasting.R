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
test <- df_tbl %>% filter(slice == "test") %>% select(-slice)

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

# Model 2: arima-boost
wf_arima_boost <- workflow() %>% 
  add_model(arima_boost(trees = 5000, 
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

# Model 4: lasso
wf_glmnet <- workflow() %>% 
  add_model(linear_reg(penalty = 0.001, mixture = 1) %>% 
              set_engine("glmnet")) %>%
  add_recipe(recipe_spec %>% step_rm(date) %>% step_dummy(all_nominal()))

# Model 5: Prophet Boost
wf_prophet_xgboost <-  workflow() %>% 
  add_model(prophet_boost(seasonality_yearly = "auto",
                          seasonality_weekly = "auto",
                          seasonality_daily = "auto", 
                          trees = 5000,
                          tree_depth = 6, 
                          learn_rate = 0.01, 
                          min_n = 5) %>% 
              set_engine("prophet_xgboost")) %>% 
  add_recipe(recipe_spec) 

# Model 6: lightgbm
wf_ltboost <- workflow() %>% 
  add_model(boost_tree(mode = "regression",
                       trees = 5000, 
                       tree_depth = 6, 
                       learn_rate = 0.01, 
                       min_n = 5) %>% 
              set_engine("lightgbm"))  %>%
  add_recipe(recipe_spec %>% step_rm(date)) 

# Model 7: MARS
wf_mars <- workflow() %>% 
  add_model(mars(mode = "regression") %>%
              set_engine("earth") )  %>%
  add_recipe(recipe_spec %>% step_rm(date)) 


# Modeltime fit -----------------------------------------------------------
# cluster <- makeCluster(detectCores())
# registerDoParallel(cluster)
nested_modeltime_tbl <- modeltime_nested_fit(
  nested_data = nested_train,
  wf_arima,
  wf_arima_boost,
  wf_ets,
  wf_glmnet,
  wf_mars,
  wf_prophet_xgboost,
  wf_ltboost,
  metric_set = rmse,
  control = control_nested_fit(verbose = TRUE)
)
# stopCluster(cluster)
