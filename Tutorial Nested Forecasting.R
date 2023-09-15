################################
# Tutorial: Nested Forecasting #
################################
# Fecha: 2023-09-13
# Creador: FR
# https://business-science.github.io/modeltime/articles/nested-forecasting.html

# Libraries ---------------------------------------------------------------
setwd("~/falrrema@gmail.com - Google Drive/My Drive/Proyectos DS/kgl_Store_Sales")
source("Helper.R")
packages <- c("tidymodels", "modeltime", "modeltime.resample", "timetk",
              "tidyverse", "tidyquant")
requirements_libs(packages)

# Data --------------------------------------------------------------------
data_tbl <- walmart_sales_weekly %>%
  select(id, Date, Weekly_Sales) %>%
  set_names(c("id", "date", "value"))

data_tbl %>%
  group_by(id) %>%
  plot_time_series(date, value, .interactive = F, .facet_ncol = 2)

# Data Preparation --------------------------------------------------------
nested_data_tbl <- data_tbl %>%
  
  # 1. Extending: We'll predict 52 weeks into the future.
  extend_timeseries(
    .id_var        = id,
    .date_var      = date,
    .length_future = 52
  ) %>%
  
  # 2. Nesting: We'll group by id, and create a future dataset
  #    that forecasts 52 weeks of extended data and
  #    an actual dataset that contains 104 weeks (2-years of data)
  nest_timeseries(
    .id_var        = id,
    .length_future = 52,
    .length_actual = 52*2
  ) %>%
  
  # 3. Splitting: We'll take the actual data and create splits
  #    for accuracy and confidence interval estimation of 52 weeks (test)
  #    and the rest is training data
  split_nested_timeseries(
    .length_test = 52
  )

nested_data_tbl
nested_data_tbl$.splits[[2]]$idx_train

# Step 1: Create Tidymodels Workflows -------------------------------------
# Prophet
rec_prophet <- recipe(value ~ date, extract_nested_train_split(nested_data_tbl)) 

wflw_prophet <- workflow() %>%
  add_model(prophet_reg("regression", seasonality_yearly = TRUE) %>% 
              set_engine("prophet")) %>%
  add_recipe(rec_prophet)

# XGBoost
rec_xgb <- recipe(value ~ date, extract_nested_train_split(nested_data_tbl)) %>%
  step_timeseries_signature(date) %>%
  step_rm(date) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE)

wflw_xgb <- workflow() %>%
  add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
  add_recipe(rec_xgb)

# Step 2: Nested Modeltime Tables -----------------------------------------
nested_modeltime_tbl <- modeltime_nested_fit(
  # Nested data 
  nested_data = nested_data_tbl,
  metric_set = rmsle,
  
  # Add workflows
  wflw_prophet,
  wflw_xgb
)

nested_modeltime_tbl
nested_modeltime_tbl$.modeltime_tables[[1]]$.calibration_data

# Step 3: Logged Attributes -----------------------------------------------
nested_modeltime_tbl %>% 
  extract_nested_test_accuracy() %>%
  table_modeltime_accuracy(.interactive = F)

nested_modeltime_tbl %>% 
  extract_nested_test_forecast() %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol  = 2,
    .interactive = TRUE
  )

# Range of training set
data_tbl %>% 
  group_by(id) %>% 
  summarise(ini = min(date),
            fin = max(date))

# Extract Nested Error Logs
nested_modeltime_tbl %>% 
  extract_nested_error_report()

# Step 4: Select the Best -------------------------------------------------
best_nested_modeltime_tbl <- nested_modeltime_tbl %>%
  modeltime_nested_select_best(
    metric                = "rmsle", 
    minimize              = TRUE, 
    filter_test_forecasts = TRUE
  )

best_nested_modeltime_tbl %>%
  extract_nested_best_model_report()

best_nested_modeltime_tbl %>%
  extract_nested_test_forecast() %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .facet_ncol  = 2,
    .interactive = TRUE
  )

# Step 5: Refitting and Future Forecast -----------------------------------
nested_modeltime_refit_tbl <- best_nested_modeltime_tbl %>%
  modeltime_nested_refit(control = control_nested_refit(verbose = TRUE))

nested_modeltime_refit_tbl %>%
  extract_nested_future_forecast() %>%
  group_by(id) %>%
  plot_modeltime_forecast(
    .interactive = FALSE,
    .facet_ncol  = 2
  )
