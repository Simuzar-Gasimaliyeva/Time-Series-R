library(tidyverse)
library(data.table)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)

raw <- fread('AirPassengers.csv') 

colnames(raw) <- c('Date','Count')

raw %>% glimpse()

raw$Date <- raw$Date %>% as.Date(.,"%Y-%m-%d")


interactive <- FALSE


raw %>%
  plot_time_series(Date, Count, .interactive = interactive)

splits <- initial_time_split(raw,p = 0.9)


# Model 1: arima_boost ----
model_arm_boost <-  arima_boost() %>% 
  set_engine("auto_arima_xgboost") %>% 
  fit(Count ~ Date, data = training(splits))
  

# Model 2: ets ----
model_fit_ets <- exp_smoothing() %>%
  set_engine(engine = "ets") %>%
  fit(Count ~ Date, data = training(splits))


# Model 3: prophet ----
model_fit_prophet <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(Count ~ Date, data = training(splits))


#Step 3 - Add fitted models to a Model Table.----
models_tbl <- modeltime_table(
  model_arm_boost,
  model_fit_ets,
  model_fit_prophet
)

models_tbl

#Step 4 - Calibrate the model to a testing set.----
calibration_tbl <- models_tbl %>%
  modeltime_calibrate(new_data = testing(splits))

calibration_tbl


#Step 5 - Testing Set Forecast & Accuracy Evaluation----
#5A - Visualizing the Forecast Test----
calibration_tbl %>%
  modeltime_forecast(
    new_data    = testing(splits),
    actual_data = raw
  ) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )


#5B - Accuracy Metrics----
calibration_tbl %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = interactive
  )

#Step 6 - Refit to Full Dataset & Forecast Forward----
calibration_tbl <- model_arm_boost %>% modeltime_calibrate(new_data = testing(splits))

refit_tbl <- calibration_tbl %>%
  modeltime_refit(data = raw)

refit_tbl %>%
  modeltime_forecast(h = "1 year", actual_data = raw) %>%
  plot_modeltime_forecast(
    .legend_max_width = 25, # For mobile screens
    .interactive      = interactive
  )













