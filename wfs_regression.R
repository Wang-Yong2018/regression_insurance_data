library(tidymodels)
library(workflowsets)
library(bonsai)
library(glmnet)
library(doParallel)
library(data.table)
library(dplyr)
library(lubridate)
library(geosphere)
library(recipes)
library(finetune)
library(yardstick)
library(rlang)

# Load data
data <- fread('input/train.zip')

# Feature engineering
data <- data %>%
  mutate(
    pickup_hour = hour(pickup_datetime),
    pickup_weekday = wday(pickup_datetime, label = TRUE),
    pickup_month = month(pickup_datetime, label = TRUE),
    is_weekend = ifelse(pickup_weekday %in% c("Sat", "Sun"), 1, 0),
    is_peak = ifelse(pickup_hour %in% c(7, 8, 17, 18), 1, 0),
    trip_duration = log1p(trip_duration),
    haversine_distance = distHaversine(
      cbind(pickup_longitude, pickup_latitude),
      cbind(dropoff_longitude, dropoff_latitude)
    ) / 1000,
    manhattan_distance = abs(pickup_longitude - dropoff_longitude) + abs(pickup_latitude - dropoff_latitude),
    direction = atan2(dropoff_longitude - pickup_longitude, dropoff_latitude - pickup_latitude)
  ) %>%
  select(-c(id, store_and_fwd_flag, pickup_datetime, dropoff_datetime)) %>%
  filter(!is.na(trip_duration))

# Data split
data_split <- initial_split(data, prop = 0.1)
train_data <- training(data_split)
test_data <- testing(data_split)

# Define recipe
recipe_log1p <- recipe(trip_duration ~ ., data = train_data) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# Define models
linear_model <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

ridge_model <- linear_reg(penalty = tune(), mixture = 0) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

lasso_model <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

lightgbm_model <- boost_tree(
  trees = 300,
  tree_depth = tune(),
  min_n = tune(),
  stop_iter = 10,
  mtry = 0.8,
  loss_reduction=0.001,
  sample_size=0.8,
  learn_rate = tune() 
) %>%
  set_engine("lightgbm", 
             num_leaves =tune() ,
             lambda_l1=0.05,
             lambda_l2=0.1,
             count = FALSE) %>%
  set_mode('regression')

# Create workflow set
lgb_grid <- workflow_set|>
  extract_parameter_set_dials(id='log1p_lightgbm')|>
  update( tree_depth = tree_depth(range=c(10, 15)),  # 树深度范围
          min_n = min_n(range=c(50, 200)),  # 每个节点的最小样本数范围
          learn_rate = learn_rate(range=c(0.01, 0.1),trans=NULL),  # 学习率范围
          num_leaves = num_leaves(range=c(256, 1024)),  # 叶子节点数范围
          )|>
  grid_space_filling(size=20)

# Parallel computation
cores <- 8
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)

# Tune models
#grid <- grid_regular(penalty(), levels = 10)
race_results <- 
  workflow_set %>%
  # option_add(param_info =lgb_params,
  #            id = "log1p_lightgbm") %>%
  workflow_map(
    "tune_race_anova",
    resamples = vfold_cv(train_data, v = 5),
    grid = lgb_grid,
    metrics = metric_set(rmse, rsq),
    control = control_race(verbose = TRUE,
                           verbose_elim=TRUE,
                           allow_par=TRUE,
                           save_pred=TRUE,
                           save_workflow = TRUE,
                           event_level=)
  )

# Stop parallel computation
stopCluster(cl)

# Evaluate models
metrics <- race_results %>%
  collect_metrics()
print(metrics)

# Find the best workflow
best_workflow_info <- metrics %>%
  filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice(1)

best_workflow_id <- best_workflow_info$wflow_id
best_workflow_config <- best_workflow_info$.config

best_workflow_param <- 
  race_results|>extract_workflow_set_result(best_workflow_id) |>
  select_best(metric='rmse') 

print(paste("Best workflow ID:", best_workflow_id))
print(paste("Best workflow config:", best_workflow_config))
print("Best parameter tuned")
print( best_workflow_param)
# Train the final model
 

best_workflow <- race_results %>%
  extract_workflow(best_workflow_id) %>%
  finalize_workflow(best_workflow_param)

final_model <- fit(best_workflow, train_data)

# Make predictions
predictions <- predict(final_model, test_data) %>%
  mutate(.pred = expm1(.pred))

# Print predictions
print(predictions)

# Save the model
saveRDS(final_model, "final_model.rds")