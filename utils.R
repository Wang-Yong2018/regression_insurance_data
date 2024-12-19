library(yardstick)
library(tidymodels)
library(lightgbm)
library(butcher)
library(broom)
library(memoise)
library(finetune)

rmsel <- function(data,
                  truth = 'truth',
                  estimate = 'estimate',
                  na_rm = TRUE) {
  #  data|>glimpse()
  data |>
    select(truth = {{truth}}, estimate = {{estimate}}) |>
    transmute(truth = log1p(truth), estimate = log1p(estimate)) |>
    rmse(truth, estimate)
}

get_lm_wf <- function(){
  
  lm_spec <- 
    linear_reg() |>
    set_mode("regression") |>
    set_engine("lm")

  lm_wf <-
    workflow() |>
    add_model(lm_spec)
  return(lm_wf)
}

get_lgb_wf <- function(n_trees = 500,
                       rate = 0.01,
                       n_depth = 15,
                       n_leaves = 1000,
                       n_leaves_min = 100,
                       min_gain_to_split = 0.001,
                       l1_alpha = 0.2,
                       l2_alpha = 0.2,
                       sample_rate = 0.8,
                       n_early_stop = 50,
                       verbose_level = 1) {
  mod_spec <- boost_tree(
    #mtry = 50,
    stop_iter = n_early_stop,
    # p/3 for regression case, p is total feature numbers
    min_n = n_leaves_min,
    trees = n_trees,
    tree_depth = n_depth,
    learn_rate = rate,
    sample_size = sample_rate,
    loss_reduction = min_gain_to_split
  ) |>
    set_mode("regression") |>
    set_engine(
      "lightgbm",
      num_leaves = n_leaves,
      lambda_l2 = l1_alpha,
      lambda_l1 = l2_alpha,
      verbose = verbose_level
    )
  
  lgb_wf <-
    workflow() |>
    add_model(mod_spec)
  return(lgb_wf)
  
}




get_fit_wf <- function(rcp,
                       data,
                       name = '',
                       description = '',
                       is_log1py = FALSE,
                       return_wf = FALSE) {
  lm_wf <- get_lm_wf()
  fit_wf <- lm_wf |>
    add_recipe(rcp) |>
    fit({{data}})
  fit_mod <-
    fit_wf |>
    extract_fit_engine()
  #tmp_baked <- rcp|>prep()|>check_missing(all_predictors())|> bake(data)
  
  tidy_mod <- fit_mod |> broom::tidy()
  glance_mod <- fit_mod |> broom::glance()
  tidy_augment <-
    fit_mod |>
    broom::augment() |>
    select(truth = `..y`, .pred = `.fitted`)
  
  if (is_log1py == TRUE) {
    # if the outcome variable is log1p transformed in recipes.
    # the .pred , truth should be reverse to original scale.
    # as the log1p residual just minus
    tidy_augment <-
      tidy_augment |>
      mutate(
        log1p_residual = .pred - truth,
        .pred = exp(.pred) + 1,
        truth = exp(truth) + 1
      )
  } else
    (tidy_augment <-
       tidy_augment |>
       mutate(log1p_residual = log1p(.pred) - log1p(truth)))
  rmsel_value <-
    tidy_augment  |>
    rmsel(truth, .pred) |>
    pull(.estimate)
  
  rmse_value <-
    tidy_augment |>
    rmse(truth, .pred) |>
    pull(.estimate)
  
  glance_mod <-
    glance_mod |>
    mutate(rmse = rmse_value, rmsel = rmsel_value)
  
  fit_mod$qr <- NULL
  # fit_mod$residuals <- NULL
  fit_mod$log_residual <- tidy_augment |> pull(log1p_residual)
  fit_mod$effects <- NULL
  fit_mod$na.action <- NULL
  fit_mod$model <- NULL
  
  
  vetiver_mod <- vetiver_model(
    model = fit_mod,
    model_name = name ,
    description = {{description}},
    metadata = list(metrics = glance_mod)
  )
  
  vetiver_mod$metadata$user$metrics  |>
    pivot_longer(cols = everything()) |>
    filter(grepl('rmse|r.squared|nob', name)) |>
    print()
  
  keep_model(vetiver_mod)
  if (return_wf == TRUE) {
    result <- fit_wf
  } else{
    result <- fit_mod
  }
  return(result)
}



get_fit_lgb_wf <- function(rcp,
                           data,
                           name = '',
                           description = '',
                           is_log1py = FALSE,
                           return_wf = FALSE) {
# setup metrics
    reg_metrics <- metric_set(mae, rsq)
# data split 
## train & test
  set.seed(1024)
  data_split <- initial_split(data)
  
  data_train <- training(data_split)
  data_test <- testing(data_split)
## cv 
  set.seed(1024)
  data_rs <- vfold_cv(data_train)
 lgbm_spec <- 
    boost_tree(trees = 500,
               tree_depth = 15,
               learn_rate =0.5,
               stop_iter= 10,
               min_n = 50,
               sample_size= 0.8,
               loss_reduction = 0.001
               )|>
    set_mode("regression") %>% 
    set_engine("lightgbm", 
               force_row_wise=TRUE,
               verbose = 1,
               num_leaves = 2048,
               lambda_l2 = 0.2,
               lambda_l1 = 0.2,
               )
  
 # lgbm_grid <- 
 #   lgbm_param %>%   
 #   grid_space_filling(size = 50)
 
 lgbm_wflow <- workflow(rcp, lgbm_spec)
 lgbm_param <-
   lgbm_wflow |>
   extract_parameter_set_dials() 
 
 ctrl <- control_resamples(save_pred = TRUE,
                           verbose = TRUE,  # Enable verbose output
                           allow_par = TRUE  # Allow parallel processing (optional)
                           )
 lgb_res <-
   lgbm_wflow %>%
   fit_resamples(data_rs, control = ctrl, metrics = reg_metrics)
 
 collect_metrics(lgb_res)
 
 
 # 
 # cores <- parallelly::availableCores(logical = FALSE)
 # cl <- parallel::makePSOCKcluster(cores)
 # doParallel::registerDoParallel(cl)
 # 
 # set.seed(9)
 # ctrl <- control_grid(save_pred = TRUE)
 # 
 # lgbm_res <-
 #   lgbm_wflow %>%
 #   tune_grid(
 #     resamples = data_rs,
 #     grid = 4,
 #     # The options below are not required by default
 #     param_info = lgbm_param, 
 #     control = ctrl,
 #     metrics = reg_metrics
 #   )
 # 
 # foreach::registerDoSEQ()
 # parallel::stopCluster(cl)
 # 
 # lgbm_res 
 # autoplot(lgbm_res)
 # collect_metrics(lgbm_res)
 # collect_metrics(lgbm_res, summarize = FALSE)
 # show_best(lgbm_res, metric = "rsq")
 # lgbm_best <- select_best(lgbm_res, metric = "mae")
 # lgbm_best
 # 
 return(lgb_res)
}