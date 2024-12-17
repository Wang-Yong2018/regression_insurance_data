library(yardstick)
library(tidymodels)
library(lightgbm)
library(butcher)
library(broom)
library(memoise)

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
  lgb_wf <- get_lgb_wf()
  fit_wf <- lgb_wf |>
    add_recipe(rcp) |>
    fit(data = {{data}})
  
  fit_mod <-
    fit_wf |>
    extract_fit_engine()
  
  best_score <- fit_mod$best_score
  params <- fit_mod$params
  
  tidy_mod <- fit_wf |> butcher::butcher()
  
  predictions <-
    fit_wf |>
    predict(new_data = data)
  
  # Combine predictions with actual values
  results <- data |>
    select(truth) |>
    bind_cols(predictions)
  
  # Define a metric set for R-squared, RMSE, and RMSLE
  metrics <- metric_set(rsq, rmse, rmsle)
  
  # Calculate the metrics
  metric_results <- metrics(results, truth = truth, estimate = .pred)
  
  
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
    (
      
      tidy_augment <-
        tidy_augment |>
        mutate(log1p_residual = log1p(.pred) - log1p(truth))
    )
  rmsel_value <-
    tidy_augment |>
    rmsel(truth, .pred) |>
    pull(.estimate)
  
  rmse_value <-
    tidy_augment |>
    rmse(truth, .pred) |>
    pull(.estimate)
  
  vetiver_mod <- vetiver_model(
    model = tidy_mod,
    model_name = name ,
    description = {{description}},
    metadata = list(metrics = metric_results)
  )
  
  # vetiver_mod$metadata$user$metrics  |>
  #   pivot_longer(cols = everything()) |>
  #   filter(grepl('rmse|r.squared|nob', name)) |>
  #   print()
  # 
  
  keep_model(vetiver_mod)
  if (return_wf == TRUE) {
    result <- fit_wf
  } else{
    result <- fit_mod
  }
  return(result)
}
