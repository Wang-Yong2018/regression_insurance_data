library(dplyr)
library(tidyr)
library(forcats)
library(yardstick)
library(tidymodels)
library(bonsai)
library(lightgbm)
library(butcher)
library(broom)
library(memoise)
library(finetune)
library(workflows)
library(workflowsets)


source('etl.R')
source('insurance_rec.R')
source('mod_board.R')

rmsel <- function(data,
                  truth = 'truth',
                  estimate = 'estimate',
                  na_rm = TRUE){
  
  # data |> glimpse()
  data |>
    dplyr::select(truth = {{truth}}, 
           estimate = {{estimate}}) |>
    transmute(truth = log1p(truth), 
              estimate = log1p(estimate)) |>
    rmse(truth, estimate)
}

get_lm_eng <- function(){
  lm_eng <- 
    linear_reg() |>
    set_mode("regression") |>
    set_engine("lm")
  
  return(lm_eng)
}

get_lm_wf <- function(){
  
  lm_eng <- get_lm_eng()
  lm_wf <-
    workflow() |>
    add_model(lm_eng)
  return(lm_wf)
}

get_lgb_eng <- function(trees = 500,
                        tree_depth = 40,
                        learn_rate = 0.1,
                        mtry=0.8,
                        sample_size = 0.8,
                        stop_iter=10,
                        num_leaves= 900,
                        min_n= 100,
                        min_gain_to_split = 0.001,
                        lambda_l1 = 0.2,
                        lambda_l2 = 0.2,
                        num_threads = -1,
                        force_row_wise = TRUE,
                        verbose = 1) {
  # for lightgbm parameter config only.
  
  lgb_eng <- 
    boost_tree(trees = {{trees}},
               tree_depth = {{tree_depth}},
               learn_rate = {{learn_rate}},
               mtry = {{mtry}},
               sample_size = {{sample_size}},
               stop_iter = {{stop_iter}},
               min_n = {{min_n}},
               loss_reduction = {{min_gain_to_split}} ) |>
    set_engine( "lightgbm",
                num_leaves = {{num_leaves}},
                lambda_l1 = {{lambda_l1}},
                lambda_l2 = {{lambda_l2}},
                num_threads = {{num_threads}},
                force_row_wise = {{force_row_wise}},
                count = FALSE, 
                verbose = {{verbose}}) |>
    set_mode("regression") 
  
  return(lgb_eng)
  
}

get_tuned_eng_list <- function(){

  tune_mod_list <- 
    list(tune_lgb=get_lgb_eng(trees= tune(),
                              num_leaves = tune(),#key factor 1
                              tree_depth=tune(), # key factor 3
                              min_n=tune() # key factor 2
    ))
  
  return(tune_mod_list)
}

get_lgb_wf <- function() {
  
  lgb_eng <- get_lgb_eng()
  lgb_wf <-
    workflow() |>
    add_model(lgb_eng)
  return(lgb_wf)
}




get_fit_wf <- function(rcp,
                       data,
                       name = '',
                       description = '',
                       is_log1py = TRUE,
                       return_wf = FALSE) {
  lm_wf <- get_lm_wf()
  fit_wf <- lm_wf |>
    add_recipe(rcp) |>
    fit({{data}})
  fit_mod <-
    fit_wf |>
    extract_fit_engine()
  #tmp_baked <- rcp|>prep()|>check_missing(all_predictors())|> bake(data)
  
  tidy_augment <- 
    fit_wf |>
    predict(data) |>
    bind_cols(data)
  
  if (is_log1py == TRUE) {
    # if the outcome variable is log1p transformed in recipes.
    # the .pred , truth should be reverse to original scale.
    # as the log1p residual just minus
    tidy_augment <-
      tidy_augment |>
      mutate( .pred = exp(.pred) + 1)
  } 
  
   tidy_augment <-
       tidy_augment |>
       mutate(log1p_residual = log1p(.pred) - log1p(truth))

  rmsel_value <-
    tidy_augment  |>
    rmsel(truth, .pred) |>
    pull(.estimate)
  
  rmse_value <-
    tidy_augment |>
    rmse(truth, .pred) |>
    pull(.estimate)
  rsq_value <-
    tidy_augment |>
    rsq(truth, .pred) |>
    pull(.estimate)
  
  glance_mod <-
    data.frame(id = 0) |>
    mutate(rmse = rmse_value,
           rmsel = rmsel_value,
           rsq = rsq_value)
  
   fit_mod$residuals <- NULL
  # fit_mod$log_residual <- tidy_augment |> pull(log1p_residual)
   fit_mod$effects <- NULL
   fit_mod$na.action <- NULL
   fit_mod$model <- NULL
  log_residual <- 
    tidy_augment |> 
    dplyr::select(any_of(c('id', 'log1p_residual')))
  
  vetiver_mod <- vetiver_model(
    model = fit_mod,
    model_name = name ,
    description = {{description}},
    metadata = list(metrics = glance_mod, residuals = log_residual)
  )
  
  vetiver_mod$metadata$user$metrics  |>
    pivot_longer(cols = everything()) |>
    filter(grepl('rmse|rsq', name)) |>
    print()
  
  keep_model(vetiver_mod)
  if (return_wf == TRUE) {
    result <- fit_wf
  } else{
    result <- fit_mod
  }
  return(result)
}

get_fit_lgbwf <- function(rcp, data, name='', description='', is_log1py=FALSE, return_wf=FALSE,
                          trees = 300,
                          tree_depth = 40,
                          learn_rate = 0.1,
                          mtry=0.8,
                          sample_size = 0.8,
                          stop_iter=10,
                          num_leaves= 900){
  lgb_wf <- get_lgb_wf(
    trees = {{trees}},
    tree_depth = {{tree_depth}},
    learn_rate = {{learn_rate}},
    mtry = {{mtry}},
    sample_size = {{sample_size}},
    stop_iter = {{stop_iter}},
    num_leaves = {{num_leaves}}
  )
  
  print(lgb_wf)
 # reg_metrics <- metric_set(mae, rsq)
  fit_wf <- 
    lgb_wf |>
    add_recipe(rcp) |>
    fit({{data}})
  
  fit_mod <-
    fit_wf |>
    extract_fit_engine()
  #tmp_baked <- rcp|>prep()|>check_missing(all_predictors())|> bake(data)
  
  tidy_augment <- 
    fit_wf |>
    predict(data) |>
    bind_cols(data)
  
  if (is_log1py == TRUE) {
    # if the outcome variable is log1p transformed in recipes.
    # the .pred , truth should be reverse to original scale.
    # as the log1p residual just minus
    tidy_augment <-
      tidy_augment |>
      mutate( .pred = exp(.pred) + 1)
  } 
  
  tidy_augment <-
    tidy_augment |>
    mutate(log1p_residual = log1p(.pred) - log1p(truth))
  
  rmsel_value <-
    tidy_augment  |>
    rmsel(truth, .pred) |>
    pull(.estimate)
  
  rmse_value <-
    tidy_augment |>
    rmse(truth, .pred) |>
    pull(.estimate)
  rsq_value <-
    tidy_augment |>
    rsq(truth, .pred) |>
    pull(.estimate)
  
  
  glance_mod <-
    data.frame(id = 0) |>
    mutate(rmse = rmse_value, rmsel = rmsel_value, rsq = rsq_value)
  
  
  log_residual <- 
    tidy_augment |> 
    dplyr::select(any_of(c('id', 'log1p_residual')))
  


  if (return_wf == TRUE) {
    result <- fit_wf
    
  } else{
    result <- fit_mod
  }
  
  vetiver_mod <- vetiver_model(
    model = result |> butcher(),
    model_name = name ,
    description = {{description}},
    metadata = list(metrics = glance_mod, residuals = log_residual)
  )
  
  vetiver_mod$metadata$user$metrics  |>
    pivot_longer(cols = everything()) |>
    filter(grepl('rmse|rsq', name)) |>
    print()
  
  keep_model(vetiver_mod)
  return(result)
}  

get_tune_fit_lgb_wf <- function(rcp,
                           data,
                           name = '',
                           description = '',
                           is_log1py = FALSE,
                           return_wf = FALSE) {
# setup metrics
    reg_metrics <- metric_set(rmse, rsq)
# data split 
## train & test
  set.seed(1024)
  data_split <- initial_split(data)
  
  data_train <- training(data_split)
  data_test <- testing(data_split)
## cv 
  set.seed(1024)
  data_rs <- vfold_cv(data_train,v = 5, strata = truth)
  lgbm_spec <-
    boost_tree(
      trees = 300,
      #tune(),# trees(range = c(300, 1500), trans = NULL),
      tree_depth = 30,
      # tree_depth(range = c(7, 15L), trans = NULL),
      mtry = 0.8,
      learn_rate = 0.1,
      stop_iter = 10,
      min_n = 100,
      #tune(), # min_n(range = c(50, 200), trans = NULL),
      sample_size = 0.8,
      loss_reduction = 0.001
    ) |>
    set_engine(
      "lightgbm",
      #force_row_wise=TRUE,
      verbose = 1,
      num_leaves = 900,
      #num_leaves(range = c(500, 2048), trans = NULL),#tune(),
      lambda_l2 = 0.1,
      #tune(),
      lambda_l1 = 0.1,
      # tune()#,
      num_threads = 14,
      count = FALSE
    ) |>
    set_mode("regression") 
 #  
 # lgbm_grid <- 
 #   lgbm_spec |>
 #   extract_parameter_set_dials()|>
 #   update(#trees = 1200, #trees(range = c(300, 1500), trans = NULL),
 #          tree_depth = tree_depth(range = c(15, 25), trans = NULL),
 #          #min_n = 128, #min_n(range = c(50, 200), trans = NULL),
 #          num_leaves = num_leaves(range = c(500, 2500), trans = NULL))|>
 #   grid_space_filling(size=5)
 
 lgbm_wflow <- workflow(rcp, lgbm_spec)
 
 print(paste('start fitting',now()))
 
 fit_wf <- 
   lgbm_wflow |> 
   fit({{data}})
 
 # show_best(fit_wf, metric = "rmse") |> print()


 fit_wf |> saveRDS(file = 'lgb_fit.RDS')
 print(paste('Complete fitting', now()))
 return(fit_wf)
 # # 
 # return(lgbm_race_res)
 # ctrl <- control_race(verbose=TRUE,
 #                      verbose_elim = TRUE,
 #                      save_pred = TRUE,
 #                      save_workflow = TRUE)
 # 
# 
#  cores <- parallelly::availableCores(logical = FALSE)
#  cl <- parallel::makePSOCKcluster(cores)
#  doParallel::registerDoParallel(cl)

 set.seed(9)
 
 # lgbm_race_res <-
 #   lgbm_wflow %>%
 #   tune_race_anova(
 #     resamples = data_rs,
 #     grid = lgbm_grid,
 #     metrics = reg_metrics,
 #     control = ctrl)
 

 # foreach::registerDoSEQ()
 # parallel::stopCluster(cl)

 # # show_best(lgbm_race_res, metric = "rmse") |> print()
 # # 
 # # plot_race(lgbm_race_res) + 
 # #   scale_x_continuous(breaks = pretty_breaks())|>print()
 # # 
 # # lgbm_race_res|> saveRDS(file= 'lgb_res.RDS')
 # # print(paste('Complete fitting',now()))
 # # 
 # return(lgbm_race_res)
}

get_input_rcp_list <- function(df){
  rcp_v1.13 <- df |> get_rcp_v1.13_cut()
  
  rcp_list <- list(v1.13 = rcp_v1.13)
  
  return(rcp_list)
}

get_input_eng_list <- function() {
  lm_eng <- get_lm_eng()
  lgb_eng <- get_lgb_eng()
  eng_list <- list(#lgb = lgb_eng, 
                   linear = lm_eng)
  return(eng_list)
}

get_fit_wset <- function(df, rcps, cv = 3, init_seed = 1234,is_save=FALSE) {
  
  
  chi_models <-
    workflow_set(preproc =rcps,
                 models = get_input_eng_list(),
                 cross = TRUE) |>
    option_add(control = control_grid(save_workflow =TRUE ))
  
  fit_chi_models <-
    chi_models %>%
    # The first argument is a function name from the {{tune}} package
    # such as `tune_grid()`, `fit_resamples()`, etc.
    workflow_map(
      fn = "fit_resamples",
      resamples = vfold_cv(df, v = cv),
      metrics = metric_set(rmse, rsq),
      seed = init_seed,
      verbose = TRUE
    )
  
  # print the best rank result of rmse
  best_result <- 
    rank_results(fit_chi_models,  rank_metric = "rmse",  select_best = TRUE) |>
    select(rank, mean, model, wflow_id, .config)
  print(best_result) 
  # plot all the resmaple fit result 
  autoplot(fit_chi_models)|>print()
  best_param <- 
    fit_chi_models |>extract_workflow_set_result()
  print('found the best model and finalizing now.')
  
  # best_wf <-
  #   fit_chi_models |>
  #   fit_best(metric = "rmse", verbose = TRUE)
  
  return(fit_chi_models)
  
}

get_tune_grid <- function(){
  
  params <- parameters(
    trees(range = c(300,700 )),  # Number of trees (500 to 1500)
    num_leaves(range=c(700,1000)),
    tree_depth(range = c(10, 50)), # Tree depth (4 to 8)
    min_n(range=c(50,300))
    #learn_rate(range = c(-2, -1)) # Learning rate (log scale: 0.001 to 0.1)
  )
  grid <- grid_space_filling(params,size=10)
  return(grid)
}

get_tuned_wset <- function(cv = 3, init_seed = 1234) {
  df <- 
    get_train()|>head(20000)|>
    get_enrich_df()
  
  chi_models <-
    workflow_set(preproc = get_input_rcp_list(df) ,
                 models = get_tuned_eng_list(),
                 cross = TRUE) |>
    option_add(control = control_grid(save_workflow = TRUE))
  
  fit_chi_models <-
    chi_models %>%
    # The first argument is a function name from the {{tune}} package
    # such as `tune_grid()`, `fit_resamples()`, etc.
    workflow_map(
      fn = "fit_resmaples",
      resamples = vfold_cv(df, v = cv),
      #metrics = metric_set(rmse, rsq),
      #grid=get_tune_grid(),
      seed = init_seed,
      verbose = TRUE
    )
  
  # print the best rank result of rmse
  best_result <- 
    rank_results(fit_chi_models,  rank_metric = "rmse",  select_best = TRUE) |>
    select(rank, mean, model, wflow_id, .config)
  print(best_result) 
  # plot all the resmaple fit result 
  autoplot(fit_chi_models)
  
  print('found the best model and finalizing now.')
  
  best_wf <-
    fit_chi_models |>
    fit_best(metric = "rmse", verbose = TRUE)
  return(best_wf)
  
}

save_fited_workflow <-function(wf, 
                               data, 
                               name='unknown',
                               description='unknown',
                               is_log1p=TRUE){
  
  tidy_augment <- 
    wf |>
    fit({{data}})|>
    predict({{data}}) |>
    bind_cols({{data}})
  
  if (is_log1py == TRUE) {
    # if the outcome variable is log1p transformed in recipes.
    # the .pred , truth should be reverse to original scale.
    # as the log1p residual just minus
    tidy_augment <-
      tidy_augment |>
      mutate( .pred = exp(.pred) + 1)
  } 
  
  tidy_augment <-
    tidy_augment |>
    mutate(log1p_residual = log1p(.pred) - log1p(truth))
  
  rmsel_value <-
    tidy_augment  |>
    rmsel(truth, .pred) |>
    pull(.estimate)
  
  rmse_value <-
    tidy_augment |>
    rmse(truth, .pred) |>
    pull(.estimate)
  rsq_value <-
    tidy_augment |>
    rsq(truth, .pred) |>
    pull(.estimate)
  
  glance_mod <-
    data.frame(id = 0) |>
    mutate(rmse = rmse_value,
           rmsel = rmsel_value,
           rsq = rsq_value)
  
 
  log_residual <- 
    tidy_augment |> 
    dplyr::select(any_of(c('id', 'log1p_residual')))
  
  vetiver_mod <- vetiver_model(
    model = wf,
    model_name = {{name}} ,
    description = {{description}},
    metadata = list(metrics = glance_mod, residuals = log_residual)
  )
  
  vetiver_mod$metadata$user$metrics  |>
    pivot_longer(cols = everything()) |>
    filter(grepl('rmse|rsq', name)) |>
    print()
  
  keep_model(vetiver_mod)
  if (return_wf == TRUE) {
    result <- fit_wf
  } else{
    result <- fit_mod
  }
  return(result)
}

get_finalized_mod <- function(fitted_wfs,metric_name='rmse'){
  
  
  best_wf_id <- 
    fitted_wfs |>
    rank_results(rank_metric=metric_name,select_best=T) |>
    filter(.metric==metric_name) |>
    arrange(mean,std_err)|>
    head(1)|>
    pull(wflow_id) 
  
  wk <- 
    fitted_wfs |> 
    extract_workflow_set_result(best_wf_id) |>
    extract_workflow() 
  
  return(wf)
  
}
