library(vetiver)
library(pins)
library(dplyr)
library(tidyr)

get_model_board <- function(){
  board_path <- './vetiver/'
  model_board <- board_folder(path = board_path,
                              versioned = TRUE)
  
}



# save the trained workflow into vetiver board.
keep_model_full <- function(mod=NULL, compress=TRUE){
# the input is the trained workflow
# the output is print feedback "keep model done!' or 'failed'
# the keep models should include the metrics of rmse, rsq and the residual data. those data stored in meta data field
# the model name should be the model name of the workflow and add version info makesure name is unique
# the model should be stored in the vetiver board, the vetiver board interface can be can from get_model_board function 
# the model should be stored in the vetiver board, the vetiver board interface can be can from get_model_board function

  # check the input model status  
  if (is.null(mod)) {
    stop('pls input a fitted mod')
  }
  
  # predict the model based on the function of get_train and get_enrich_df. 
  # get the data from get_train and get_enrich_df 
  data <- 
    get_train() |>
    get_enrich_df()
  
  # predict the resul
  pred <- 
    predict(mod, data) |>
    bind_cols(data)|>
    select(id,pred,truth)
  
  # basd on the predict and original data, let's calc the rmse , rsq
  metrics <- 
    pred |>
    yardstick::metrics(truth = truth, estimate = .pred) |>
    filter(.metric %in% c('rmse','rsq')) |>
    select(.metric,.estimate)
  
  # get the residual data
  residuals <- 
    pred |>
    select(.actual,.pred) |>
    mutate(residuals = .actual - .pred) |>
    select(residuals) 
  
  
  # get the vetiver board by get_mod_board function
  board <- bet_model_board()
  
  # save vetiver expert, pls help to versioning, storing the model based on the vetiver library getting start 
  # Create a vetiver object for deployment of above trained model
  # Create a vetiver_model() object based on about model ,name, rmse score and residual
  vetiver_model <- vetiver_model(model = mod,
                                 versioned = ,description = ,
                                 nameOfClass()
                                  metrics, residuals)
  
  
  
  
  get_model_board()|>
    vetiver_pin_write(mod)
  
  # prepare the vetiver model based on trained model ,metrics and residual
  vetiver_model <- 
    list(
      model = mod,
      metrics = metrics,
      residuals = residuals
    )
  # q above code throw Error in `list_modify()`:
  # q ! `.x` must be a list, not NULL.
  # q Run `rlang::last_trace()` to see where the error occurred.
  # q 警告信息:
  # q  Unknown or uninitialised column: `metadata`. 
 
  
  # write the vetiver model to the board
  get_model_board()|>
    vetiver_pin_write(vetiver_model) 
  
  # print if the keep model process is successful or failed
  print('keep model done!')

}

keep_model <- function(mod=NULL) {
  if (is.null(mod)) {
    stop('pls input a fitted mod')
  }
  get_model_board()|>
     vetiver_pin_write(mod) 
}

get_mods_log_residual<- function(mod_name_pattern = NULL) {
  model_board <- get_model_board()
  model_names <-
    model_board |>
    pin_list()|>
    set_names()
  
  if (!is.null(mod_name_pattern)){
    model_names <-
      model_names |>
      keep(\(name) grepl(mod_name_pattern,name))
  }
  
  metric_result <- 
    model_names |> 
    map(\(name)  
      model_board |> pin_meta(name) |>  pluck('user','residuals') |>  as_tibble()|>rename(value=log1p_residual) ) |>
    list_rbind( names_to = "name") #  per column list_cbind,   per vector list_c
  
}


show_model_permformance <- function(metric_name='rmsel'){
  model_board <- get_model_board()
  model_names <-
    model_board |>
    pin_list() |>
    set_names()
  
  metric_result <- 
    model_names |> 
    map(\(name)  
      model_board |>
          pin_meta(name) %>% 
          pluck('user','metrics') %>%
          as_tibble()
    ) %>%
    list_rbind( names_to = "id") #  per column list_cbind,   per vector list_c
  
  # metric_result %>% filter(.metric==metric_name) %>% arrange(.estimate)
  metric_result |>
    select(id,metric_name) |>
    rename(.estimate = metric_name) 
  
}

get_mod <- function(name){
  model_board <- get_model_board()
  vetiver_mod <- 
    model_board|>
    pin_read(name={{name}})
  return(vetiver_mod)
}
 