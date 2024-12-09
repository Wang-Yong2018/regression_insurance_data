library(yardstick)
library(tidymodels)

rmsel <- function(data, truth='truth', estimate='estimate',na_rm=TRUE){
  #  data|>glimpse()
  data|>
    select(truth={{truth}}, estimate={{estimate}})|>
    transmute(truth = log1p(truth),
              estimate = log1p(estimate) )|>
    rmse(truth, estimate) 
}

get_lm_wf<- function(){
  
  lm_spec <- 
    linear_reg() |>
    set_mode("regression") |>
    set_engine("lm")

  lm_wf <-
    workflow() |>
    add_model(lm_spec)
}



get_fit_wf <- function(rcp,data,name){
  
  lm_wf <- get_lm_wf()
  fit_mod <- lm_wf |>
    add_recipe(rcp)|>
    fit({{data}}) 
  
  result_df <- 
    fit_mod |> 
    augment(new_data = {{data}}) |>
    select(truth,.pred)
  
  rmsel_value <- 
    result_df |>
    rmsel(truth, .pred)|>
    
    pull(.estimate)
  rmse_value <- 
    result_df |>
    rmse(truth, .pred) |>
    pull(.estimate)
  
  rsq_value <- 
    result_df |> 
    rsq(truth, .pred) |>
    pull(.estimate)
  
  vetival_mod <-vetiver_model(model=fit_mod, 
                              model_name = paste0(name) ,
                              metadata = list(metrics=list(rmsel=rmsel_value,
                                                           rsq=rsq_value,
                                                           rmse=rmse_value)))
  
  return(vetival_mod)
}
