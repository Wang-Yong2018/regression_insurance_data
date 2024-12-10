library(yardstick)
library(tidymodels)
library(broom)



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
  fit_wf <- lm_wf |>
    add_recipe(rcp)|>
    fit({{data}}) 
  
  fit_mod <- 
    fit_wf |> 
    extract_fit_engine()
  
  tidy_mod <- fit_mod |> broom::tidy()
  glance_mod <- fit_mod |> broom::glance()
  tidy_augment <-
    fit_mod |> 
    broom::augment() |>
    select(truth=`..y`,
           .pred=`.fitted`)|>
    mutate(log1p_residual=log1p(.pred)-log1p(truth))
  
  rmsel_value <- 
    tidy_augment  |>
    rmsel(truth, .pred)|>
    pull(.estimate)
  
  rmse_value <- 
    tidy_augment |>
    rmse(truth, .pred) |>
    pull(.estimate)
  
  glance_mod <- 
    glance_mod|>
    mutate(rmse=rmse_value,
           rmsel=rmsel_value)
  
  fit_mod$qr <- NULL
 # fit_mod$residuals <- NULL
  fit_mod$log_residual <- tidy_augment|>pull(log1p_residual)
  fit_mod$effects <- NULL
  fit_mod$na.action <- NULL
  fit_mod$model <- NULL
  
  
  vetiver_mod <-vetiver_model(model=fit_mod, 
                              model_name = paste0(name) ,
                              metadata = list(metrics=glance_mod))
  keep_model(vetiver_mod)
  return(vetiver_mod)
}

