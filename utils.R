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



get_fit_wf <- function(rcp,data,name='',description='', is_log1py=FALSE,return_wf=FALSE){
  
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
           .pred=`.fitted`)
  
 if (is_log1py==TRUE) {
   # if the outcome variable is log1p transformed in recipes.
   # the .pred , truth should be reverse to original scale.
   # as the log1p residual just minus
  tidy_augment <-
    tidy_augment |>
    mutate(log1p_residual=.pred-truth,
           .pred=exp(.pred)+1,
           truth=exp(truth)+1)
 } else(
   
  tidy_augment <-
    tidy_augment |>
    mutate(log1p_residual=log1p(.pred)-log1p(truth))
 )
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
                              model_name = name ,
                              description = {{description}},
                              metadata = list(metrics=glance_mod))
  
  vetiver_mod$metadata$user$metrics  |>
    pivot_longer(cols=everything()) |>
    filter(grepl('rmse|r.squared|nob',name)) |>
    print()
  
  keep_model(vetiver_mod)
  if(return_wf==TRUE){
    result <- fit_wf
  } else{
    result <- fit_mod
  }
  return(result)
}


