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
  #tmp_baked <- rcp|>prep()|>check_missing(all_predictors())|> bake(data) 
  
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


get_cut_pct <- function(df) {
  
  income_pct <- df |> pull(Annual.Income) |> quantile(
    x = _,
    probs = seq(0, 1, 0.05),
    na.rm = TRUE,
    include.lowest = T
  )
  age_pct <- df|> pull(Age) |> quantile(
    x = _,
    probs = seq(0, 1, 0.1),
    na.rm = TRUE,
    include.lowest = T
  )
  date_pct <- df|>pull(Policy.Start.Date)|> quantile(
    x=_, 
    probs=seq(0,1,0.05),
    na.rm = TRUE,
    include.lowest = T
  )
  Credit_pct <-df |>pull(Credit.Score)|> quantile(
    x=_, 
    probs=seq(0,1,0.1),
    na.rm = TRUE,
    include.lowest = T
  )
  Health_pct <- df|>pull(Health.Score)|> quantile(
    x=_, 
    probs=seq(0,1,0.1),
    na.rm = TRUE,
    include.lowest = T
  )
 
  result<-list('income_pct'=income_pct,
               'age_pct'=age_pct,
               'date_pct'=date_pct,
               'Credit_pct'=Credit_pct,
               'Health_pct'=Health_pct)
 return(result) 
}

get_grp_feature<- function(df){
  grp_col <-c('Gender','Marital.Status',
              'Number.of.Dependents', 'Education.Level',
              'Occupation','Location', 'Policy.Type',
              'Smoking.Status',
              'Exercise.Frequency',
              'Property.Type' )
  result_pct <- df|>get_cut_pct() 
  tmp_df <- df|>
    collect()|>
    mutate( Annual.Income_cut = cut(Annual.Income,  breaks = result_pct$income_pct, include.lowest=TRUE),
            Credit.Score_cut = cut(Credit.Score,  breaks = result_pct$Credit_pct,  include.lowest=TRUE),
            Health.Score_cut = cut(Health.Score,  breaks = result_pct$Health_pct,  include.lowest=TRUE),
            Age_cut = cut(Age,  breaks = result_pct$age_pct,  include.lowest=TRUE)
            
    )
  grp_df <-
    tmp_df |>
    group_by(across(all_of(grp_col)),.drop=TRUE ) |>
    summarise(
      cnt=n(),
      mean_income=mean(Annual.Income,na.rm=TRUE),
      mean_age=mean(Age,na.rm=TRUE),
      min_premium = mean(Premium.Amount),
      max_premium = max(Premium.Amount),
      sd_premium =  coalesce(sd(Premium.Amount),0),
      ids=list(id),
      .groups = 'drop') |>
    ungroup()|>
    select(-c(grp_col))|>
    unnest_longer(col=ids,values_to='id',indices_to='id_seq')|>
    arrange(id)
  
  
  return(grp_df)
}
