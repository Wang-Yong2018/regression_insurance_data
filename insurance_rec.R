library(tidymodels)
library(recipes)
library(dplyr)
get_rcp_v1.13_cut <- function(df){
  
rcp <-
  
  recipes::recipe(truth~., df) |>
  step_log(all_outcomes(), offset = 1) |>
  update_role(id, new_role='ID')|>
  #    step_date(date, features = c("dow", "month", "year")) |>
  step_impute_median(all_numeric_predictors()) |>
  step_integer(date)|>
  step_log(Annual.Income,offset=1)|>
  step_mutate(Annual.Income.raw=Annual.Income,
              Credit.Score.raw=Credit.Score,
              microsecond.raw=microsecond,
              Health.Score.raw=Health.Score)|>
  step_discretize(Annual.Income, num_breaks =30) |>
  step_discretize(Credit.Score,Health.Score,microsecond, num_breaks=10) |>
  step_dummy_multi_choice(Gender,  Marital.Status,
                          Education.Level, Occupation ,
                          Location,
                          Policy.Type,Customer.Feedback,
                          Smoking.Status ,Exercise.Frequency,
                          Property.Type)|>
  step_unknown(all_nominal_predictors()) |>
  step_corr(all_numeric_predictors()) |>
  step_pca(all_numeric_predictors(),num_comp = 50) |>
  step_nzv(all_predictors())
  
  return(rcp)
}