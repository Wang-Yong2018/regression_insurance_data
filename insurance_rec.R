library(tidymodels)
library(recipes)
library(dplyr)

get_rcp_v1.14 <- function(df){
  
  rcp <-
    recipes::recipe(truth~., df) |>
    #step_log(all_outcomes(), offset = 1,skip=TRUE) |>
    update_role(id, new_role='ID')|>
    #    step_date(date, features = c("dow", "month", "year")) |>
    step_mutate(sin_ms = sin(2*pi * microsecond),
                cos_ms = cos(2*pi * microsecond),
                inv_year_ms =year * microsecond,
                angle_theta = atan2(year, microsecond),  # Angle
                magnitude = sqrt(year**2 + microsecond**2)  # Magnitude
    )|>
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

get_rcp_v1.13_cut <- function(df){
  
  rcp <-
    recipes::recipe(truth~., df) |>
    #step_log(all_outcomes(), offset = 1,skip=TRUE) |>
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

get_rcp_v0.9 <- function(df){
  rcp <- 
    recipes::recipe(truth ~.,df)|>
    #  step_log(all_outcomes(), offset = 1,skip=TRUE) |>
    step_date(date, features = c("dow", "month","year")) |>
    step_time(date, features= c('second'))|>
    step_rm(date)|>
    #step_holiday(`Policy.Start.Date`, holidays = timeDate::listHolidays()) |>
    step_impute_median(all_numeric_predictors())|>
    step_unknown(all_nominal_predictors()) |>
    step_mutate(
      date_year = as_factor(date_year),
      Annual.Income=log1p(Annual.Income),
    ) |>
    step_nzv()|>
    step_pca(num_comp=50)
}

get_rcp_v0.8 <- function(df){
  rcp <- 
    recipes::recipe(truth ~.,df)|>
    step_log(all_outcomes(), offset = 1,skip=TRUE) |>
    step_date(date, features = c("dow", "month","year")) |>
    step_rm(date)|>
    #step_holiday(`Policy.Start.Date`, holidays = timeDate::listHolidays()) |>
    step_impute_median(all_numeric_predictors())|>
    step_unknown(all_nominal_predictors()) |>
    step_mutate(
      date_year = as_factor(date_year),
      Annual.Income=log1p(Annual.Income),
    ) |>
    step_nzv()|>
    step_pca(num_comp=50)
  
}