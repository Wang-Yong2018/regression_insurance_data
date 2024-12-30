# this is test file
library(testthat)
library(tidymodels)
source('etl.R')
# this is basic test function , it can check is testthat works
test_that("testthat works", {
  expect_true(TRUE)
})

# test a tidymodel workflow result can predict result 1.2M rows data with rmse score less thant 1.06
test_that("test tidymodel workflow", {
  # load data from etl.R get_enrich_df function
  # pls help to check the code below 
  data <- 
    get_train() |>
    get_enrich_df() 
  
  #data <- readRDS("data/data.rds")
  # load model
  model <- readRDS("test_model.rds.gz")
  # get predict score and bind_col pred to original data 
  rmse_score <- 
    model|>
    predict(data) |>
    bind_cols(data) |>
    yardstick::rmse(estimate=.pred, truth=truth)
  
  rmse_score <- rmse_score |>pull(.estimate)
  # check rmse score
  expect_true(rmse_score < 1.08)
})
