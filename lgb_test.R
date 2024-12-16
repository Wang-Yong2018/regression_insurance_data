#====================================
# data
library(AmesHousing)
# data cleaning
library(janitor)
# data prep
library(dplyr)
# visualisation
library(ggplot2)
# tidymodels
library(rsample)
library(recipes)
library(parsnip)
library(tune)
library(dials)
library(workflows)
library(yardstick)
library(bonsai)

#================================
## 
# speed up computation with parallel processing
library(doParallel)
all_cores <- parallel::detectCores(logical = FALSE)
registerDoParallel(cores = all_cores)

#================================
## 
ames_split <- rsample::initial_split(
  ames,
  prop = 0.8,
  strata = sale_price
)


#================================
## 

preprocessing_recipe <-
  recipes::recipe(sale_price ~ ., data = training(ames_split)) %>%
  # combine low frequency factor levels
  recipes::step_other(all_nominal(), threshold = 0.01) %>%
  # remove no variance predictors which provide no predictive information 
  recipes::step_nzv(all_nominal()) %>%
  # prep the recipe so it can be used on other data
  prep()

#================================
## 

ames_cv_folds <-
  recipes::bake(
    preprocessing_recipe,
    new_data = training(ames_split)
  ) %>%
  rsample::vfold_cv(v = 5)

#================================
## 

lightgbm_model<-
  parsnip::boost_tree(
    mode = "regression",
    trees = 1000,
    min_n = tune(),
    tree_depth = tune(),
  ) %>%
  set_engine("lightgbm", objective = "reg:squarederror",verbose=-1)

#================================
## 
lightgbm_params <-
  dials::parameters(
    # The parameters have sane defaults, but if you have some knowledge 
    # of the process you can set upper and lower limits to these parameters.
    min_n(), # 2nd important
    tree_depth() # 3rd most important
  )


#================================
## 
lgbm_grid <-
  dials::grid_max_entropy(
    lightgbm_params,
    size = 30 # set this to a higher number to get better results
    # I don't want to run this all night, so I set it to 30
  )
head(lgbm_grid)




#================================
## 
lgbm_wf <-
  workflows::workflow() %>%
  add_model(lightgbm_model
  ) %>%
  add_formula(sale_price ~ .)


#================================
## 
lgbm_tuned <- tune::tune_grid(
  object = lgbm_wf,
  resamples = ames_cv_folds,
  grid = lgbm_grid,
  metrics = yardstick::metric_set(rmse, rsq, mae),
  control = tune::control_grid(verbose = FALSE) # set this to TRUE to see
  # in what step of the process you are. But that doesn't look that well in
  # a blog.
)


#================================
## 
lgbm_tuned %>%
  tune::show_best(metric = "rmse",n = 5)

# # A tibble: 5 x 8
# min_n tree_depth .metric .estimator mean n std_err .config
# <int> <int> <chr> <chr> <dbl> <int> <dbl> <chr>
#   1 8 3 rmse standard 24555. 5 1192. Model30
# 2 13 4 rmse standard 24647. 5 1042. Model29
# 3 12 1 rmse standard 25195. 5 1281. Model04
# 4 19 5 rmse standard 25206. 5 1079. Model05
# 5 6 5 rmse standard 25382. 5 858. Model18
#================================
## 
lgbm_tuned %>%
  tune::show_best(metric = "rmse",n = 10) %>%
  tidyr::pivot_longer(min_n:tree_depth, names_to="variable",values_to="value" ) %>%
  ggplot(aes(value,mean)) +
  geom_line(alpha=1/2)+
  geom_point()+
  facet_wrap(~variable,scales = "free")+
  ggtitle("Best parameters for RMSE")
#================================
## 
lgbm_tuned %>%
  tune::show_best(metric = "mae",n = 10) %>%
  tidyr::pivot_longer(min_n:tree_depth, names_to="variable",values_to="value" ) %>%
  ggplot(aes(value,mean)) +
  geom_line(alpha=1/2)+
  geom_point()+
  facet_wrap(~variable,scales = "free")+
  ggtitle("Best parameters for MAE")
#================================
## 
lgbm_best_params <-
  lgbm_tuned %>%
  tune::select_best("rmse")
#================================
## 
lgbm_model_final <-
  lightgbm_model%>%
  finalize_model(lgbm_best_params)
lgbm_model_final
#========================================================================
## 
test_processed <- bake(preprocessing_recipe, new_data = testing(ames_split))
test_prediction <-
  trained_model_all_data %>%
  # use the training model fit to predict the test data
  predict(new_data = test_processed) %>%
  bind_cols(testing(ames_split))
#================================
## 
train_prediction %>%
  yardstick::metrics(sale_price, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ",")) %>%
  knitr::kable()
#================================
## 
test_prediction %>%
  yardstick::metrics(sale_price, .pred) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ",")) %>%
  knitr::kable()

#================================
## 
house_prediction_residual <- test_prediction %>%
  arrange(.pred) %>%
  mutate(residual_pct = (sale_price - .pred) / .pred) %>%
  select(.pred, residual_pct)

ggplot(house_prediction_residual, aes(x = .pred, y = residual_pct)) +
  geom_point() +
  xlab("Predicted Sale Price") +
  ylab("Residual (%)") +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::percent)