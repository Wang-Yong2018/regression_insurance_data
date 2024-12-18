library(bonsai)
lgbm_spec <- 
  boost_tree(trees = tune(), learn_rate = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("lightgbm", num_threads = 1)

lgbm_wflow <- workflow(hash_rec, lgbm_spec)
lgbm_wflow %>% 
  extract_parameter_set_dials()
#> Collection of 4 parameters for tuning
#> 
#>    identifier       type    object
#>         trees      trees nparam[+]
#>    learn_rate learn_rate nparam[+]
#>    agent hash  num_terms nparam[+]
#>  company hash  num_terms nparam[+]

# Individual functions: 
trees()
#> # Trees (quantitative)
#> Range: [1, 2000]
learn_rate()
#> Learning Rate (quantitative)
#> Transformer: log-10 [1e-100, Inf]
#> Range (transformed scale): [-10, -1]
#> 
#> set.seed(12)
grid <- 
  lgbm_wflow %>% 
  extract_parameter_set_dials() %>% 
  grid_space_filling(size = 25)



lgbm_param <- 
  lgbm_wflow %>% 
  extract_parameter_set_dials() %>% 
  update(trees = trees(c(1L, 100L)),
         learn_rate = learn_rate(c(-5, -1)))

set.seed(712)
grid <- 
  lgbm_param %>% 
  grid_space_filling(size = 25)


grid %>% 
  ggplot(aes(trees, learn_rate)) +
  geom_point(size = 4) +
  scale_y_log10()


lgbm_spec <- 
  boost_tree(trees = tune(), learn_rate = tune(),  min_n = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("lightgbm", num_threads = 1)

lgbm_wflow <- workflow(hash_rec, lgbm_spec)

# Update the feature hash ranges (log-2 units)
lgbm_param <-
  lgbm_wflow %>%
  extract_parameter_set_dials() %>%
  update(`agent hash`   = num_hash(c(3, 8)),
         `company hash` = num_hash(c(3, 8)))

set.seed(9)
ctrl <- control_grid(save_pred = TRUE)

lgbm_res <-
  lgbm_wflow %>%
  tune_grid(
    resamples = hotel_rs,
    grid = 25,
    # The options below are not required by default
    param_info = lgbm_param, 
    control = ctrl,
    metrics = reg_metrics
  )



lgbm_res 
#> # Tuning results
#> # 10-fold cross-validation using stratification 
#> # A tibble: 10 × 5
#>    splits             id     .metrics          .notes           .predictions        
#>    <list>             <chr>  <list>            <list>           <list>              
#>  1 <split [3372/377]> Fold01 <tibble [50 × 9]> <tibble [0 × 3]> <tibble [9,425 × 9]>
#>  2 <split [3373/376]> Fold02 <tibble [50 × 9]> <tibble [0 × 3]> <tibble [9,400 × 9]>
#>  3 <split [3373/376]> Fold03 <tibble [50 × 9]> <tibble [0 × 3]> <tibble [9,400 × 9]>
#>  4 <split [3373/376]> Fold04 <tibble [50 × 9]> <tibble [0 × 3]> <tibble [9,400 × 9]>
#>  5 <split [3373/376]> Fold05 <tibble [50 × 9]> <tibble [0 × 3]> <tibble [9,400 × 9]>
#>  6 <split [3374/375]> Fold06 <tibble [50 × 9]> <tibble [0 × 3]> <tibble [9,375 × 9]>
#>  7 <split [3375/374]> Fold07 <tibble [50 × 9]> <tibble [0 × 3]> <tibble [9,350 × 9]>
#>  8 <split [3376/373]> Fold08 <tibble [50 × 9]> <tibble [0 × 3]> <tibble [9,325 × 9]>
#>  9 <split [3376/373]> Fold09 <tibble [50 × 9]> <tibble [0 × 3]> <tibble [9,325 × 9]>
#> 10 <split [3376/373]> Fold10 <tibble [50 × 9]> <tibble [0 × 3]> <tibble [9,325 × 9]>
#> 
#> 
#> 
#> lgbm_best <- select_best(lgbm_res, metric = "mae")
lgbm_bestdata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCC
#> # A tibble: 1 × 6
#>   trees min_n learn_rate `agent hash` `company hash` .config              
#>   <int> <int>      <dbl>        <int>          <int> <chr>                
#> 1  1890    10     0.0159          115            174 Preprocessor12_Model1
