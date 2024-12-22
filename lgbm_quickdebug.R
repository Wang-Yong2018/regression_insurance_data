set.seed(1234)
categorical_vars<-c("Gender","Marital.Status", "Number.of.Dependents",
                    "Education.Level", "Occupation","Location", 
                    "Policy.Type", "Customer.Feedback" ,
                    "Smoking.Status", "Exercise.Frequency",
                    "Property.Type", "cnt")

lgbm_spec <- 
  boost_tree(trees = 300, #tune(),# trees(range = c(300, 1500), trans = NULL),
             tree_depth =30, # tree_depth(range = c(7, 15L), trans = NULL),
             mtry=0.8,
             learn_rate = 0.1,
             stop_iter= 20,
             min_n = 100,#tune(), # min_n(range = c(50, 200), trans = NULL),
             sample_size= 0.8,
             loss_reduction = 0.01
  )|>
  set_engine("lightgbm", 
             #force_row_wise=TRUE,
             verbose = 1,
             num_leaves = 90, #num_leaves(range = c(500, 2048), trans = NULL),#tune(),
             lambda_l2 = 0.2, #tune(),
             lambda_l1 = 0.2, # tune()#,
             count=FALSE,
             min_gain_to_split = 0.001,
             categorical_feature = categorical_vars,
             num_threads=14
  )|>
  set_mode("regression") 
lgbm_wflow <- workflow(rcp, lgbm_spec)
fit_lgb_wflow <-lgbm_wflow|>fit(data)
