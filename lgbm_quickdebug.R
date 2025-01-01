library(tidymodels)
library(workflowsets)
library(bonsai)  # 导入 bonsai 以支持 lightgbm
library(glmnet)  # 导入 glmnet 以支持 lasso
library(doParallel)  # 导入 doParallel 以支持并行计算

# 1. 创建模拟数据（10 万行）
set.seed(123)
n <- 100000  # 数据量
data <- tibble(
  price = rlnorm(n, meanlog = 6, sdlog = 0.5),  # 价格，对数正态分布，meanlog = 6，sdlog = 0.5
  gender = sample(c("Male", "Female"), n, replace = TRUE),
  age = rnorm(n, mean = 40, sd = 10),  # 年龄，正态分布，均值 40，标准差 10
  major = sample(c("Engineering", "Arts", "Science"), n, replace = TRUE),
  education = sample(c("High School", "Bachelor", "Master", "PhD"), n, replace = TRUE),
  income = rexp(n, rate = 1/50000),  # 收入，指数分布，均值 50000
  health_score = rexp(n, rate = 1/50),  # 健康积分，指数分布，均值 50
  credit_score = rexp(n, rate = 1/500),  # 信用积分，指数分布，均值 500
  occupation = sample(c("Engineer", "Artist", "Scientist", "Teacher"), n, replace = TRUE),
  product_category = sample(c("Electronics", "Clothing", "Books", "Home"), n, replace = TRUE),
  service_level = sample(c("Basic", "Standard", "Premium"), n, replace = TRUE)
)

# 2. 数据分割
data_split <- initial_split(data, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# 3. 定义两个 recipe
# Recipe 1: 原始数据预处理
recipe_original <- recipe(price ~ ., data = train_data) %>%
  step_novel(all_nominal_predictors()) %>%  # 处理新类别值
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%  # 移除零方差列
  step_normalize(all_numeric_predictors())

# Recipe 2: 对响应变量进行 log1p 转换
recipe_log1p <- recipe(price ~ ., data = train_data) %>%
  step_novel(all_nominal_predictors()) %>%  # 处理新类别值
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%  # 移除零方差列
  step_normalize(all_numeric_predictors()) %>%
  step_log(price, base = exp(1), offset = 1, skip = TRUE)  # skip = TRUE 避免预测时出错

# 4. 定义模型
linear_model <- linear_reg() %>%
  set_engine("lm")

lightgbm_model <- boost_tree(
  trees = 300,  # 设置树的数量
  tree_depth = 18,  # 设置树的深度
  min_n = 50  # 设置每个节点的最小样本数
) %>%
  set_engine("lightgbm", num_leaves = 900) %>%  # 设置叶子节点数
  set_mode("regression")

lasso_model <- linear_reg(penalty = 0.1, mixture = 1) %>%  # mixture = 1 表示 Lasso
  set_engine("glmnet")

# 5. 创建工作流集
workflow_set <- workflow_set(
  preproc = list(original = recipe_original, log1p = recipe_log1p),
  models = list(linear = linear_model, lightgbm = lightgbm_model, lasso = lasso_model)
)

# 6. 设置并行计算（仅在 workflow_map 前后）
cores <- 16  # 使用 16 核 CPU
cl <- makePSOCKcluster(cores)  # 创建并行集群
registerDoParallel(cl)  # 注册并行后端

# 7. 训练模型（并行计算）
results <- workflow_set %>%
  workflow_map("fit_resamples", resamples = vfold_cv(train_data, v = 5), verbose = TRUE)

# 8. 关闭并行计算
stopCluster(cl)

# 9. 评估模型
metrics <- results %>%
  collect_metrics()
print(metrics)

# 10. 动态找到最佳工作流
best_workflow_info <- metrics %>%
  filter(.metric == "rmse") %>%  # 选择 rmse 作为评估指标
  arrange(mean) %>%  # 按 rmse 升序排列
  slice(1)  # 选择 rmse 最小的工作流

best_workflow_id <- best_workflow_info$wflow_id  # 最佳工作流的 ID
best_workflow_config <- best_workflow_info$.config  # 最佳工作流的配置

print(paste("Best workflow ID:", best_workflow_id))
print(paste("Best workflow config:", best_workflow_config))

# 11. 提取最佳工作流并训练最终模型
best_workflow <- results %>%
  extract_workflow(best_workflow_id) %>%
  finalize_workflow(best_workflow_info)  # 根据最佳配置调整工作流

final_model <- fit(best_workflow, train_data)

# 12. 进行预测
predictions <- predict(final_model, test_data)

# 13. 使用 expm1 将预测值恢复为原始比例
predictions <- predictions %>%
  mutate(.pred = expm1(.pred))  # expm1 反向转换

# 14. 查看预测结果
print(predictions)

# 15. 保存模型（可选）
saveRDS(final_model, "final_model.rds")