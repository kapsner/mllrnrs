library(mlbench)
data("DNA")
dataset <- DNA |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[160:180]

param_list_glmnet <- expand.grid(
  alpha = seq(0, 1, 0.05)
)

if (isTRUE(as.logical(Sys.getenv("_R_CHECK_LIMIT_CORES_")))) {
  # on cran
  ncores <- 2L
} else {
  ncores <- ifelse(
    test = parallel::detectCores() > 4,
    yes = 4L,
    no = ifelse(
      test = parallel::detectCores() < 2L,
      yes = 1L,
      no = parallel::detectCores()
    )
  )
}

train_x <- model.matrix(
  ~ -1 + .,
  dataset[, .SD, .SDcols = feature_cols]
)
train_y <- as.integer(dataset[, get("Class")]) - 1L

options("mlexperiments.bayesian.max_init" = 4L)

fold_list <- splitTools::create_folds(
  y = train_y,
  k = 3,
  type = "stratified",
  seed = seed
)


# ###########################################################################
# %% glmnet
# ###########################################################################

# ###########################################################################
# %% NESTED CV
# ###########################################################################

test_that(desc = "test nested cv, grid, multiclass - glmnet", code = {
  skip_on_cran()
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("measures")

  glmnet_optimizer <- mlexperiments::MLNestedCV$new(
    learner = mllrnrs::LearnerGlmnet$new(
      metric_optimization_higher_better = FALSE
    ),
    strategy = "grid",
    fold_list = fold_list,
    k_tuning = 3L,
    ncores = ncores,
    seed = seed
  )
  set.seed(seed)
  random_grid <- sample(seq_len(nrow(param_list_glmnet)), 3)
  glmnet_optimizer$parameter_grid <- kdry::mlh_subset(
    param_list_glmnet,
    random_grid
  )
  glmnet_optimizer$split_type <- "stratified"

  y_weights <- ifelse(train_y == 1, 0.8, ifelse(train_y == 2, 1.2, 1))
  glmnet_optimizer$learner_args <- list(
    family = "multinomial",
    type.measure = "class",
    standardize = TRUE,
    case_weights = y_weights
  )
  glmnet_optimizer$predict_args <- list(type = "response", reshape = TRUE)
  glmnet_optimizer$performance_metric <- mlexperiments::metric("ACC")

  # set data
  glmnet_optimizer$set_data(
    x = train_x,
    y = train_y
  )

  cv_results <- glmnet_optimizer$execute()
  expect_type(cv_results, "list")
  expect_equal(dim(cv_results), c(3, 7))
  expect_true(inherits(
    x = glmnet_optimizer$results,
    what = "mlexCV"
  ))
})


# ###########################################################################
# %% Lightgbm
# ###########################################################################

param_list_lightgbm <- expand.grid(
  bagging_fraction = seq(0.6, 1, .2),
  feature_fraction = seq(0.6, 1, .2),
  min_data_in_leaf = seq(2, 10, 2),
  learning_rate = seq(0.1, 0.2, 0.1),
  num_leaves = seq(2, 20, 4),
  max_depth = -1L,
  verbose = -1L
)

options("mlexperiments.bayesian.max_init" = 4L)
options("mlexperiments.optim.lgb.nrounds" = 20L)
options("mlexperiments.optim.lgb.early_stopping_rounds" = 5L)

# ###########################################################################
# %% NESTED CV
# ###########################################################################

test_that(desc = "test nested cv, grid, multiclass - lightgbm", code = {
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("measures")

  lightgbm_optimizer <- mlexperiments::MLNestedCV$new(
    learner = mllrnrs::LearnerLightgbm$new(
      metric_optimization_higher_better = FALSE
    ),
    strategy = "grid",
    fold_list = fold_list,
    k_tuning = 3L,
    ncores = ncores,
    seed = seed
  )
  set.seed(seed)
  random_grid <- sample(seq_len(nrow(param_list_lightgbm)), 3)
  lightgbm_optimizer$parameter_grid <-
    param_list_lightgbm[random_grid, ]
  lightgbm_optimizer$split_type <- "stratified"

  y_weights <- ifelse(train_y == 1, 0.8, ifelse(train_y == 2, 1.2, 1))
  lightgbm_optimizer$learner_args <- list(
    objective = "multiclass",
    metric = "multi_logloss",
    num_class = 3,
    case_weights = y_weights
  )
  lightgbm_optimizer$predict_args <- list(reshape = TRUE)
  lightgbm_optimizer$performance_metric <- mlexperiments::metric("ACC")

  # set data
  lightgbm_optimizer$set_data(
    x = train_x,
    y = train_y
  )

  cv_results <- lightgbm_optimizer$execute()
  expect_type(cv_results, "list")
  expect_equal(dim(cv_results), c(3, 13))
  expect_true(inherits(
    x = lightgbm_optimizer$results,
    what = "mlexCV"
  ))
})


# ###########################################################################
# %% Ranger
# ###########################################################################

param_list_ranger <- expand.grid(
  num.trees = seq(500, 1000, 500),
  mtry = seq(2, 6, 2),
  min.node.size = seq(1, 9, 4),
  max.depth = seq(1, 9, 4),
  sample.fraction = seq(0.5, 0.8, 0.3)
)


# ###########################################################################
# %% NESTED CV
# ###########################################################################

test_that(desc = "test nested cv, grid, regression - ranger", code = {
  testthat::skip_if_not_installed("ranger")
  testthat::skip_if_not_installed("measures")

  ranger_optimizer <- mlexperiments::MLNestedCV$new(
    learner = mllrnrs::LearnerRanger$new(),
    strategy = "grid",
    fold_list = fold_list,
    k_tuning = 3L,
    ncores = ncores,
    seed = seed
  )
  set.seed(seed)
  random_grid <- sample(seq_len(nrow(param_list_ranger)), 3)
  ranger_optimizer$parameter_grid <-
    param_list_ranger[random_grid, ]
  ranger_optimizer$split_type <- "stratified"

  y_weights <- ifelse(train_y == 1, 0.8, ifelse(train_y == 2, 1.2, 1))
  ranger_optimizer$learner_args <- list(
    classification = TRUE,
    case_weights = y_weights
  )

  ranger_optimizer$performance_metric <- mlexperiments::metric("ACC")

  # set data
  ranger_optimizer$set_data(
    x = train_x,
    y = train_y
  )

  cv_results <- ranger_optimizer$execute()
  expect_type(cv_results, "list")
  expect_equal(dim(cv_results), c(3, 8))
  expect_true(inherits(
    x = ranger_optimizer$results,
    what = "mlexCV"
  ))
})


# ###########################################################################
# %% xgboost
# ###########################################################################

param_list_xgboost <- expand.grid(
  subsample = seq(0.6, 1, .2),
  colsample_bytree = seq(0.6, 1, .2),
  min_child_weight = seq(1, 5, 4),
  learning_rate = seq(0.1, 0.2, 0.1),
  max_depth = seq(1, 5, 4)
)

ncores <- 2L

options("mlexperiments.bayesian.max_init" = 10L)
options("mlexperiments.optim.xgb.nrounds" = 20L)
options("mlexperiments.optim.xgb.early_stopping_rounds" = 5L)


# ###########################################################################
# %% NESTED CV
# ###########################################################################

test_that(desc = "test nested cv, grid, multi:softprob - xgboost, with weights", code = {
  testthat::skip_if_not_installed("xgboost")
  testthat::skip_if_not_installed("measures")

  xgboost_optimizer <- mlexperiments::MLNestedCV$new(
    learner = mllrnrs::LearnerXgboost$new(
      metric_optimization_higher_better = FALSE
    ),
    strategy = "grid",
    fold_list = fold_list,
    k_tuning = 3L,
    ncores = ncores,
    seed = seed
  )
  set.seed(seed)
  random_grid <- sample(seq_len(nrow(param_list_xgboost)), 3)
  xgboost_optimizer$parameter_grid <-
    param_list_xgboost[random_grid, ]
  xgboost_optimizer$split_type <- "stratified"

  y_weights <- ifelse(train_y == 1, 0.8, ifelse(train_y == 2, 1.2, 1))
  xgboost_optimizer$learner_args <- list(
    objective = "multi:softprob",
    eval_metric = "mlogloss",
    num_class = 3,
    case_weights = y_weights
  )
  xgboost_optimizer$predict_args <- list(reshape = TRUE)
  xgboost_optimizer$performance_metric <- mlexperiments::metric("ACC")

  # set data
  xgboost_optimizer$set_data(
    x = train_x,
    y = train_y
  )

  cv_results <- xgboost_optimizer$execute()
  expect_type(cv_results, "list")
  expect_equal(dim(cv_results), c(3, 11))
  expect_true(inherits(
    x = xgboost_optimizer$results,
    what = "mlexCV"
  ))
})
