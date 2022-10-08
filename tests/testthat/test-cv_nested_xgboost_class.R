library(mlbench)
data("DNA")
dataset <- DNA |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[160:180]

param_list_xgboost <- expand.grid(
  subsample = seq(0.6, 1, .2),
  colsample_bytree = seq(0.6, 1, .2),
  min_child_weight = seq(1, 5, 4),
  learning_rate = seq(0.1, 0.2, 0.1),
  max_depth = seq(1, 5, 4)
)
xgboost_bounds <- list(
  subsample = c(0.2, 1),
  colsample_bytree = c(0.2, 1),
  min_child_weight = c(1L, 10L),
  learning_rate = c(0.1, 0.2),
  max_depth =  c(1L, 10L)
)

ncores <- ifelse(
  test = parallel::detectCores() > 4,
  yes = 4L,
  no = ifelse(
    test = parallel::detectCores() < 2L,
    yes = 1L,
    no = parallel::detectCores()
  )
)
optim_args <- list(
  iters.n = ncores,
  kappa = 3.5,
  acq = "ucb"
)

train_x <- model.matrix(
  ~ -1 + .,
  dataset[, .SD, .SDcols = feature_cols]
)
train_y <- as.integer(dataset[, get("Class")]) - 1L

options("mlexperiments.bayesian.max_init" = 10L)
options("mlexperiments.optim.xgb.nrounds" = 100L)
options("mlexperiments.optim.xgb.early_stopping_rounds" = 10L)

fold_list <- splitTools::create_folds(
  y = train_y,
  k = 3,
  type = "stratified",
  seed = seed
)


test_that(
  desc = "test nested cv, bayesian, multi:softprob - xgboost",
  code = {

    xgboost_optimization <- mlexperiments::MLNestedCV$new(
      learner = mllrnrs::LearnerXgboost$new(),
      strategy = "bayesian",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    xgboost_optimization$parameter_bounds <- xgboost_bounds
    xgboost_optimization$parameter_grid <- param_list_xgboost
    xgboost_optimization$split_type <- "stratified"
    xgboost_optimization$optim_args <- optim_args

    xgboost_optimization$learner_args <- list(
      objective = "multi:softprob",
      eval_metric = "mlogloss",
      num_class = 3
    )
    xgboost_optimization$predict_args <- list(reshape = TRUE)
    xgboost_optimization$performance_metric <- mlexperiments::metric("bacc")
    xgboost_optimization$performance_metric_name <- "Balanced accuracy"

    # set data
    xgboost_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- xgboost_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 10))
    expect_true(inherits(
      x = xgboost_optimization$results,
      what = "mlexCV"
    ))
  }
)


test_that(
  desc = "test nested cv, grid - xgboost",
  code = {

    xgboost_optimization <- mlexperiments::MLNestedCV$new(
      learner = mllrnrs::LearnerXgboost$new(),
      strategy = "grid",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )
    set.seed(seed)
    random_grid <- sample(seq_len(nrow(param_list_xgboost)), 10)
    xgboost_optimization$parameter_grid <-
      param_list_xgboost[random_grid, ]
    xgboost_optimization$split_type <- "stratified"

    xgboost_optimization$learner_args <- list(
      objective = "multi:softprob",
      eval_metric = "mlogloss",
      num_class = 3
    )
    xgboost_optimization$predict_args <- list(reshape = TRUE)
    xgboost_optimization$performance_metric <- mlexperiments::metric("bacc")
    xgboost_optimization$performance_metric_name <- "Balanced accuracy"

    # set data
    xgboost_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- xgboost_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 10))
    expect_true(inherits(
      x = xgboost_optimization$results,
      what = "mlexCV"
    ))
  }
)
