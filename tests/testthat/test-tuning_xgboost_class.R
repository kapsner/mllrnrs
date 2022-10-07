library(mlbench)
data("DNA")
dataset <- DNA |>
  data.table::as.data.table() |>
  na.omit()

learner <- mllrnrs::LearnerXgboost
seed <- 123
feature_cols <- colnames(dataset)[160:180]

param_list_xgboost <- expand.grid(
  objective = "multi:softprob",
  eval_metric = "mlogloss",
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

test_that(
  desc = "test bayesian tuner, parameter_grid, multi:softprob - xgboost",
  code = {

    xgboost_tuner <- mlexperiments::MLTuneParameters$new(
      learner = learner,
      strategy = "bayesian",
      ncores = ncores,
      seed = seed
    )
    xgboost_tuner$parameter_grid <- param_list_xgboost
    xgboost_tuner$parameter_bounds <- xgboost_bounds
    xgboost_tuner$learner_args <- list(verbose = FALSE, num_class = 3)
    xgboost_tuner$optim_args <- optim_args

    # set data
    xgboost_tuner$set_data(
      x = train_x,
      y = train_y
    )

    tune_results <- xgboost_tuner$execute(k = 5)

    expect_type(tune_results, "list")
    expect_equal(dim(tune_results), c(ncores + 10, 19))
    expect_true(inherits(x = xgboost_tuner$results, what = "mlexTune"))
  }
)


test_that(
  desc = "test grid tuner, multi:softprob - xgboost",
  code = {

    xgboost_tuner <- mlexperiments::MLTuneParameters$new(
      learner = learner,
      strategy = "grid",
      ncores = ncores,
      seed = seed
    )
    set.seed(seed)
    random_grid <- sample(seq_len(nrow(param_list_xgboost)), 10)
    xgboost_tuner$parameter_grid <- param_list_xgboost[random_grid, ]
    xgboost_tuner$learner_args <- list(verbose = FALSE, num_class = 3)

    # set data
    xgboost_tuner$set_data(
      x = train_x,
      y = train_y
    )

    tune_results <- xgboost_tuner$execute(k = 5)
    expect_type(tune_results, "list")
    expect_equal(dim(tune_results), c(10, 12))
    expect_true(inherits(x = xgboost_tuner$results, what = "mlexTune"))
  }
)
