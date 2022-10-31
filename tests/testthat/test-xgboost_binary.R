library(mlbench)
data("PimaIndiansDiabetes2")
dataset <- PimaIndiansDiabetes2 |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[1:8]

param_list_xgboost <- expand.grid(
  subsample = seq(0.6, 1, .2),
  colsample_bytree = seq(0.6, 1, .2),
  min_child_weight = seq(1, 5, 4),
  learning_rate = seq(0.1, 0.2, 0.1),
  max_depth = seq(1, 5, 4)
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
train_y <- as.integer(dataset[, get("diabetes")]) - 1L

options("mlexperiments.bayesian.max_init" = 10L)
options("mlexperiments.optim.xgb.nrounds" = 100L)
options("mlexperiments.optim.xgb.early_stopping_rounds" = 10L)

fold_list <- splitTools::create_folds(
  y = train_y,
  k = 3,
  type = "stratified",
  seed = seed
)


# ###########################################################################
# %% CV
# ###########################################################################

test_that(
  desc = "test cv, binary:logistic - xgboost",
  code = {

    xgboost_optimizer <- mlexperiments::MLCrossValidation$new(
      learner = mllrnrs::LearnerXgboost$new(
        metric_optimization_higher_better = FALSE
      ),
      fold_list = fold_list,
      ncores = ncores,
      seed = seed
    )
    xgboost_optimizer$learner_args <- c(
      as.list(
        data.table::data.table(
          param_list_xgboost[37, ],
          stringsAsFactors = FALSE
        ),
      ),
      list(
        objective = "binary:logistic",
        eval_metric = "logloss"
      ),
      nrounds = 45L
    )
    xgboost_optimizer$performance_metric_args <- list(positive = "1")
    xgboost_optimizer$performance_metric <- mlexperiments::metric("auc")

    # set data
    xgboost_optimizer$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- xgboost_optimizer$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 10))
    expect_true(inherits(
      x = xgboost_optimizer$results,
      what = "mlexCV"
    ))
  }
)


# ###########################################################################
# %% NESTED CV
# ###########################################################################

xgboost_bounds <- list(
  subsample = c(0.2, 1),
  colsample_bytree = c(0.2, 1),
  min_child_weight = c(1L, 10L),
  learning_rate = c(0.1, 0.2),
  max_depth =  c(1L, 10L)
)
optim_args <- list(
  iters.n = ncores,
  kappa = 3.5,
  acq = "ucb"
)

test_that(
  desc = "test bayesian tuner, parameter_grid, binary:logistic - xgboost",
  code = {

    xgboost_tuner <- mlexperiments::MLTuneParameters$new(
      learner = mllrnrs::LearnerXgboost$new(
        metric_optimization_higher_better = FALSE
      ),
      strategy = "bayesian",
      ncores = ncores,
      seed = seed
    )
    xgboost_tuner$parameter_grid <- param_list_xgboost
    xgboost_tuner$parameter_bounds <- xgboost_bounds
    xgboost_tuner$learner_args <- list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      verbose = FALSE
    )
    xgboost_tuner$optim_args <- optim_args

    # set data
    xgboost_tuner$set_data(
      x = train_x,
      y = train_y
    )

    tune_results <- xgboost_tuner$execute(k = 3)

    expect_type(tune_results, "list")
    expect_true(inherits(x = xgboost_tuner$results, what = "mlexTune"))
  }
)


test_that(
  desc = "test grid tuner, binary:logistic - xgboost",
  code = {

    xgboost_tuner <- mlexperiments::MLTuneParameters$new(
      learner = mllrnrs::LearnerXgboost$new(
        metric_optimization_higher_better = FALSE
      ),
      strategy = "grid",
      ncores = ncores,
      seed = seed
    )
    set.seed(seed)
    random_grid <- sample(seq_len(nrow(param_list_xgboost)), 10)
    xgboost_tuner$parameter_grid <- param_list_xgboost[random_grid, ]
    xgboost_tuner$learner_args <- list(
      objective = "binary:logistic",
      eval_metric = "logloss",
      verbose = FALSE
    )

    # set data
    xgboost_tuner$set_data(
      x = train_x,
      y = train_y
    )

    tune_results <- xgboost_tuner$execute(k = 3)
    expect_type(tune_results, "list")
    expect_equal(dim(tune_results), c(10, 11))
    expect_true(inherits(x = xgboost_tuner$results, what = "mlexTune"))
  }
)

# ###########################################################################
# %% NESTED CV
# ###########################################################################

test_that(
  desc = "test nested cv, bayesian, binary:logistic - xgboost",
  code = {

    xgboost_optimizer <- mlexperiments::MLNestedCV$new(
      learner = mllrnrs::LearnerXgboost$new(
        metric_optimization_higher_better = FALSE
      ),
      strategy = "bayesian",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    xgboost_optimizer$parameter_bounds <- xgboost_bounds
    xgboost_optimizer$parameter_grid <- param_list_xgboost
    xgboost_optimizer$split_type <- "stratified"
    xgboost_optimizer$optim_args <- optim_args

    xgboost_optimizer$learner_args <- list(
      objective = "binary:logistic",
      eval_metric = "logloss"
    )
    xgboost_optimizer$performance_metric_args <- list(positive = "1")
    xgboost_optimizer$performance_metric <- mlexperiments::metric("auc")

    # set data
    xgboost_optimizer$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- xgboost_optimizer$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 10))
    expect_true(inherits(
      x = xgboost_optimizer$results,
      what = "mlexCV"
    ))
  }
)


test_that(
  desc = "test nested cv, grid - xgboost",
  code = {

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
    random_grid <- sample(seq_len(nrow(param_list_xgboost)), 10)
    xgboost_optimizer$parameter_grid <-
      param_list_xgboost[random_grid, ]
    xgboost_optimizer$split_type <- "stratified"

    xgboost_optimizer$learner_args <- list(
      objective = "binary:logistic",
      eval_metric = "logloss"
    )
    xgboost_optimizer$performance_metric_args <- list(positive = "1")
    xgboost_optimizer$performance_metric <- mlexperiments::metric("auc")

    # set data
    xgboost_optimizer$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- xgboost_optimizer$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 10))
    expect_true(inherits(
      x = xgboost_optimizer$results,
      what = "mlexCV"
    ))
  }
)
