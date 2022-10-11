library(mlbench)
data("BostonHousing")
dataset <- BostonHousing |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[1:13]

param_list_lightgbm <- expand.grid(
  bagging_fraction = seq(0.6, 1, .2),
  feature_fraction = seq(0.6, 1, .2),
  min_data_in_leaf = seq(10, 50, 10),
  learning_rate = seq(0.1, 0.2, 0.1),
  num_leaves = seq(10, 50, 10),
  max_depth = -1L
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
train_y <- dataset[, get("medv")]

options("mlexperiments.bayesian.max_init" = 10L)
options("mlexperiments.optim.lgb.nrounds" = 100L)
options("mlexperiments.optim.lgb.early_stopping_rounds" = 10L)

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
  desc = "test cv, regression - lightgbm",
  code = {

    lightgbm_optimization <- mlexperiments::MLCrossValidation$new(
      learner = mllrnrs::LearnerLightgbm$new(
        metric_optimization_higher_better = FALSE
      ),
      fold_list = fold_list,
      ncores = ncores,
      seed = seed
    )
    lightgbm_optimization$learner_args <- c(
      as.list(
        data.table::data.table(
          param_list_lightgbm[37, ],
          stringsAsFactors = FALSE
        ),
      ),
      list(
        objective = "regression",
        metric = "l2"
      ),
      nrounds = 45L
    )
    lightgbm_optimization$performance_metric_args <- list(positive = "1")
    lightgbm_optimization$performance_metric <- mlexperiments::metric("msle")
    lightgbm_optimization$performance_metric_name <- "Mean squared error loss"

    # set data
    lightgbm_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- lightgbm_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 11))
    expect_true(inherits(
      x = lightgbm_optimization$results,
      what = "mlexCV"
    ))
  }
)


# ###########################################################################
# %% NESTED CV
# ###########################################################################

lightgbm_bounds <- list(
  bagging_fraction = c(0.2, 1),
  feature_fraction = c(0.2, 1),
  min_data_in_leaf = c(10L, 50L),
  learning_rate = c(0.1, 0.2),
  num_leaves =  c(10L, 50L)
)
optim_args <- list(
  iters.n = ncores,
  kappa = 3.5,
  acq = "ucb"
)

test_that(
  desc = "test bayesian tuner, parameter_grid, regression - lightgbm",
  code = {

    lightgbm_tuner <- mlexperiments::MLTuneParameters$new(
      learner = mllrnrs::LearnerLightgbm$new(
        metric_optimization_higher_better = FALSE
      ),
      strategy = "bayesian",
      ncores = ncores,
      seed = seed
    )
    lightgbm_tuner$parameter_grid <- param_list_lightgbm
    lightgbm_tuner$parameter_bounds <- lightgbm_bounds
    lightgbm_tuner$learner_args <- list(
      objective = "regression",
      metric = "l2",
      verbose = FALSE
    )
    lightgbm_tuner$optim_args <- optim_args

    # set data
    lightgbm_tuner$set_data(
      x = train_x,
      y = train_y
    )

    tune_results <- lightgbm_tuner$execute(k = 3)

    expect_type(tune_results, "list")
    expect_true(inherits(x = lightgbm_tuner$results, what = "mlexTune"))
  }
)


test_that(
  desc = "test grid tuner, regression - lightgbm",
  code = {

    lightgbm_tuner <- mlexperiments::MLTuneParameters$new(
      learner = mllrnrs::LearnerLightgbm$new(
        metric_optimization_higher_better = FALSE
      ),
      strategy = "grid",
      ncores = ncores,
      seed = seed
    )
    set.seed(seed)
    random_grid <- sample(seq_len(nrow(param_list_lightgbm)), 10)
    lightgbm_tuner$parameter_grid <- param_list_lightgbm[random_grid, ]
    lightgbm_tuner$learner_args <- list(
      objective = "regression",
      metric = "l2",
      verbose = FALSE
    )

    # set data
    lightgbm_tuner$set_data(
      x = train_x,
      y = train_y
    )

    tune_results <- lightgbm_tuner$execute(k = 3)
    expect_type(tune_results, "list")
    expect_equal(dim(tune_results), c(10, 12))
    expect_true(inherits(x = lightgbm_tuner$results, what = "mlexTune"))
  }
)

# ###########################################################################
# %% NESTED CV
# ###########################################################################

test_that(
  desc = "test nested cv, bayesian, regression - lightgbm",
  code = {

    lightgbm_optimization <- mlexperiments::MLNestedCV$new(
      learner = mllrnrs::LearnerLightgbm$new(
        metric_optimization_higher_better = FALSE
      ),
      strategy = "bayesian",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    lightgbm_optimization$parameter_bounds <- lightgbm_bounds
    set.seed(312)
    select_rows <- sample(seq_len(nrow(param_list_lightgbm)), 10L)
    lightgbm_optimization$parameter_grid <- param_list_lightgbm[select_rows, ]
    lightgbm_optimization$split_type <- "stratified"
    lightgbm_optimization$optim_args <- optim_args

    lightgbm_optimization$learner_args <- list(
      objective = "regression",
      metric = "l2"
    )
    lightgbm_optimization$performance_metric_args <- list(positive = "1")
    lightgbm_optimization$performance_metric <- mlexperiments::metric("msle")
    lightgbm_optimization$performance_metric_name <- "Mean squared error loss"

    # set data
    lightgbm_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- lightgbm_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 11))
    expect_true(inherits(
      x = lightgbm_optimization$results,
      what = "mlexCV"
    ))
  }
)


test_that(
  desc = "test nested cv, grid - lightgbm",
  code = {

    lightgbm_optimization <- mlexperiments::MLNestedCV$new(
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
    random_grid <- sample(seq_len(nrow(param_list_lightgbm)), 10)
    lightgbm_optimization$parameter_grid <-
      param_list_lightgbm[random_grid, ]
    lightgbm_optimization$split_type <- "stratified"

    lightgbm_optimization$learner_args <- list(
      objective = "regression",
      metric = "l2"
    )
    lightgbm_optimization$performance_metric_args <- list(positive = "1")
    lightgbm_optimization$performance_metric <- mlexperiments::metric("msle")
    lightgbm_optimization$performance_metric_name <- "Mean squared error loss"

    # set data
    lightgbm_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- lightgbm_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 11))
    expect_true(inherits(
      x = lightgbm_optimization$results,
      what = "mlexCV"
    ))
  }
)
