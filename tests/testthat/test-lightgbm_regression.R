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
  min_data_in_leaf = seq(2, 10, 2),
  learning_rate = seq(0.1, 0.2, 0.1),
  num_leaves = seq(2, 20, 4),
  max_depth = -1L,
  verbose = -1L
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
# %% TUNING
# ###########################################################################

lightgbm_bounds <- list(
  bagging_fraction = c(0.2, 1),
  feature_fraction = c(0.2, 1),
  min_data_in_leaf = c(2L, 12L),
  learning_rate = c(0.1, 0.2),
  num_leaves =  c(2L, 20L)
)
optim_args <- list(
  iters.n = ncores,
  kappa = 3.5,
  acq = "ucb"
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
    lightgbm_optimization$performance_metric <- mlexperiments::metric("msle")
    # set data
    lightgbm_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- lightgbm_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 12))
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
    random_grid <- sample(seq_len(nrow(param_list_lightgbm)), 3)
    lightgbm_optimization$parameter_grid <-
      param_list_lightgbm[random_grid, ]
    lightgbm_optimization$split_type <- "stratified"

    lightgbm_optimization$learner_args <- list(
      objective = "regression",
      metric = "l2"
    )
    lightgbm_optimization$performance_metric <- mlexperiments::metric("msle")

    # set data
    lightgbm_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- lightgbm_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 12))
    expect_true(inherits(
      x = lightgbm_optimization$results,
      what = "mlexCV"
    ))
  }
)
