library(mlbench)
data("BostonHousing")
dataset <- BostonHousing |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[1:13]
cat_vars <- "chas"

param_list_ranger <- expand.grid(
  num.trees = seq(500, 1000, 500),
  mtry = seq(2, 6, 2),
  min.node.size = seq(1, 9, 4),
  max.depth = seq(1, 9, 4),
  sample.fraction = seq(0.5, 0.8, 0.3)
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

train_x <- data.matrix(
  dataset[, .SD, .SDcols = feature_cols]
)
train_y <- dataset[, get("medv")]

options("mlexperiments.bayesian.max_init" = 10L)

fold_list <- splitTools::create_folds(
  y = train_y,
  k = 3,
  type = "stratified",
  seed = seed
)

# ###########################################################################
# %% TUNING
# ###########################################################################

ranger_bounds <- list(
  num.trees = c(100L, 1000L),
  mtry = c(2L, 9L),
  min.node.size = c(1L, 20L),
  max.depth = c(1L, 40L),
  sample.fraction = c(0.3, 1.)
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
  desc = "test nested cv, bayesian, regression - ranger",
  code = {

    ranger_optimizer <- mlexperiments::MLNestedCV$new(
      learner = mllrnrs::LearnerRanger$new(),
      strategy = "bayesian",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    ranger_optimizer$parameter_bounds <- ranger_bounds
    ranger_optimizer$parameter_grid <- param_list_ranger
    ranger_optimizer$split_type <- "stratified"
    ranger_optimizer$optim_args <- optim_args

    ranger_optimizer$performance_metric <- mlexperiments::metric("msle")

    # set data
    ranger_optimizer$set_data(
      x = train_x,
      y = train_y,
      cat_vars = cat_vars
    )

    cv_results <- ranger_optimizer$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 7))
    expect_true(inherits(
      x = ranger_optimizer$results,
      what = "mlexCV"
    ))
  }
)


test_that(
  desc = "test nested cv, grid - ranger",
  code = {

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

    ranger_optimizer$performance_metric <- mlexperiments::metric("msle")

    # set data
    ranger_optimizer$set_data(
      x = train_x,
      y = train_y,
      cat_vars = cat_vars
    )

    cv_results <- ranger_optimizer$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 7))
    expect_true(inherits(
      x = ranger_optimizer$results,
      what = "mlexCV"
    ))
  }
)
