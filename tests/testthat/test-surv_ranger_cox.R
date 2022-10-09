dataset <- survival::colon |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
surv_cols <- c("status", "time", "rx")

feature_cols <- colnames(dataset)[3:ncol(dataset)]

param_list_ranger <- expand.grid(
  sample.fraction = seq(0.6, 1, .2),
  min.node.size = seq(1, 5, 4),
  mtry = seq(2, 6, 2),
  num.trees = c(5L, 10L),
  max.depth = seq(1, 5, 4)
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

split_vector <- splitTools::multi_strata(
  df = dataset[, .SD, .SDcols = surv_cols],
  strategy = "kmeans",
  k = 4
)

train_x <- model.matrix(
  ~ -1 + .,
  dataset[, .SD, .SDcols = setdiff(colnames(dataset), surv_cols[1:2])]
)
train_y <- survival::Surv(
  event = (dataset[, get("status")] |>
             as.character() |>
             as.integer()),
  time = dataset[, get("time")],
  type = "right"
)

options("mlexperiments.bayesian.max_init" = 10L)

fold_list <- splitTools::create_folds(
  y = split_vector,
  k = 3,
  type = "stratified",
  seed = seed
)


# ###########################################################################
# %% CV
# ###########################################################################

test_that(
  desc = "test cv - surv_ranger_cox",
  code = {

    surv_ranger_cox_optimization <- mlexperiments::MLCrossValidation$new(
      learner = mllrnrs::LearnerSurvRangerCox$new(),
      fold_list = fold_list,
      ncores = ncores,
      seed = seed
    )
    surv_ranger_cox_optimization$learner_args <- as.list(
      data.table::data.table(param_list_ranger[1, ], stringsAsFactors = FALSE)
    )
    surv_ranger_cox_optimization$performance_metric <- c_index
    surv_ranger_cox_optimization$performance_metric_name <- "C-index"


    # set data
    surv_ranger_cox_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- surv_ranger_cox_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 6))
    expect_true(inherits(
      x = surv_ranger_cox_optimization$results,
      what = "mlexCV"
    ))
  }
)

# ###########################################################################
# %% TUNING
# ###########################################################################

ranger_bounds <- list(
  sample.fraction = c(0.2, 1),
  min.node.size = c(1L, 10L),
  mtry = c(2L, 10L),
  num.trees = c(1L, 10L),
  max.depth =  c(1L, 10L)
)
optim_args <- list(
  iters.n = ncores,
  kappa = 3.5,
  acq = "ucb"
)

test_that(
  desc = "test bayesian tuner, initGrid - surv_ranger_cox",
  code = {

    surv_ranger_cox_tuner <- mlexperiments::MLTuneParameters$new(
      learner = mllrnrs::LearnerSurvRangerCox$new(),
      strategy = "bayesian",
      ncores = ncores,
      seed = seed
    )
    surv_ranger_cox_tuner$parameter_bounds <- ranger_bounds
    surv_ranger_cox_tuner$parameter_grid <- param_list_ranger
    surv_ranger_cox_tuner$optim_args <- optim_args

    # create split-strata from training dataset
    surv_ranger_cox_tuner$split_vector <- split_vector

    # set data
    surv_ranger_cox_tuner$set_data(
      x = train_x,
      y = train_y
    )

    tune_results <- surv_ranger_cox_tuner$execute(k = 3)
    expect_type(tune_results, "list")
    expect_equal(dim(tune_results), c(ncores + 10, 14))
    expect_true(inherits(x = surv_ranger_cox_tuner$results, what = "mlexTune"))
  }
)


test_that(
  desc = "test grid tuner - surv_ranger_cox",
  code = {

    surv_ranger_cox_tuner <- mlexperiments::MLTuneParameters$new(
      learner = mllrnrs::LearnerSurvRangerCox$new(),
      strategy = "grid",
      ncores = ncores,
      seed = seed
    )
    set.seed(seed)
    random_grid <- sample(seq_len(nrow(param_list_ranger)), 10)
    surv_ranger_cox_tuner$parameter_grid <- param_list_ranger[random_grid, ]

    # create split-strata from training dataset
    surv_ranger_cox_tuner$split_vector <- split_vector

    # set data
    surv_ranger_cox_tuner$set_data(
      x = train_x,
      y = train_y
    )

    tune_results <- surv_ranger_cox_tuner$execute(k = 3)
    expect_type(tune_results, "list")
    expect_equal(dim(tune_results), c(10, 7))
    expect_true(inherits(x = surv_ranger_cox_tuner$results, what = "mlexTune"))
  }
)

# ###########################################################################
# %% NESTED CV
# ###########################################################################

test_that(
  desc = "test nested cv, bayesian - surv_ranger_cox",
  code = {

    surv_ranger_cox_optimization <- mlexperiments::MLNestedCV$new(
      learner = mllrnrs::LearnerSurvRangerCox$new(),
      strategy = "bayesian",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    surv_ranger_cox_optimization$parameter_bounds <- ranger_bounds
    surv_ranger_cox_optimization$parameter_grid <- param_list_ranger
    surv_ranger_cox_optimization$split_type <- "stratified"
    surv_ranger_cox_optimization$split_vector <- split_vector
    surv_ranger_cox_optimization$optim_args <- optim_args

    surv_ranger_cox_optimization$performance_metric <- c_index
    surv_ranger_cox_optimization$performance_metric_name <- "C-index"

    # set data
    surv_ranger_cox_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- surv_ranger_cox_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 6))
    expect_true(inherits(
      x = surv_ranger_cox_optimization$results,
      what = "mlexCV"
    ))
  }
)


test_that(
  desc = "test nested cv, grid - surv_ranger_cox",
  code = {

    surv_ranger_cox_optimization <- mlexperiments::MLNestedCV$new(
      learner = mllrnrs::LearnerSurvRangerCox$new(),
      strategy = "grid",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )
    set.seed(seed)
    random_grid <- sample(seq_len(nrow(param_list_ranger)), 10)
    surv_ranger_cox_optimization$parameter_grid <-
      param_list_ranger[random_grid, ]
    surv_ranger_cox_optimization$split_type <- "stratified"
    surv_ranger_cox_optimization$split_vector <- split_vector
    surv_ranger_cox_optimization$optim_args <- optim_args

    surv_ranger_cox_optimization$performance_metric <- c_index
    surv_ranger_cox_optimization$performance_metric_name <- "C-index"

    # set data
    surv_ranger_cox_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- surv_ranger_cox_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 6))
    expect_true(inherits(
      x = surv_ranger_cox_optimization$results,
      what = "mlexCV"
    ))
  }
)
