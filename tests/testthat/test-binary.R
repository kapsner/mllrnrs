library(mlbench)
data("PimaIndiansDiabetes2")
dataset <- PimaIndiansDiabetes2 |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[1:8]

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
train_y <- as.integer(dataset[, get("diabetes")]) - 1L

options("mlexperiments.bayesian.max_init" = 10L)

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

test_that(
  desc = "test nested cv, grid, binary - glmnet",
  code = {

    skip_on_cran()

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

    glmnet_optimizer$learner_args <- list(
      family = "binomial",
      type.measure = "class",
      standardize = TRUE
    )
    glmnet_optimizer$predict_args <- list(type = "response")
    glmnet_optimizer$performance_metric_args <- list(
      positive = "1",
      negative = "0"
    )
    glmnet_optimizer$performance_metric <- mlexperiments::metric("AUC")

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
  }
)

test_that(
  desc = "test nested cv, grid - glmnet, errors",
  code = {

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

    glmnet_optimizer$learner_args <- list(
      type.measure = "class",
      standardize = TRUE
    )
    glmnet_optimizer$predict_args <- list(type = "response")
    glmnet_optimizer$performance_metric_args <- list(
      positive = "1",
      negative = "0"
    )
    glmnet_optimizer$performance_metric <- mlexperiments::metric("AUC")

    # set data
    glmnet_optimizer$set_data(
      x = train_x,
      y = train_y
    )

    expect_error(glmnet_optimizer$execute())
  }
)


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

options("mlexperiments.bayesian.max_init" = 10L)
options("mlexperiments.optim.lgb.nrounds" = 100L)
options("mlexperiments.optim.lgb.early_stopping_rounds" = 10L)

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
  desc = "test nested cv, bayesian, binary - lightgbm",
  code = {

    lightgbm_optimizer <- mlexperiments::MLNestedCV$new(
      learner = mllrnrs::LearnerLightgbm$new(
        metric_optimization_higher_better = FALSE
      ),
      strategy = "bayesian",
      fold_list = fold_list,
      k_tuning = 3L,
      ncores = ncores,
      seed = seed
    )

    lightgbm_optimizer$parameter_bounds <- lightgbm_bounds
    lightgbm_optimizer$parameter_grid <- param_list_lightgbm
    lightgbm_optimizer$split_type <- "stratified"
    lightgbm_optimizer$optim_args <- optim_args

    lightgbm_optimizer$learner_args <- list(
      objective = "binary",
      metric = "binary_logloss",
      cat_vars = c("pregnant", "pedigree")
    )
    lightgbm_optimizer$performance_metric_args <- list(
      positive = "1",
      negative = "0"
    )
    lightgbm_optimizer$performance_metric <- mlexperiments::metric("auc")

    # set data
    lightgbm_optimizer$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- lightgbm_optimizer$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 12))
    expect_true(inherits(
      x = lightgbm_optimizer$results,
      what = "mlexCV"
    ))
  }
)


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

test_that(
  desc = "test nested cv, grid, binary - ranger",
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

    ranger_optimizer$learner_args <- list(probability = TRUE,
                                          cat_vars = c("pregnant", "pedigree"))
    ranger_optimizer$predict_args <- list(prob = TRUE, positive = "1")

    ranger_optimizer$performance_metric_args <- list(
      positive = "1",
      negative = "0"
    )
    ranger_optimizer$performance_metric <- mlexperiments::metric("AUC")

    # set data
    ranger_optimizer$set_data(
      x = train_x,
      y = factor(train_y)
    )

    cv_results <- ranger_optimizer$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(3, 8))
    expect_true(inherits(
      x = ranger_optimizer$results,
      what = "mlexCV"
    ))
  }
)


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
options("mlexperiments.optim.xgb.nrounds" = 100L)
options("mlexperiments.optim.xgb.early_stopping_rounds" = 10L)

# ###########################################################################
# %% NESTED CV
# ###########################################################################

test_that(
  desc = "test nested cv, grid, binary:logistic - xgboost",
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
    random_grid <- sample(seq_len(nrow(param_list_xgboost)), 3)
    xgboost_optimizer$parameter_grid <-
      param_list_xgboost[random_grid, ]
    xgboost_optimizer$split_type <- "stratified"

    xgboost_optimizer$learner_args <- list(
      objective = "binary:logistic",
      eval_metric = "logloss"
    )
    xgboost_optimizer$performance_metric_args <- list(
      positive = "1",
      negative = "0"
    )
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

