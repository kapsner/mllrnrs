#' @title R6 Class to construct a LightGBM learner
#'
#' @description
#' The `LearnerLightgbm` class is the interface to the `lightgbm` R package for
#'   use with the `mlexperiments` package.
#'
#' @details
#' Optimization metric: needs to be specified with the learner parameter
#'   `metric`. The following options can be set via `options()`:
#'   * "mlexperiments.optim.lgb.nrounds" (default: `5000L`)
#'   * "mlexperiments.optim.lgb.early_stopping_rounds" (default: `500L`)
#'   * "mlexperiments.lgb.print_every_n" (default: `50L`)
#'   * "mlexperiments.lgb.verbose" (default: `-1L`)
#'
#' `LearnerLightgbm` can be used with
#' * [mlexperiments::MLTuneParameters]
#' * [mlexperiments::MLCrossValidation]
#' * [mlexperiments::MLNestedCV]
#'
#' @seealso [lightgbm::lgb.train()], [lightgbm::lgb.cv()]
#'
#' @examples
#' # binary classification
#'
#' library(mlbench)
#' data("PimaIndiansDiabetes2")
#' dataset <- PimaIndiansDiabetes2 |>
#'   data.table::as.data.table() |>
#'   na.omit()
#'
#' seed <- 123
#' feature_cols <- colnames(dataset)[1:8]
#'
#' param_list_lightgbm <- expand.grid(
#'   bagging_fraction = seq(0.6, 1, .2),
#'   feature_fraction = seq(0.6, 1, .2),
#'   min_data_in_leaf = seq(10, 50, 10),
#'   learning_rate = seq(0.1, 0.2, 0.1),
#'   num_leaves = seq(10, 50, 10),
#'   max_depth = -1L
#' )
#'
#' train_x <- model.matrix(
#'   ~ -1 + .,
#'   dataset[, .SD, .SDcols = feature_cols]
#' )
#' train_y <- as.integer(dataset[, get("diabetes")]) - 1L
#'
#' fold_list <- splitTools::create_folds(
#'   y = train_y,
#'   k = 3,
#'   type = "stratified",
#'   seed = seed
#' )
#' lightgbm_cv <- mlexperiments::MLCrossValidation$new(
#'   learner = mllrnrs::LearnerLightgbm$new(
#'     metric_optimization_higher_better = FALSE
#'   ),
#'   fold_list = fold_list,
#'   ncores = 2,
#'   seed = 123
#' )
#' lightgbm_cv$learner_args <- c(
#'   as.list(
#'     data.table::data.table(
#'       param_list_lightgbm[37, ],
#'       stringsAsFactors = FALSE
#'     ),
#'   ),
#'   list(
#'     objective = "binary",
#'     metric = "binary_logloss"
#'   ),
#'   nrounds = 45L
#' )
#' lightgbm_cv$performance_metric_args <- list(positive = "1")
#' lightgbm_cv$performance_metric <- mlexperiments::metric("auc")
#'
#' # set data
#' lightgbm_cv$set_data(
#'   x = train_x,
#'   y = train_y
#' )
#'
#' lightgbm_cv$execute()
#'
#' @export
#'
LearnerLightgbm <- R6::R6Class( # nolint
  classname = "LearnerLightgbm",
  inherit = mlexperiments::MLLearnerBase,
  public = list(

    #' @description
    #' Create a new `LearnerLightgbm` object.
    #'
    #' @param metric_optimization_higher_better A logical. Defines the direction
    #'  of the optimization metric used throughout the hyperparameter
    #'  optimization.
    #'
    #' @return A new `LearnerLightgbm` R6 object.
    #'
    #' @examples
    #' LearnerLightgbm$new(metric_optimization_higher_better = FALSE)
    #'
    initialize = function(metric_optimization_higher_better) { # nolint
      if (!requireNamespace("lightgbm", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"lightgbm\" must be installed to use ",
            "'learner = \"LearnerLightgbm\"'."
          ),
          call. = FALSE
        )
      }
      super$initialize(metric_optimization_higher_better =
                         metric_optimization_higher_better)
      self$environment <- "mllrnrs"
      self$cluster_export <- lightgbm_ce()
      private$fun_optim_cv <- lightgbm_optimization
      private$fun_fit <- lightgbm_fit
      private$fun_predict <- lightgbm_predict
      private$fun_bayesian_scoring_function <- lightgbm_bsF
    }
  )
)


lightgbm_ce <- function() {
  c("lightgbm_optimization", "lightgbm_fit",
    "setup_lgb_dataset")
}

lightgbm_bsF <- function(...) { # nolint

  params <- list(...)

  set.seed(seed)#, kind = "L'Ecuyer-CMRG")
  bayes_opt_lightgbm <- lightgbm_optimization(
    x = x,
    y = y,
    params = params,
    fold_list = method_helper$fold_list,
    ncores = 1L, # important, as bayesian search is already parallelized
    seed = seed
  )

  ret <- kdry::list.append(
    list("Score" = bayes_opt_lightgbm$metric_optim_mean),
    bayes_opt_lightgbm
  )

  return(ret)
}

# tune lambda
lightgbm_optimization <- function(
    x,
    y,
    params,
    fold_list,
    ncores,
    seed
  ) {
  stopifnot(
    is.list(params),
    "objective" %in% names(params)
  )

  if ("cat_vars" %in% names(params)) {
    cat_vars <- params[["cat_vars"]]
    params[["cat_vars"]] <- NULL
  } else {
    cat_vars <- NULL
  }

  # train final model with best nrounds
  dtrain <- setup_lgb_dataset(
    x = x,
    y = y,
    objective = params$objective,
    cat_vars = cat_vars
  )

  # use the same folds for all algorithms
  # folds: list provides a possibility to use a list of pre-defined CV
  # folds (each element must be a vector of test fold's indices).
  # When folds are supplied, the nfold and stratified parameters
  # are ignored.
  lgb_fids <- kdry::mlh_outsample_row_indices(
    fold_list = fold_list,
    dataset_nrows = nrow(x)
  )

  params$num_threads <- ncores

  fit_args <- list(
    params = params,
    data = dtrain,
    nrounds = as.integer(options("mlexperiments.optim.lgb.nrounds")),
    folds = lgb_fids,
    eval_freq = as.integer(options("mlexperiments.lgb.print_every_n")),
    showsd = FALSE, # speedup computation
    early_stopping_rounds = as.integer(
      options("mlexperiments.optim.lgb.early_stopping_rounds")
    ),
    verbose = as.integer(options("mlexperiments.lgb.verbose"))
  )

  set.seed(seed)
  # train the model for this cv-fold
  cvfit <- do.call(lightgbm::lgb.cv, fit_args)

  # save the results / use lightgbm's metric here for selecting the best model
  res <- list(
    "metric_optim_mean" = cvfit$best_score,
    "nrounds" = cvfit$best_iter
  )

  return(res)
}

lightgbm_fit <- function(x, y, nrounds, ncores, seed, ...) {
  params <- list(...)
  stopifnot("objective" %in% names(params))

  if ("cat_vars" %in% names(params)) {
    cat_vars <- params[["cat_vars"]]
    params[["cat_vars"]] <- NULL
  } else {
    cat_vars <- NULL
  }

  # train final model with best nrounds
  dtrain_full <- setup_lgb_dataset(
    x = x,
    y = y,
    objective = params$objective,
    cat_vars = cat_vars
  )

  params$num_threads <- ncores

  fit_args <- list(
    data = dtrain_full,
    params = params,
    eval_freq = as.integer(options("mlexperiments.lgb.print_every_n")),
    nrounds = nrounds,
    valids = list(
      train = dtrain_full  # setup a watchlist (the training data here)
    ),
    verbose = as.integer(options("mlexperiments.lgb.verbose"))
  )

  set.seed(seed)
  # fit the model
  bst <- do.call(lightgbm::lgb.train, fit_args)
  return(bst)
}

# wrapper function for creating the input data for lightgbm
setup_lgb_dataset <- function(x, y, objective, cat_vars) {
  stopifnot(is.atomic(y))
  # create a lgb.DMatrix
  dtrain <- lightgbm::lgb.Dataset(
    data = x,
    label = y
  )

  if (!is.null(cat_vars)) {
    stopifnot(length(intersect(cat_vars, names(x))) == length(cat_vars))
    lightgbm::lgb.Dataset.set.categorical(
      dataset = dtrain,
      categorical_feature = which(cat_vars %in% colnames(x))
    )
  }
  return(dtrain)
}

lightgbm_predict <- function(model, newdata, ncores, ...) {
  kwargs <- list(...)
  args <- kdry::list.append(
    list(
      object = model
      # , data = newdata # data in 3.3.2 and newdata in 3.3.2.99
    ),
    kwargs
  )

  # temporary fix due to API change of lightgbm's predict-method:
  # - https://lightgbm.readthedocs.io/en/v3.3.5/R/reference/predict.lgb.Booster.html
  # - https://lightgbm.readthedocs.io/en/v4.0.0/R/reference/predict.lgb.Booster.html
  # this can be removed, once new version is out on cran, but then also
  # add lightgbm>=4.0.0 as a requirement to the package description
  if (utils::packageVersion("lightgbm") <= package_version("3.3.5")) {
    args$data <- newdata
  } else {
    args$newdata <- newdata
    # remove also reshape argument (https://github.com/microsoft/LightGBM/pull/4971)
    args$reshape <- NULL
  }

  preds <- do.call(stats::predict, args)
  if (!is.null(kwargs$reshape)) {
    if (isTRUE(kwargs$reshape)) {
      preds <- kdry::mlh_reshape(preds)
    }
  }
  return(preds)
}
