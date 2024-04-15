#' @title R6 Class to construct a Xgboost learner
#'
#' @description
#' The `LearnerXgboost` class is the interface to the `xgboost` R package for
#'   use with the `mlexperiments` package.
#'
#' @details
#' Optimization metric: needs to be specified with the learner parameter
#'   `eval_metric`. The following options can be set via `options()`:
#'   * "mlexperiments.optim.xgb.nrounds" (default: `5000L`)
#'   * "mlexperiments.optim.xgb.early_stopping_rounds" (default: `500L`)
#'   * "mlexperiments.xgb.print_every_n" (default: `50L`)
#'   * "mlexperiments.xgb.verbose" (default: `FALSE`)
#'
#' `LearnerXgboost` can be used with
#' * [mlexperiments::MLTuneParameters]
#' * [mlexperiments::MLCrossValidation]
#' * [mlexperiments::MLNestedCV]
#'
#' @seealso [xgboost::xgb.train()], [xgboost::xgb.cv()]
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
#' param_list_xgboost <- expand.grid(
#'    subsample = seq(0.6, 1, .2),
#'    colsample_bytree = seq(0.6, 1, .2),
#'    min_child_weight = seq(1, 5, 4),
#'    learning_rate = seq(0.1, 0.2, 0.1),
#'    max_depth = seq(1, 5, 4)
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
#' xgboost_cv <- mlexperiments::MLCrossValidation$new(
#'   learner = mllrnrs::LearnerXgboost$new(
#'     metric_optimization_higher_better = FALSE
#'   ),
#'   fold_list = fold_list,
#'   ncores = 2,
#'   seed = 123
#' )
#' xgboost_cv$learner_args <- c(
#'   as.list(
#'     data.table::data.table(
#'       param_list_xgboost[37, ],
#'       stringsAsFactors = FALSE
#'     ),
#'   ),
#'   list(
#'     objective = "binary:logistic",
#'     eval_metric = "logloss"
#'   ),
#'   nrounds = 45L
#' )
#' xgboost_cv$performance_metric_args <- list(positive = "1")
#' xgboost_cv$performance_metric <- mlexperiments::metric("auc")
#'
#' # set data
#' xgboost_cv$set_data(
#'   x = train_x,
#'   y = train_y
#' )
#'
#' xgboost_cv$execute()
#'
#' @export
#'
LearnerXgboost <- R6::R6Class( # nolint
  classname = "LearnerXgboost",
  inherit = mlexperiments::MLLearnerBase,
  public = list(

    #' @description
    #' Create a new `LearnerXgboost` object.
    #'
    #' @param metric_optimization_higher_better A logical. Defines the direction
    #'  of the optimization metric used throughout the hyperparameter
    #'  optimization.
    #'
    #' @return A new `LearnerXgboost` R6 object.
    #'
    #' @examples
    #' LearnerXgboost$new(metric_optimization_higher_better = FALSE)
    #'
    initialize = function(metric_optimization_higher_better) { # nolint
      if (!requireNamespace("xgboost", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"xgboost\" must be installed to use ",
            "'learner = \"LearnerXgboost\"'."
          ),
          call. = FALSE
        )
      }
      super$initialize(metric_optimization_higher_better =
                         metric_optimization_higher_better)
      self$environment <- "mllrnrs"
      self$cluster_export <- xgboost_ce()
      private$fun_optim_cv <- xgboost_optimization
      private$fun_fit <- xgboost_fit
      private$fun_predict <- xgboost_predict
      private$fun_bayesian_scoring_function <- xgboost_bsF
    }
  )
)


xgboost_ce <- function() {
  c("xgboost_optimization", "xgboost_fit",
    "setup_xgb_dataset", "xgboost_dataset_wrapper")
}

xgboost_bsF <- function(...) { # nolint

  params <- list(...)

  set.seed(seed)#, kind = "L'Ecuyer-CMRG")
  bayes_opt_xgboost <- xgboost_optimization(
    x = x,
    y = y,
    params = params,
    fold_list = method_helper$fold_list,
    ncores = 1L, # important, as bayesian search is already parallelized
    seed = seed
  )

  ret <- kdry::list.append(
    list("Score" = bayes_opt_xgboost$metric_optim_mean),
    bayes_opt_xgboost
  )

  return(ret)
}

# tune lambda
xgboost_optimization <- function(
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

  temp_list <- xgboost_dataset_wrapper(
    x = x,
    y = y,
    params = params
  )
  params <- temp_list$params
  dtrain <- temp_list$dtrain

  # use the same folds for all algorithms
  # folds: list provides a possibility to use a list of pre-defined CV
  # folds (each element must be a vector of test fold's indices).
  # When folds are supplied, the nfold and stratified parameters
  # are ignored.
  xgb_fids <- kdry::mlh_outsample_row_indices(
    fold_list = fold_list,
    dataset_nrows = nrow(x)
  )

  fit_args <- list(
    params = params,
    data = dtrain,
    nrounds = as.integer(options("mlexperiments.optim.xgb.nrounds")),
    folds = xgb_fids,
    print_every_n = as.integer(options("mlexperiments.xgb.print_every_n")),
    early_stopping_rounds = as.integer(
      options("mlexperiments.optim.xgb.early_stopping_rounds")
    ),
    verbose = as.logical(options("mlexperiments.xgb.verbose")),
    nthread = ncores
  )

  set.seed(seed)
  # train the model for this cv-fold
  cvfit <- do.call(xgboost::xgb.cv, fit_args)

  # save the results / use xgboost's metric here for selecting the best model
  # (cox-nloglik)
  metric_col <- grep(
    pattern = "^test(.*)mean$",
    x = colnames(cvfit$evaluation_log),
    value = TRUE
  )
  stopifnot(length(metric_col) == 1)

  res <- list(
    "metric_optim_mean" = cvfit$evaluation_log[
      get("iter") == cvfit$best_iteration,
      get(metric_col)
    ],
    "nrounds" = cvfit$best_iteration
  )

  return(res)
}

xgboost_dataset_wrapper <- function(x, y, params) {
  # create dataset
  dataset_args <- list(
    x = x,
    y = y,
    objective = params$objective
  )
  if ("case_weights" %in% names(params)) {
    stopifnot(
      "late fail: `case_weights` must be of same length as `y`" =
        length(params$case_weights) == length(y)
    )
    dataset_args <- c(
      dataset_args,
      list(case_weights = params$case_weights)
    )
    # remove case_weights-param from learner-args
    params$case_weights <- NULL
  }
  dtrain <- do.call(setup_xgb_dataset, dataset_args)

  # return dataset and modified params
  return(list(
    dtrain = dtrain,
    params = params
  ))
}

xgboost_fit <- function(x, y, nrounds, ncores, seed, ...) {
  params <- list(...)
  stopifnot("objective" %in% names(params))

  # create dataset
  temp_list <- xgboost_dataset_wrapper(
    x = x,
    y = y,
    params = params
  )
  params <- temp_list$params
  dtrain_full <- temp_list$dtrain

  # train final model with best nrounds
  fit_args <- list(
    data = dtrain_full,
    params = params,
    print_every_n = as.integer(options("mlexperiments.xgb.print_every_n")),
    nthread = ncores,
    nrounds = nrounds,
    watchlist = list(
      train = dtrain_full  # setup a watchlist (the training data here)
    ),
    verbose = as.logical(options("mlexperiments.xgb.verbose"))
  )

  set.seed(seed)
  # fit the model
  bst <- do.call(xgboost::xgb.train, fit_args)
  return(bst)
}

# wrapper function for creating the input data for xgboost
setup_xgb_dataset <- function(x, y, objective, ...) {
  kwargs <- list(...)
  if (objective %in% c("survival:aft", "survival:cox")) {
    return(setup_surv_xgb_dataset(x, y, objective))
  } else {
    stopifnot(is.atomic(y))
    # create a xgb.DMatrix
    dtrain <- xgboost::xgb.DMatrix(x)
    label <- y
    xgboost::setinfo(dtrain, "label", label)
    if ("case_weights" %in% names(kwargs)) {
      xgboost::setinfo(dtrain, "weight", kwargs$case_weights)
    }
    return(dtrain)
  }
}


# wrapper function for creating the input data for xgboost
setup_surv_xgb_dataset <- function(x, y, objective) {

  # create a xgb.DMatrix
  dtrain <- xgboost::xgb.DMatrix(x)

  # for aft-models, the label must be formatted as follows:
  if (objective == "survival:aft") {
    y_lower_bound <- y[, 1]
    y_upper_bound <- ifelse(
      y[, 2] == 1,
      y[, 1],
      Inf
    )
    xgboost::setinfo(dtrain, "label_lower_bound", y_lower_bound)
    xgboost::setinfo(dtrain, "label_upper_bound", y_upper_bound)
  } else if (objective == "survival:cox") {
    # Cox regression for right censored survival time data (negative values
    # are considered right censored). Note that predictions are returned on
    # the hazard ratio scale (i.e., as HR = exp(marginal_prediction) in
    # the proportional hazard function h(t) = h0(t) * HR).
    label <- ifelse(y[, 2] == 1, y[, 1], -y[, 1])
    xgboost::setinfo(dtrain, "label", label)
  }

  return(dtrain)
}


xgboost_predict <- function(model, newdata, ncores, ...) {
  kwargs <- list(...)
  args <- kdry::list.append(
    list(
      object = model,
      newdata = newdata
    ),
    kwargs
  )
  preds <- do.call(stats::predict, args)
  if (!is.null(kwargs$reshape)) {
    if (isTRUE(kwargs$reshape)) {
      preds <- kdry::mlh_reshape(preds)
    }
  }
  return(preds)
}
