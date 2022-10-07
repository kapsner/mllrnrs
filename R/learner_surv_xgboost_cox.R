#' @export
LearnerSurvXgboostCox <- R6::R6Class( # nolint
  classname = "LearnerSurvXgboostCox",
  inherit = mlexperiments::MLLearnerBase,
  public = list(
    initialize = function() {
      if (!requireNamespace("xgboost", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"xgboost\" must be installed to use ",
            "'learner = \"LearnerSurvXgboostCox\"'."
          ),
          call. = FALSE
        )
      }
      super$initialize()
      self$metric_optimization_higher_better <- FALSE
      self$metric_performance_higher_better <- TRUE
      self$environment <- "mllrnrs"
      self$cluster_export <- surv_xgboost_cox_ce()
      private$fun_optim_cv <- surv_xgboost_cox_optimization
      private$fun_fit <- xgboost_fit
      private$fun_predict <- xgboost_predict
      private$fun_bayesian_scoring_function <- xgboost_bsF
      private$fun_performance_metric <- surv_xgboost_c_index
      self$metric_performance_name <- "C-index"
    }
  )
)


surv_xgboost_cox_ce <- function() {
  c("surv_xgboost_cox_optimization", "xgboost_optimization",
    "setup_xgb_dataset", "setup_surv_xgb_dataset", "xgboost_fit")
}

# tune lambda
surv_xgboost_cox_optimization <- function(
    x,
    y,
    params,
    fold_list,
    ncores,
    seed
  ) {
  stopifnot(
    inherits(x = y, what = "Surv"),
    is.list(params),
    params$objective == "survival:cox"
  )

  return(xgboost_optimization(x, y, params, fold_list, ncores, seed))
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

surv_xgboost_cox_predict <- function(model, newdata, ncores, ...) {
  # From the docs:
  # Note that predictions are returned on the hazard ratio scale
  # (i.e., as HR = exp(marginal_prediction) in the proportional
  # hazard function h(t) = h0(t) * HR).
  return(xgboost_predict(model, newdata, ncores, ...))
}

surv_xgboost_c_index <- function(ground_truth, predictions) {
  return(glmnet::Cindex(pred = predictions, y = ground_truth))
}
