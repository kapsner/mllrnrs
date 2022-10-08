#' @export
LearnerSurvXgboostCox <- R6::R6Class( # nolint
  classname = "LearnerSurvXgboostCox",
  inherit = mllrnrs::LearnerXgboost,
  public = list(
    initialize = function() {
      super$initialize()
      self$cluster_export <- surv_xgboost_cox_ce()
      private$fun_optim_cv <- surv_xgboost_cox_optimization
    }
  )
)


surv_xgboost_cox_ce <- function() {
  c("surv_xgboost_cox_optimization", "setup_surv_xgb_dataset",
    xgboost_ce())
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
