#' @export
LearnerLightgbm <- R6::R6Class( # nolint
  classname = "LearnerLightgbm",
  inherit = mlexperiments::MLLearnerBase,
  public = list(
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
    training_data = nrow(x)
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
      object = model,
      data = newdata # data in 3.3.2 and newdata in 3.3.2.99
    ),
    kwargs
  )
  preds <- do.call(stats::predict, args)
  if (!is.null(kwargs$reshape)) {
    if (isTRUE(kwargs$reshape)) {
      preds <- data.table::as.data.table(preds)[
        , which.max(.SD) - 1L, by = seq_len(nrow(preds))
      ][, get("V1")]
    }
  }
  return(preds)
}
