#' @title R6 Class to construct a Ranger learner
#'
#' @description
#' The `LearnerRanger` class is the interface to the `ranger` R package for use
#'   with the `mlexperiments` package.
#'
#' @details
#' Optimization metric:
#' * classification: classification error rate
#' * regression: mean squared error
#' Can be used with
#' * [mlexperiments::MLTuneParameters]
#' * [mlexperiments::MLCrossValidation]
#' * [mlexperiments::MLNestedCV]
#'
#' @seealso [ranger::ranger()]
#'
#' @importFrom mlexperiments metric
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
#' param_list_ranger <- expand.grid(
#'   num.trees = seq(500, 1000, 500),
#'   mtry = seq(2, 6, 2),
#'   min.node.size = seq(1, 9, 4),
#'   max.depth = seq(1, 9, 4),
#'   sample.fraction = seq(0.5, 0.8, 0.3)
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
#' ranger_cv <- mlexperiments::MLCrossValidation$new(
#'   learner = mllrnrs::LearnerRanger$new(),
#'   fold_list = fold_list,
#'   ncores = 2,
#'   seed = 123
#' )
#' ranger_cv$learner_args <- c(
#'   as.list(
#'     data.table::data.table(
#'       param_list_ranger[37, ],
#'       stringsAsFactors = FALSE
#'     ),
#'   ),
#'   list(classification = TRUE)
#' )
#' ranger_cv$performance_metric_args <- list(positive = "1", negative = "0")
#' ranger_cv$performance_metric <- mlexperiments::metric("AUC")
#'
#' # set data
#' ranger_cv$set_data(
#'   x = train_x,
#'   y = train_y
#' )
#'
#' ranger_cv$execute()
#'
#' @export
LearnerRanger <- R6::R6Class( # nolint
  classname = "LearnerRanger",
  inherit = mlexperiments::MLLearnerBase,
  public = list(

    #' @description
    #' Create a new `LearnerRanger` object.
    #'
    #' @return A new `LearnerRanger` R6 object.
    #'
    #' @examples
    #' LearnerRanger$new()
    #'
    initialize = function() {
      if (!requireNamespace("ranger", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"ranger\" must be installed to use ",
            "'learner = \"LearnerRanger\"'."
          ),
          call. = FALSE
        )
      }
      super$initialize(metric_optimization_higher_better = FALSE)

      self$environment <- "mllrnrs"
      self$cluster_export <- ranger_ce()
      private$fun_optim_cv <- ranger_optimization
      private$fun_fit <- ranger_fit
      private$fun_predict <- ranger_predict
      private$fun_bayesian_scoring_function <- ranger_bsF
    }
  )
)


ranger_ce <- function() {
  c("ranger_optimization", "ranger_fit", "metric",
    "ranger_predict", "ranger_predict_base", "ranger_cv")
}

ranger_bsF <- function(...) { # nolint

  params <- list(...)

  params <- kdry::list.append(
    main_list = params,
    append_list = method_helper$execute_params["cat_vars"]
  )

  set.seed(seed)#, kind = "L'Ecuyer-CMRG")
  bayes_opt_ranger <- ranger_optimization(
    x = x,
    y = y,
    params = params,
    fold_list = method_helper$fold_list,
    ncores = 1L, # important, as bayesian search is already parallelized
    seed = seed
  )

  ret <- kdry::list.append(
    list("Score" = bayes_opt_ranger$metric_optim_mean),
    bayes_opt_ranger
  )

  return(ret)
}

# ranger-cv is not implemented yet
ranger_cv <- function(
    x,
    y,
    params,
    fold_list,
    ncores,
    seed
) {
  stopifnot(
    is.list(params)
  )

  outlist <- list()

  # currently, there is no cross validation implemented in the ranger package.
  # as the code has already been written for xgboost, I just adapt it here
  # to work for survival models with ranger and to accept a list of parameters
  # from the parmeter grid-search.

  # loop over the folds
  for (fold in names(fold_list)) {

    # get row-ids of the current fold
    ranger_train_idx <- fold_list[[fold]]

    # train the model for this cv-fold
    args <- kdry::list.append(
      list(
        x = kdry::mlh_subset(x, ranger_train_idx),
        y = kdry::mlh_subset(y, ranger_train_idx),
        ncores = ncores,
        seed = seed
      ),
      params
    )

    if ("case_weights" %in% names(args)) {
      args$case_weights <- kdry::mlh_subset(
        args$case_weights, ranger_train_idx
      )
    }

    outlist[[fold]] <- list()

    set.seed(seed)
    outlist[[fold]][["cvfit"]] <- do.call(ranger_fit, args)
    outlist[[fold]][["train_idx"]] <- ranger_train_idx

  }
  return(outlist)
}

ranger_optimization <- function(
    x,
    y,
    params,
    fold_list,
    ncores,
    seed
  ) {

  # initialize a dataframe to store the results
  results_df <- data.table::data.table(
    "fold" = character(0),
    "metric" = numeric(0)
  )

  cvfit_list <- ranger_cv(
    x = x,
    y = y,
    params = params,
    fold_list = fold_list,
    ncores = ncores,
    seed = seed
  )

  # check, if this is a classification context and select metric accordingly
  if (is.factor(y) || isTRUE(params$classification)) {
    msg <- "Classification: using 'mean classification error'"
    FUN <- metric("MMCE") # nolint
  } else {
    msg <- "Regression: using 'mean squared error'"
    FUN <- metric("MSE") # nolint
  }
  message(paste("\n", msg, "as optimization metric."))

  # currently, there is no cross validation implemented in the ranger package.
  # as the code has already been written for xgboost, I just adapt it here
  # to work for survival models with ranger and to accept a list of parameters
  # from the parmeter grid-search.

  # loop over the folds
  for (fold in names(cvfit_list)) {

    # get row-ids of the current fold
    cvfit <- cvfit_list[[fold]][["cvfit"]]
    ranger_train_idx <- cvfit_list[[fold]][["train_idx"]]

    pred_args <- list(
      model = cvfit,
      newdata = kdry::mlh_subset(x, -ranger_train_idx),
      ncores = ncores,
      cat_vars = params[["cat_vars"]]
    )

    # if probability = TRUE (in case of classification),
    # we need to transform back to classes in order to be able to calculate
    # the classification error metric
    if (isTRUE(params$probability)) {
      pred_args <- kdry::list.append(
        pred_args, list(reshape = TRUE)
      )
    }

    preds <- do.call(ranger_predict, pred_args)

    perf_args <- list(
      predictions = preds,
      ground_truth = kdry::mlh_subset(y, -ranger_train_idx)
    )
    perf <- mlexperiments::metric_types_helper(
      FUN = FUN,
      y = y,
      perf_args = perf_args
    )


    # save the results of this fold into a dataframe
    # from help("ranger::ranger"):
    # prediction.error - Overall out of bag prediction error. [...] for
    # survival one minus Harrell's C-index.
    results_df <- data.table::rbindlist(
      l = list(
        results_df,
        list(
          "fold" = fold,
          "oob_metric" = 1 - cvfit$prediction.error,
          "validation_metric" = perf
        )
      ),
      fill = TRUE
    )
  }

  res <- list(
    "metric_optim_mean" = mean(results_df$validation_metric)
  )

  return(res)
}

# pass parameters as ...
ranger_fit <- function(x, y, ncores, seed, ...) {
  kwargs <- list(...)

  var_handler <- mlexperiments::handle_cat_vars(kwargs)
  cat_vars <- var_handler$cat_vars
  ranger_params <- var_handler$params

  x <- kdry::dtr_matrix2df(matrix = x, cat_vars = cat_vars)

  # rename mlexperiments "case_weights" to implementation specific (ranger)
  # "case.weights"
  if ("case_weights" %in% names(ranger_params)) {
    stopifnot(
      "late fail: `case_weights` must be of same length as `y`" =
        length(ranger_params$case_weights) == length(y)
    )
    names(ranger_params)[which(names(ranger_params) == "case_weights")] <-
      "case.weights"
  }

  args <- kdry::list.append(
    list(
      x = x,
      y = y,
      num.threads = ncores,
      oob.error = TRUE
    ),
    ranger_params
  )

  set.seed(seed)
  # fit the model
  bst <- do.call(ranger::ranger, args)
  return(bst)
}

ranger_predict_base <- function(model, newdata, ncores, ...) {

  kwargs <- list(...)

  var_handler <- mlexperiments::handle_cat_vars(kwargs)
  cat_vars <- var_handler$cat_vars
  ranger_params <- var_handler$params

  predict_args <- kdry::list.append(
    list(
      object = model,
      data = kdry::dtr_matrix2df(matrix = newdata, cat_vars = cat_vars),
      num.threads = ncores,
      type = "response"
    ),
    ranger_params
  )

  return(do.call(stats::predict, predict_args))
}

ranger_predict <- function(model, newdata, ncores, ...) {
  preds_obj <- ranger_predict_base(model, newdata, ncores, ...)
  preds <- preds_obj$predictions
  kwargs <- list(...)
  if (!is.null(kwargs$reshape)) {
    if (isTRUE(kwargs$reshape)) {
      preds <- kdry::mlh_reshape(preds)
    }
  }
  if (!is.null(kwargs$prob)) {
    stopifnot(!is.null(kwargs$positive))
    if (isTRUE(kwargs$prob)) {
      preds <- data.table::as.data.table(preds)[, get(kwargs$positive)]
    }
  }
  return(preds)
}
