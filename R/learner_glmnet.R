#' @title R6 Class to construct a Glmnet learner
#'
#' @description
#' The `LearnerGlmnet` class is the interface to the `glmnet` R package for use
#'   with the `mlexperiments` package.
#'
#' @details
#' Optimization metric:
#' Can be used with
#' * [mlexperiments::MLTuneParameters]
#' * [mlexperiments::MLCrossValidation]
#' * [mlexperiments::MLNestedCV]
#'
#' @seealso [glmnet::glmnet()], [glmnet::cv.glmnet()]
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
#' glmnet_cv <- mlexperiments::MLCrossValidation$new(
#'   learner = mllrnrs::LearnerGlmnet$new(
#'     metric_optimization_higher_better = FALSE
#'   ),
#'   fold_list = fold_list,
#'   ncores = 2,
#'   seed = 123
#' )
#' glmnet_cv$learner_args <- list(
#'   alpha = 1,
#'   lambda = 0.1,
#'   family = "binomial",
#'   type.measure = "class",
#'   standardize = TRUE
#' )
#' glmnet_cv$predict_args <- list(type = "response")
#' glmnet_cv$performance_metric_args <- list(positive = "1")
#' glmnet_cv$performance_metric <- mlexperiments::metric("auc")
#'
#' # set data
#' glmnet_cv$set_data(
#'   x = train_x,
#'   y = train_y
#' )
#'
#' glmnet_cv$execute()

#' @export
#'
LearnerGlmnet <- R6::R6Class( # nolint
  classname = "LearnerGlmnet",
  inherit = mlexperiments::MLLearnerBase,
  public = list(

    #' @description
    #' Create a new `LearnerGlmnet` object.
    #'
    #' @param metric_optimization_higher_better A logical. Defines the direction
    #'  of the optimization metric used throughout the hyperparameter
    #'  optimization.
    #'
    #' @return A new `LearnerGlmnet` R6 object.
    #'
    #' @examples
    #' LearnerGlmnet$new(metric_optimization_higher_better = FALSE)
    #'
    initialize = function(metric_optimization_higher_better) { # nolint
      if (!requireNamespace("glmnet", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"glmnet\" must be installed to use ",
            "'learner = \"LearnerGlmnet\"'."
          ),
          call. = FALSE
        )
      }
      super$initialize(
        metric_optimization_higher_better = metric_optimization_higher_better
      )
      # type.measure:
      # * default: "deviance" (lower = better), for gaussian models, logistic
      #   and poisson regression
      # * "class": misclassification error (lower = better), for binomial and
      #   multinomial logistic regression
      # * "auc": two-class logistic regression
      self$environment <- "mllrnrs"
      self$cluster_export <- glmnet_ce()
      private$fun_optim_cv <- function(...) {
        kwargs <- list(...)
        stopifnot(
          (sapply(
            X = c("family", "type.measure"),
            FUN = function(x) {
              x %in% names(kwargs$params)
            }
          )),
          .check_glmnet_params(kwargs$params,
                               self$metric_optimization_higher_better)
        )
        return(do.call(glmnet_optimization, kwargs))
      }
      private$fun_fit <- glmnet_fit
      private$fun_predict <- glmnet_predict
      private$fun_bayesian_scoring_function <- function(...) {
        kwargs <- list(...)
        stopifnot(
          (sapply(
            X = c("family", "type.measure"),
            FUN = function(x) {
              x %in% names(kwargs)
            }
          )),
          .check_glmnet_params(kwargs, self$metric_optimization_higher_better)
        )
        return(do.call(glmnet_bsF, kwargs))
      }
    }
  )
)

.check_glmnet_params <- function(params, higher_better) {
  stopifnot(
    params$family %in% c("gaussian", "binomial", "poisson",
                         "multinomial", "mgaussian"),
    params$type.measure != "C",
    ifelse(
      test = params$family == "binomial" &&
        params$type.measure == "auc",
      yes = isTRUE(higher_better),
      no = isFALSE(higher_better)
    )
  )
  TRUE
}


glmnet_ce <- function() {
  c("glmnet_optimization", "glmnet_fit")
}

glmnet_bsF <- function(...) { # nolint
  kwargs <- list(...)
  # call to glmnet_optimization here with ncores = 1, since the
  # Bayesian search is parallelized already / "FUN is fitted n times
  # in m threads"
  set.seed(seed)#, kind = "L'Ecuyer-CMRG")
  bayes_opt_glmnet <- glmnet_optimization(
    x = x,
    y = y,
    params = kwargs,
    fold_list = method_helper$fold_list,
    ncores = 1L, # important, as bayesian search is already parallelized
    seed = seed
  )

  ret <- kdry::list.append(
    list("Score" = bayes_opt_glmnet$metric_optim_mean),
    bayes_opt_glmnet
  )

  return(ret)
}

# tune lambda
glmnet_optimization <- function(
    x,
    y,
    params,
    fold_list,
    ncores,
    seed
  ) {
  stopifnot(
    is.list(params),
    (sapply(
      X = c("alpha", "family", "type.measure"),
      FUN = function(x) {
        x %in% names(params)
      }
    )),
    (!sapply(
      X = c("x", "y", "foldid"),
      FUN = function(x) {
        x %in% names(params)
      }
    ))
  )

  FUN <- ifelse( # nolint
    test = params$family == "binomial" &&
      params$type.measure == "auc",
    yes = max,
    no = min
  )

  # from the documentation (help("glmnet::cv.glmnet")):
  # If users would like to cross-validate alpha as well, they should call
  # cv.glmnet with a pre-computed vector foldid, and then use this same
  # fold vector in separate calls to cv.glmnet with different values
  # of alpha.
  glmnet_fids <- kdry::mlh_outsample_row_indices(
    fold_list = fold_list,
    dataset_nrows = nrow(x),
    type = "glmnet"
  )

  # initialize the parallel backend, if required
  if (ncores > 1L) {
    cl <- kdry::pch_register_parallel(ncores)
    on.exit(
      expr = {
        kdry::pch_clean_up(cl)
      }
    )
    go_parallel <- TRUE
  } else {
    go_parallel <- FALSE
  }

  cv_args <- kdry::list.append(
    params,
    list(
      x = x,
      y = y,
      foldid = glmnet_fids$fold_id,
      parallel = go_parallel
    )
  )

  set.seed(seed)
  # fit the glmnet-cv-model
  cvfit <- do.call(glmnet::cv.glmnet, cv_args)

  res <- list(
    "metric_optim_mean" = FUN(cvfit$cvm),
    "lambda" = cvfit$lambda.min
  )

  return(res)
}

glmnet_fit <- function(x, y, ncores, seed, ...) {
  kwargs <- list(...)
  stopifnot((sapply(
              X = c("lambda", "alpha", "family"),
              FUN = function(x) {
                x %in% names(kwargs)
              }
            )),
            (!sapply(
              X = c("x", "y"),
              FUN = function(x) {
                x %in% names(kwargs)
              }
            )))

  fit_args <- kdry::list.append(
    list(
      x = x,
      y = y
    ),
    kwargs
  )
  set.seed(seed)
  # train final model with a given lambda / alpha
  fit <- do.call(glmnet::glmnet, fit_args)
  return(fit)
}

glmnet_predict <- function(model, newdata, ncores, ...) {
  kwargs <- list(...) # nolint
  pred_args <- kdry::list.append(
    list(
      object = model,
      newx = newdata
    ),
    kwargs
  )
  preds <- do.call(stats::predict, pred_args)
  if (!is.null(kwargs$reshape)) {
    if (isTRUE(kwargs$reshape)) {
      preds <-  preds[, , 1]
      preds <- kdry::mlh_reshape(preds)
    }
  } else {
    preds <- preds[, 1]
  }
}
