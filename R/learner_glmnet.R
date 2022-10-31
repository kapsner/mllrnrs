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
#' * [mlexperiments::MLNestedCVs]
#'
#' @seealso [glmnet::glmnet()], [glmnet::cv.glmnet()]
#'
#' @examples

#' @export
#'
LearnerGlmnet <- R6::R6Class( # nolint
  classname = "LearnerGlmnet",
  inherit = mlexperiments::MLLearnerBase,
  public = list(

    #' @description
    #' Create a new `LearnerGlmnet` object.
    #'
    #' @return A new `LearnerGlmnet` R6 object.
    #'
    #' @examples
    #' LearnerGlmnet$new()
    #'
    initialize = function() {
      if (!requireNamespace("glmnet", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"glmnet\" must be installed to use ",
            "'learner = \"LearnerGlmnet\"'."
          ),
          call. = FALSE
        )
      }
      super$initialize(metric_optimization_higher_better = FALSE)
      self$environment <- "mllrnrs"
      self$cluster_export <- glmnet_ce()
      private$fun_optim_cv <- glmnet_optimization
      private$fun_fit <- glmnet_fit
      private$fun_predict <- glmnet_predict
      private$fun_bayesian_scoring_function <- glmnet_bsF
    }
  )
)


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
    inherits(x = y, what = "Surv"),
    is.list(params),
    length(params) == 1L,
    names(params) == "alpha"
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
    "metric_optim_mean" = max(cvfit$cvm),
    "lambda" = cvfit$lambda.min
  )

  return(res)
}

glmnet_fit <- function(x, y, ncores, seed, ...) {
  kwargs <- list(...)
  stopifnot("lambda" %in% names(kwargs),
            "alpha" %in% names(kwargs))

  set.seed(seed)
  # train final model with a given lambda / alpha
  fit_args <- kdry::list.append(
    list(
      x = x,
      y = y
    ),
    kwargs
  )
  fit <- do.call(glmnet::glmnet, fit_args)
  return(fit)
}

glmnet_predict <- function(model, newdata, ncores, ...) {
  kwargs <- list(...) # nolint
  # From the docs:
  # Type "response" gives [...] the fitted relative-risk for "cox".
  return(stats::predict(model, newx = newdata, type = "response")[, 1])
}
