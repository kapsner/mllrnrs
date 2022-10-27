#' @title R6 Class to construct a Glmnet survival learner for Cox regression
#'
#' @description
#' The `LearnerSurvXgboostCox` class is the interface to perform a Cox
#'   regression with the `glmnet` R package for use with the `mlexperiments`
#'   package.
#'
#' @details
#' Optimization metric: C-index
#' Can be used with
#' * mlexperiments::MLTuneParameters
#' * mlexperiments::MLCrossValidation
#' * mlexperiments::MLNestedCVs
#'
#' @seealso [glmnet::glmnet()], [glmnet::cv.glmnet()]
#'
#' @examples
#' # survival analysis
#'
#' dataset <- survival::colon |>
#'   data.table::as.data.table() |>
#'   na.omit()
#' dataset <- dataset[get("etype") == 2, ]
#'
#' seed <- 123
#' surv_cols <- c("status", "time", "rx")
#'
#' feature_cols <- colnames(dataset)[3:(ncol(dataset) - 1)]
#'
#' param_list_glmnet <- expand.grid(
#'   alpha = seq(0, 1, .2)
#' )
#'
#' ncores <- 2L
#'
#' split_vector <- splitTools::multi_strata(
#'   df = dataset[, .SD, .SDcols = surv_cols],
#'   strategy = "kmeans",
#'   k = 4
#' )
#'
#' train_x <- model.matrix(
#'   ~ -1 + .,
#'   dataset[, .SD, .SDcols = setdiff(feature_cols, surv_cols[1:2])]
#' )
#' train_y <- survival::Surv(
#'   event = (dataset[, get("status")] |>
#'              as.character() |>
#'              as.integer()),
#'   time = dataset[, get("time")],
#'   type = "right"
#' )
#'
#'
#' fold_list <- splitTools::create_folds(
#'   y = split_vector,
#'   k = 3,
#'   type = "stratified",
#'   seed = seed
#' )
#'
#' surv_glmnet_cox_optimizer <- mlexperiments::MLCrossValidation$new(
#'   learner = mllrnrs::LearnerSurvGlmnetCox$new(),
#'   fold_list = fold_list,
#'   ncores = ncores,
#'   seed = seed
#' )
#' surv_glmnet_cox_optimizer$learner_args <- list(
#'   alpha = 0.8,
#'   lambda = 0.002
#' )
#' surv_glmnet_cox_optimizer$performance_metric <- c_index
#' surv_glmnet_cox_optimizer$performance_metric_name <- "C-index"
#'
#' # set data
#' surv_glmnet_cox_optimizer$set_data(
#'   x = train_x,
#'   y = train_y
#' )
#'
#' surv_glmnet_cox_optimizer$execute()

#' @export
#'
LearnerSurvGlmnetCox <- R6::R6Class( # nolint
  classname = "LearnerSurvGlmnetCox",
  inherit = mlexperiments::MLLearnerBase,
  public = list(

    #' @description
    #' Create a new `LearnerSurvGlmnetCox` object.
    #'
    #' @return A new `LearnerSurvGlmnetCox` R6 object.
    #'
    #' @examples
    #' LearnerSurvGlmnetCox$new()
    #'
    initialize = function() {
      if (!requireNamespace("glmnet", quietly = TRUE)) {
        stop(
          paste0(
            "Package \"glmnet\" must be installed to use ",
            "'learner = \"LearnerSurvGlmnetCox\"'."
          ),
          call. = FALSE
        )
      }
      super$initialize(metric_optimization_higher_better = TRUE)
      self$environment <- "mllrnrs"
      self$cluster_export <- surv_glmnet_cox_ce()
      private$fun_optim_cv <- surv_glmnet_cox_optimization
      private$fun_fit <- surv_glmnet_cox_fit
      private$fun_predict <- surv_glmnet_cox_predict
      private$fun_bayesian_scoring_function <- surv_glmnet_cox_bsF
    }
  )
)


surv_glmnet_cox_ce <- function() {
  c("surv_glmnet_cox_optimization", "surv_glmnet_cox_fit")
}

surv_glmnet_cox_bsF <- function(alpha) { # nolint
  # call to surv_glmnet_cox_optimization here with ncores = 1, since the
  # Bayesian search is parallelized already / "FUN is fitted n times
  # in m threads"
  set.seed(seed)#, kind = "L'Ecuyer-CMRG")
  bayes_opt_glmnet <- surv_glmnet_cox_optimization(
    x = x,
    y = y,
    params = list("alpha" = alpha),
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
surv_glmnet_cox_optimization <- function(
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

  set.seed(seed)
  # fit the glmnet-cv-model
  cvfit <- glmnet::cv.glmnet(
    x = x,
    y = y,
    family = "cox",
    foldids = glmnet_fids$fold_id,
    type.measure = "C",
    alpha = params$alpha,
    parallel = go_parallel,
    standardize = TRUE
  )

  res <- list(
    "metric_optim_mean" = max(cvfit$cvm),
    "lambda" = cvfit$lambda.min
  )

  return(res)
}

surv_glmnet_cox_fit <- function(x, y, ncores, seed, ...) {
  kwargs <- list(...)
  stopifnot("lambda" %in% names(kwargs),
            "alpha" %in% names(kwargs))

  set.seed(seed)
  # train final model with a given lambda / alpha
  fit <- glmnet::glmnet(
    x = x,
    y = y,
    family = "cox",
    standardize = TRUE,
    alpha = kwargs$alpha,
    lambda = kwargs$lambda
  )
  return(fit)
}

surv_glmnet_cox_predict <- function(model, newdata, ncores, ...) {
  kwargs <- list(...) # nolint
  # From the docs:
  # Type "response" gives [...] the fitted relative-risk for "cox".
  return(stats::predict(model, newx = newdata, type = "response")[, 1])
}
