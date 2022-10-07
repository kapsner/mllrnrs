#' @import data.table
#' @importFrom R6 R6Class
NULL

mlexperiments_default_options <- list(
  mlexperiments.learner = c(
    LearnerSurvCoxPHCox$classname,
    LearnerSurvGlmnetCox$classname, # = "LearnerSurvGlmnetCox"
    LearnerSurvXgboostCox$classname,
    LearnerSurvRangerCox$classname
  ),
  mlexperiments.optim.xgb.nrounds = 5000L,
  mlexperiments.optim.xgb.early_stopping_rounds = 500L,
  mlexperiments.xgb.print_every_n = 50L,
  mlexperiments.xgb.verbose = FALSE
)


.onLoad <- function(libname, pkgname) {
  op <- options()
  toset <- !(names(mlexperiments_default_options) %in% names(op))
  if (any(toset)) options(mlexperiments_default_options[toset])
  invisible()
}

NULL
