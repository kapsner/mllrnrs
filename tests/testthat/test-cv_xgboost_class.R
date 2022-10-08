library(mlbench)
data("DNA")
dataset <- DNA |>
  data.table::as.data.table() |>
  na.omit()

seed <- 123
feature_cols <- colnames(dataset)[160:180]

param_list_xgboost <- expand.grid(
  subsample = seq(0.6, 1, .2),
  colsample_bytree = seq(0.6, 1, .2),
  min_child_weight = seq(1, 5, 4),
  learning_rate = seq(0.1, 0.2, 0.1),
  max_depth = seq(1, 5, 4)
)

ncores <- ifelse(
  test = parallel::detectCores() > 4,
  yes = 4L,
  no = ifelse(
    test = parallel::detectCores() < 2L,
    yes = 1L,
    no = parallel::detectCores()
  )
)

train_x <- model.matrix(
  ~ -1 + .,
  dataset[, .SD, .SDcols = feature_cols]
)
train_y <- as.integer(dataset[, get("Class")]) - 1L

options("mlexperiments.bayesian.max_init" = 10L)
options("mlexperiments.optim.xgb.nrounds" = 100L)
options("mlexperiments.optim.xgb.early_stopping_rounds" = 10L)

fold_list <- splitTools::create_folds(
  y = train_y,
  k = 5,
  type = "stratified",
  seed = seed
)

test_that(
  desc = "test cv, multi:softprob - xgboost",
  code = {

    xgboost_optimization <- mlexperiments::MLCrossValidation$new(
      learner = mllrnrs::LearnerXgboost$new(),
      fold_list = fold_list,
      ncores = ncores,
      seed = seed
    )
    xgboost_optimization$learner_args <- c(
      as.list(
        data.table::data.table(
          param_list_xgboost[37, ],
          stringsAsFactors = FALSE
        ),
      ),
      list(
        objective = "multi:softprob",
        eval_metric = "mlogloss",
        num_class = 3
      ),
      nrounds = 45L
    )
    xgboost_optimization$predict_args <- list(reshape = TRUE)
    xgboost_optimization$performance_metric <- mlexperiments::metric("bacc")
    xgboost_optimization$performance_metric_name <- "Balanced accuracy"

    # set data
    xgboost_optimization$set_data(
      x = train_x,
      y = train_y
    )

    cv_results <- xgboost_optimization$execute()
    expect_type(cv_results, "list")
    expect_equal(dim(cv_results), c(5, 10))
    expect_true(inherits(
      x = xgboost_optimization$results,
      what = "mlexCV"
    ))
  }
)
