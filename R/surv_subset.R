.subset_surv <- function(y, train_idx, type = "right") {
  # when subsetting a Surv-object directly, it gets destroyed; this is a
  # wrapper for subsetting surv objects correctly
  ret <- survival::Surv(
    time = y[train_idx, 1],
    event = y[train_idx, 2],
    type = type
  )
  return(ret)
}
