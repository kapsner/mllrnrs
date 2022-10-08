#' @export
c_index <- function(ground_truth, predictions) {
  return(glmnet::Cindex(pred = predictions, y = ground_truth))
}
