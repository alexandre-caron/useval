#' Logit Inverse Transformation
#'

logitinv <- function(x) {
  1/(1 + exp(-x))
}
