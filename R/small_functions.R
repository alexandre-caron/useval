#' Logit Inverse Transformation
#'

logitinv <- function(x) {
  1/(1 + exp(-x))
}

short_pl <- function(posterior_pl, j) {
  lapply(posterior_pl, function(x) {
    if(is.null(x)) return(NULL)
    if(ncol(x) < (j+1)) return(NULL)
    as.matrix(x[,(j+1):ncol(x)])
  })
}

log_pdf <- function(p, n, mu, s2){
  prob = -(log(p/(1-p)) - mu)^2/(2*s2) + (n-1)*log(1-p) - log(p)
  return(prob)
}
