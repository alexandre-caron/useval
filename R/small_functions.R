#' Logit Inverse Transformation
#'
#' Computes the inverse of the logit transformation
#'
#' @param x A real number
#'
#'
#' @return The logit inverse transformation
logitinv <- function(x) {
  1/(1 + exp(-x))
}


#' Logit  Transformation
#'
#' Computes the logit transformation
#'
#' @param p A number between 0 and 1
#'
#'
#' @return The logit transformation
logit <- function(p){
  log(p/(1-p))
}


#' Short probability array
#'
#' Extract only the last columns from a generated posterior probabilities object
#'
#' @param posterior_pl As list containing the generated posterior probabilities
#' @param j Number of different problems observed
#'
#'
#' @return The logit transformation
short_pl <- function(posterior_pl, j) {
  lapply(posterior_pl, function(x) {
    if(is.null(x)) return(NULL)
    if(ncol(x) < (j+1)) return(NULL)
    as.matrix(x[,(j+1):ncol(x)])
  })
}


#' Log of the logit normal probability distribution function
#'
#' Coomputes ...
#'
#' @param p The value of the probability to evaluate the pdf
#' @param n Number of individuals
#' @param mu Expectation
#' @param s2 Variance
#'
#'
#' @return The pdf of the logit-normal distribution
log_pdf <- function(p, n, mu, s2){
  -(logit(p) - mu)^2/(2*s2) + (n-1)*log(1-p) - log(p)
}









