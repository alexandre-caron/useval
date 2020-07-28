

#' @title List of parameters for fitting the matrix-based model
#'
#' @description \code{discovery_parameters} returns a list of parameters used by
#'   fitting function (e.g. \code{fit_model}) and prior hyper-parameters for the
#'   logit-normal distribution (mean and variance) \describe{\item{mean (mu)}{A
#'   Gaussian distribution with mean \code{mu_prior_mean} and standard deviation
#'   \code{mu_prior_sd} parameters} \item{variance (s2)}{An inverse gamma
#'   distribution with shape \code{s2_prior_a} and scale \code{s2_prior_b}}}.
#'   Non-informative prior value are set as default.
#'
#' @seealso [fit_model()].
#'
#' @param d A logical matrix.
#' @param mu_prior_mean A numeric scalar. The default is 0.
#' @param mu_prior_sd A numeric scalar >0. The default is 1.5.
#' @param s2_prior_a A numeric scalar >0. The default is 20.
#' @param s2_prior_b A numeric scalar >0. The default is 20.
#'
#' @return A list of parameters to be passed to fitting functions.
#'
#' @examples
#'
#' @export model_parameters
#'

model_parameters <- function(d,
                       mu_prior_mean = 0, mu_prior_sd = 1.5,
                       s2_prior_a = 20, s2_prior_b = 20) {

  if(any(colSums(d) == 0)) {
    message(paste(sum(colSums(d) == 0),
                  "columns containing only FALSE removed:",
                  "a problem has to be detected at least once"))
    d <- d[,colSums(d)>0]
  }

  ms <- colSums(d)
  j <- ncol(d)
  n <- nrow(d)

  if(j == 0) {
    message("The matrix d is empty")
  }


  list(j = j, n = n, ms = ms,
       mu_prior_mean = mu_prior_mean, mu_prior_sd = mu_prior_sd,
       s2_prior_a = s2_prior_a, s2_prior_b = s2_prior_b)

}

