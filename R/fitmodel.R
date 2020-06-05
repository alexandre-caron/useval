
#' @title Fit the matrix-based model with Stan
#'
#' @description \code{fit_model} returns a stanfit object. The \code{fit_model}
#'   function fit the matrix \code{d} to the matrix-based model given the number
#'   of column \code{m} and the following the following prior : \describe{
#'   \item{mu}{A Gaussian distribution with mean \code{mu_prior_mean} and
#'   standard deviation \code{mu_prior_sd} parameters} \item{s2}{An inverse
#'   gamma distribution with shape \code{s2_prior_a} and scale
#'   \code{s2_prior_b}}}
#'
#' @export
#'
#' @seealso \url{https://mc-stan.org/math/}.
#'
#' @param d A logical matrix.
#' @param m An integer scalar >= to the number of column in \code{d}.
#' @param mu_prior_mean A numeric scalar.
#' @param mu_prior_sd A numeric scalar >0.
#' @param s2_prior_a A numeric scalar >0.
#' @param s2_prior_b A numeric scalar >0.
#' @param ... Other arguments passed on to `rstan::sampling` (e.g. iter,
#'   chains, etc.).
#'
#' @return An object of class `stanfit` returned by `rstan::sampling`.
#'


fit_model <- function(d, m,
                      mu_prior_mean, mu_prior_sd, s2_prior_a, s2_prior_b,
                      ...) {

  if(ncol(d) == 0) {
    message("The matrix d is empty (no problems discovered so far)")
    if(m == 0) stop("The loglikelihood of an empty matrix given m=0 is 0")
  }

  ms <- colSums(d)
  if(any(ms == 0)) {
    message("Columns ")
  }

  j <- ncol(d)
  n <- nrow(d)

  x <- as.array(c(ms, rep(0, m - j)))
  model_data <- list(n = n, m = m, x = x,
                     mu_prior_mean = mu_prior_mean, mu_prior_sd = mu_prior_sd,
                     s2_prior_a = s2_prior_a, s2_prior_b = s2_prior_b)

  rstan::sampling(object = stanmodels$mu_s2, data = model_data,
                  refresh = 0, ... = ...)
}



#' @title Compute the integrated likelihood
#'
#' @description \code{integrated_likelihood} returns the integrated log marginal
#'   likelihood via bridge sampling.
#'
#' @export
#'
#' @param stan_fit A stanfit object.
#' @param seed A integer scalar. If \code{NULL}, no seed is set for bridge
#'   sampling.
#' @param ... Other arguments passed on to `bridgesampling::bridge_sampler`.
#'
#' @return A numeric scalar.
#'

integrated_likelihood <- function(stan_fit, seed = NULL, ...) {
  if(!is.null(seed)) {
    set.seed(seed = seed)
  }
  bridgesampling::bridge_sampler(stan_fit,silent = TRUE, ...)$logml
}



#' @title Provide posterior sample
#'
#' @description \code{posterior_sample} returns the integrated likelihood
#'   via bridge sampling.
#'
#' @export
#'
#' @seealso \url{https://mc-stan.org/math/}.
#'
#' @param stan_fit A stanfit object.
#'
#' @return A numeric scalar.
#'

posterior_sample <- function(stan_fit) {
  sample_size <- (stan_fit@sim$warmup+1):stan_fit@sim$iter
  sample_list <- lapply(stan_fit@sim$samples, function(x) {
    simplify2array(x[3:(length(x)-1)])[sample_size,]
  })
  post_sample <- logitinv(do.call(rbind,sample_list))
  unname(post_sample)
}
