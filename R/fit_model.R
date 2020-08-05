
#' @title Fit the matrix-based model with Stan
#'
#' @description \code{fit_model} returns a stanfit object. The \code{fit_model}
#'   function fit the matrix \code{d} to the matrix-based model for a given
#'   number of column \code{m} and and prior hyper-parameters for the
#'   logit-normal distribution (mean and variance)
#'   \describe{ \item{mu}{A Gaussian distribution with mean \code{mu_prior_mean}
#'   and standard deviation \code{mu_prior_sd} parameters} \item{s2}{An inverse
#'   gamma distribution with shape \code{s2_prior_a} and scale
#'   \code{s2_prior_b}}}
#'
#' @seealso \url{https://mc-stan.org/math/}.
#' @seealso [model_parameters()].
#'
#' @param parameters A list from \code{model_parameters()}.
#' @param m An integer scalar >= to the number of column in \code{d}.
#' @param ... Other arguments passed on to `rstan::sampling` (e.g. iter, chains,
#'   etc.).
#'
#' @return An object of class `stanfit` returned by `rstan::sampling`.
#'
#' @family fitting functions
#'
#' @export fit_model
#'

fit_model <- function(parameters, m, ...) {

  x_padding <- m - parameters$j

  if(x_padding < 0) {
    stop(paste0("The given number of problems (m=", m, ") ",
                "must be higher or equal to the number of problems discovered ",
                "so far (",parameters$j, ") ", "in the matrix d"))
  }

  x <- as.array(c(parameters$ms, rep(0, x_padding)))

  if(length(x) == 0) {
    message("The loglikelihood of the empty matrix given m=0 is 0")
    return(fit = NA)
  }

  parameters$x <- x
  parameters$m <- m
  parameters$ms <- NULL
  parameters$j <- NULL

  fit <- rstan::sampling(object = stanmodels$mu_s2,
                  data = parameters, refresh = 0, ... = ...)

  diagnostic <- list()
  diag <- do.call(rbind, lapply(rstan::get_sampler_params(fit), colMeans))
  diagnostic <- cbind(m = parameters$m, diag)

  list(fit = fit, diagnostic = diagnostic)

}


#' @title Compute the integrated likelihood
#'
#' @description \code{integrated_likelihood} compute the integrated log
#'   marginal likelihood via bridge sampling.
#'
#' @param stan_fit A stanfit object.
#' @param seed A integer scalar. If \code{NULL}, no seed is set for bridge
#'   sampling.
#' @param ... Other arguments passed on to `bridgesampling::bridge_sampler`.
#'
#' @return A numeric scalar.
#'
#' @family fitting functions
#'
#' @export integrated_likelihood
#'

integrated_likelihood <- function(stan_fit, seed = NULL, ...) {

  fit <- stan_fit$fit

  if(!is.null(seed)) {
    set.seed(seed = seed)
  }

  bridgesampling::bridge_sampler(fit, silent = TRUE, ...)$logml

}



#' @title Extract posterior sample for pl
#'
#' @description \code{posterior_pl} extract the posterior sample of pl from the
#'   MCMC chains
#'
#' @seealso \url{https://mc-stan.org/math/}.
#'
#' @param stan_fit A stanfit object.
#'
#' @return A numeric scalar.
#'
#' @family fitting functions
#'
#' @export posterior_pl
#'

posterior_pl <- function(stan_fit) {

  fit <- stan_fit$fit

  sample_size <- (fit@sim$warmup+1):fit@sim$iter
  sample_list <- lapply(fit@sim$samples, function(x) {
    pl <- x[3:(length(x)-1)]
    if(length(pl)>1) {
      simplify2array(pl)[sample_size,]
    } else {
      matrix(pl[[1]][sample_size], ncol = 1)
    }
  })

  pl <- logitinv(do.call(rbind,sample_list))

  unname(pl)

}


#' @title Extract posterior sample for mu and s2
#'
#' @description \code{posterior_mu_s2} extract the posterior sample of mu and s2 from the
#'   MCMC chains
#'
#' @seealso \url{https://mc-stan.org/math/}.
#'
#' @param stan_fit A stanfit object.
#'
#' @return A numeric scalar.
#'
#' @family fitting functions
#'
#' @export posterior_mu_s2
#'

posterior_mu_s2 <- function(stan_fit) {

  fit <- stan_fit$fit

  sample_size <- (fit@sim$warmup+1):fit@sim$iter
  sample_list <- lapply(fit@sim$samples, function(x) {
    mu_s2 <- x[1:2]
    simplify2array(mu_s2)[sample_size,]
  })

  mu_s2 <- do.call(rbind,sample_list)

  mu_s2
  #unname(mu_s2)
}



#' @title Fit the model and provide posterior sample and integrated likelihood
#'
#' @description \code{posterior_analysis} fit the model using \code{fit_model}
#'   on a m grid from 0 to \code{m_max()}. It provides \describe{\item{the
#'   integrated likelihood}{computed via bridge sampling by
#'   \code{integrated_likelihood}} \item{the posterior_pl}{extracted from the
#'   MCMC chains by \code{posterior_pl}} \item{a summary of model
#'   diagnostic}{computed by \code{fit_model}}}
#'
#' @seealso [fit_model()].
#' @seealso [integrated_likelihood()].
#' @seealso [posterior_pl()].
#'
#' @param parameters A list from \code{model_parameters()}.
#' @param m_max An integer scalar >= to the number of column in \code{d}.
#' @param ... Other arguments passed on to `rstan::sampling` (e.g. iter, chains,
#'   etc.).#'
#' @return
#'
#' @family fitting functions
#'
#' @export posterior_analysis
#'
#' @examples
#'

posterior_analysis <- function(parameters, m_max, ...) {

  il <- numeric()
  pl <- list()
  mu_s2 <- list()
  diagnostic <- list()

  if(length(parameters$ms) == 0) {
    m_grid <- 1:m_max
    il[1] <- 0
  } else {
    m_grid <- parameters$j:m_max
    il[1:parameters$j] <- -Inf
  }

  for(i in m_grid){
    fit <- fit_model(parameters, i)
    diagnostic[[i+1]] <- fit$diagnostic
    il[i+1] <- integrated_likelihood(fit)
    pl[[i+1]] <- posterior_pl(fit)
    mu_s2[[i+1]] <- posterior_mu_s2(fit)
  }

  list(integrated_likelihood = il,
       posterior_pl = pl,
       posterior_mu_s2 = mu_s2,
       diagnostic = do.call(rbind, diagnostic),
       n = parameters$n)

}
