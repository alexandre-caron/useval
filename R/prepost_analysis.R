
#' @title TO DO sample_pm
#'
#' @description TO DO sample_pm
#'
#' @param joint_pm TO DO
#' @param size TO DO
#'
#' @return TO DO
#' @export sample_pm
#'

sample_pm <- function(joint_pm, size) {
  s <- sample(x = nrow(joint_pm), size = size,
              prob = joint_pm[,"p"], replace = TRUE)
  joint_pm[s, c("m_c", "m_nc")]
}


#' Title TO DO
#'
#' @param nb_sample TO DO
#' @param n TO DO
#' @param mu TO DO
#' @param s2 TO DO
#' @param ... Parameters related to optimal parameters of rstan sampling
#' function
#'
#' @return TO DO
#' @export
#'

rstan_p <- function(nb_sample, n, mu, s2, ...){
  fit = rstan::sampling(object = stanmodels$p,
                        data = list(n=n, mu=mu, s2=s2), refresh = 0, ... = ...)

  sample_size <- (fit@sim$warmup+1):fit@sim$iter

  if(length(sample_size)<nb_sample){
      logitinv(sample(fit@sim$samples[[1]]$logit_p[sample_size],
                  nb_sample, replace = TRUE))
  }else{
      logitinv(sample(fit@sim$samples[[1]]$logit_p[sample_size],
                  nb_sample, replace = FALSE))
  }
}



#' @title TO DO prepost_pl
#'
#' @description TO DO prepost_pl
#'
#' @param m a vector
#' @param pl a list
#' @param mu_s2 TO DO
#' @param n a vector
#' @param n_init TO DO
#' @param seed a number for the seed, default = NULL
#' @param ... optional parameters related to rstan_p function
#'
#' @return TO DO
#' @export prepost_pl
#'

prepost_pl <- function(m, pl, mu_s2, n, n_init, seed = NULL, ...) {
  if(!is.null(seed)) set.seed(seed)
  sapply(m, function(x){
    pl_m <- pl[[m+1]]
    mu_s2_m <- mu_s2[[m+1]]
    if(is.null(pl_m)) {
      return(n*0)
    } else {
      id = sample(x = 1:nrow(pl_m), size = 1)
      pl_m_sample <- pl_m[id,]
      mu_s2_sample <- mu_s2_m[id,]
      nb_undiscovered <- lapply(n, function(x) {
        sum(rbinom(length(pl_m_sample),x,pl_m_sample)== 0)
    })

    pl_m_post_undiscovered = lapply(1:length(n), function(k) {
        if (nb_undiscovered[[k]]>0) {
          rstan_p(nb_sample = nb_undiscovered[[k]], n = n[k] + n_init,
                              mu = mu_s2_sample[1],
                              s2 = mu_s2_sample[2],
                  ...=...)
        } else {
          0
        }
      })
      unlist(lapply(pl_m_post_undiscovered, function(x) {1 - prod(1-x)}))
    }
  })
}


#' @title TO DO multinom_sample
#'
#' @description TO DO multinom_sample
#'
#' @param prepost_pl_nc TO DO
#' @param prepost_pl_c TO DO
#' @param N TO DO
#'
#' @return Return the number of newly discovered problems by using the posterior
#' distribution
#' @export multinom_sample
#'

multinom_sample <- function(prepost_pl_nc, prepost_pl_c, N){
  lapply(1:nrow(prepost_pl_nc), function(i) {
    proba <- data.frame(nc = prepost_pl_nc[i,], c = prepost_pl_c[i,])
    t(apply(X = proba, 1, FUN = function(x) {
      rmultinom(1, size = N, prob = c(
        probok = (1 - x["c"])*(1 - x["nc"]),
        probc = x["c"],
        probnc = x["nc"] * (1 - x["c"])
      ))
    }))
  })
}


#' @title TO DO multinom_sample
#'
#' @description TO DO multinom_sample
#'
#' @param prepost_pl_nc TO DO
#' @param prepost_pl_c TO DO
#' @param N TO DO
#'
#' @return Returns the expected number of problems
#' @export multinom_sample
#'

multinom_expectation <- function(prepost_pl_nc, prepost_pl_c, N){
  lapply(1:nrow(prepost_pl_nc), function(i) {
    proba <- data.frame(nc = prepost_pl_nc[i,], c = prepost_pl_c[i,])
    t(apply(X = proba, 1, FUN = function(x) {
      N*c(probok = (1 - x["c"])*(1 - x["nc"]), probc = x["c"],
        probnc = x["nc"] * (1 - x["c"]))
    }))
  })
}


#' @title TO DO prepost_analysis
#'
#' @description TO DO prepost_analysis
#'
#' @param fit_nc TO DO
#' @param fit_c TO DO
#' @param sample_size TO DO
#' @param n_end_users TO DO
#' @param size Number of simulations to perform
#' @param ... optinal parameters related to prepost_pl function
#'
#' @return TO DO
#' @export prepost_analysis
#'

prepost_analysis <- function(fit_nc, fit_c, sample_size, n_end_users, size, ...){
  pm <- joint_pm(pd_nc = fit_nc$integrated_likelihood,
                 pd_c = fit_c$integrated_likelihood)
  j_nc <- min(pm[pm$px_nc>-Inf,"m_nc"])
  j_c <- min(pm[pm$px_nc>-Inf,"m_c"])
  pl_nc <- short_pl(posterior_pl = fit_nc$posterior_pl, j = j_nc)
  mu_s2_nc <- fit_nc$posterior_mu_s2
  pl_c <- short_pl(posterior_pl = fit_c$posterior_pl, j = j_c)
  mu_s2_c <- fit_c$posterior_mu_s2
  s_pm <- sample_pm(pm, size = size)
  res_prepost_nc <- sapply(1:length(s_pm$m_nc), function(x){
    prepost_pl(m = s_pm$m_nc[x], pl = pl_nc, mu_s2 = mu_s2_nc, n = sample_size, n_init = fit_nc$n, ...=...)
  })
  res_prepost_c <- sapply(1:length(s_pm$m_c), function(x){
    prepost_pl(m = s_pm$m_c[x], pl = pl_c, mu_s2 = mu_s2_c, n = sample_size, n_init = fit_c$n, ...=...)
  })
  # According to the use case replace multinom_sample by multinom_expectation
  y <- multinom_expectation(prepost_pl_nc = res_prepost_nc,
                     prepost_pl_c = res_prepost_c,
                     N = n_end_users)
  return(list(pm = pm, y = y))
}
