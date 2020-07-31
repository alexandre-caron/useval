
#' @title TO DO sample_pm
#'
#' @description TO DO sample_pm
#'
#' @param joint_pm TO DO
#' @param size TO DO
#'
#' @return
#' @export sample_pm
#'

sample_pm <- function(joint_pm, size) {
  s <- sample(x = nrow(joint_pm), size = size,
              prob = joint_pm[,"p"], replace = TRUE)
  joint_pm[s, c("m_c", "m_nc")]
}


#' Title TO DO
#'
#' @param n TO DO
#' @param mu TO DO
#' @param s2 TO DO
#'
#' @return TO DO
#' @export
#'

rstan_p <- function(nb_sample, n, mu, s2){
  fit = rstan::sampling(object = stanmodels$p, chains = 1,
                        data = list(n=n, mu=mu, s2=s2), refresh = 0,
                        iter = max(nb_sample + 1000, 2000),
                        warmup = 1000)
  logitinv(sample(fit@sim$samples[[1]]$logit_p[1001:max(nb_sample + 1000, 2000)],
                  nb_sample))
}


#' @title TO DO sample_posterior_pl
#'
#' @description TO DO sample_posterior_pl
#'
#' @param nb_sample TO DO
#' @param n TO DO
#' @param mu TO DO
#' @param s2 TO DO
#' @param metropolis TO DO
#'
#' @return
#' @export sample_posterior_pl
#'

sample_posterior_pl <- function(nb_sample, n, mu,s2, sampler = "rstan"){
  switch (sampler,
    armspp = {
      thin = 100
      temp = armspp::arms(thin*nb_sample, log_pdf, lower = 0, upper = 1,
                          arguments = list(n = n, mu  = mu, s2= s2),
                          metropolis = TRUE)
      temp[thin*(1:nb_sample)]
    },
    rstan = {
      rstan_p(nb_sample, n, mu, s2)
    }
  )
}


#' @title TO DO prepost_pl
#'
#' @description TO DO prepost_pl
#'
#' @param m TO DO
#' @param pl TO DO
#' @param n TO DO
#' @param seed TO DO
#'
#' @return
#' @export prepost_pl
#'

prepost_pl <- function(m, pl, mu_s2, n, seed = NULL) {
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
      undiscovered <- lapply(n, function(x) {
        rbinom(length(pl_m_sample),x,pl_m_sample)== 0
    })

    pl_m_post_undiscovered = lapply(undiscovered, function(x) {
        if (sum(x)>0) {
          sample_posterior_pl(nb_sample = sum(x), n = n, mu  = mu_s2_sample[1],
                             s2= mu_s2_sample[2])
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
#' @return
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
#' @return
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
#' @param sim TO DO
#'
#' @return
#' @export prepost_analysis
#'

prepost_analysis <- function(fit_nc, fit_c, sample_size, n_end_users, sim){
  pm <- joint_pm(pd_nc = fit_nc$integrated_likelihood,
                 pd_c = fit_c$integrated_likelihood)
  j_nc <- min(pm[pm$px_nc>-Inf,"m_nc"])
  j_c <- min(pm[pm$px_nc>-Inf,"m_c"])
  pl_nc <- short_pl(posterior_pl = fit_nc$posterior_pl, j = dm_nc$j)
  mu_s2_nc <- fit_nc$posterior_mu_s2
  pl_c <- short_pl(posterior_pl = fit_c$posterior_pl, j = dm_c$j)
  mu_s2_c <- fit_c$posterior_mu_s2
  s_pm <- sample_pm(pm, sim)
  res_prepost_nc <- sapply(1:length(s_pm$m_nc), function(x){
    prepost_pl(m = s_pm$m_nc[x], pl = pl_nc, mu_s2 = mu_s2_nc, n = sample_size)
  })
  res_prepost_c <- sapply(1:length(s_pm$m_c), function(x){
    prepost_pl(m = s_pm$m_c[x], pl = pl_c, mu_s2 = mu_s2_c, n = sample_size)
  })
  # According to the use case replace multinom_sample by multinom_expectation
  y <- multinom_expectation(prepost_pl_nc = res_prepost_nc,
                     prepost_pl_c = res_prepost_c,
                     N = n_end_users)
  return(list(pm = pm, y = y))
}
