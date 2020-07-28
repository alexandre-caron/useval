
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

prepost_pl <- function(m, pl, n, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sapply(m, function(x){
    pl_m <- pl[[m+1]]
    if(is.null(pl_m)) {
      return(n*0)
    } else {
      pl_m_sample <- pl_m[sample(x = 1:nrow(pl_m), size = 1),]
      undiscovered <- lapply(n, function(x) {
        rbinom(length(pl_m_sample),x,pl_m_sample)== 0
      })
      unlist(lapply(undiscovered, function(x){1-prod(1-(pl_m_sample[x]))}))
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
  pl_c <- short_pl(posterior_pl = fit_c$posterior_pl, j = dm_c$j)
  s_pm <- sample_pm(pm, sim)
    res_prepost_nc <- sapply(1:length(s_pm$m_nc), function(x){
    prepost_pl(m = s_pm$m_nc[x], pl = pl_nc, n = sample_size)
  })
    res_prepost_c <- sapply(1:length(s_pm$m_c), function(x){
    prepost_pl(m = s_pm$m_c[x], pl = pl_c, n = sample_size)
  })
    y <- multinom_sample(prepost_pl_nc = res_prepost_nc,
                     prepost_pl_c = res_prepost_c,
                     N = n_end_users)
  return(list(pm = pm, y = y))
}
