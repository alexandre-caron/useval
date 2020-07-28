
#' @title TO DO px_to_pd
#'
#' @description TO DO px_to_pd
#'
#' @param px TO DO
#' @param ... TO DO
#'
#' @return
#' @export px_to_pd
#'
#' @examples
#'

px_to_pd <- function(px, ...) {
  if(px[1] == 0) {
    j <- 0
  } else {
    j <- max(which(px == -Inf))
  }
  sapply(1:length(px), function(i) {
    px[i] + lchoose(i-1,j)
  })
}


#' @title TO DO joint_pm
#'
#' @description TO DO joint_pm
#'
#' @param pd_nc TO DO
#' @param pd_c TO DO
#'
#' @return
#' @export joint_pm
#'
#' @examples
#'

joint_pm <- function(pd_nc, pd_c){
  px_nc <- px_to_pd(pd_nc)
  px_c <- px_to_pd(pd_c)
  m_nc <- 0:(length(pd_nc)-1)
  m_c <- 0:(length(pd_c)-1)

  pm <- expand.grid(m_c = m_c, m_nc = m_nc)

  pm$px_nc <- rep(px_nc, each = length(px_c))
  pm$px_c <- rep(px_c, length(px_nc))
  pm$px <- pm$px_nc + pm$px_c
  pm$p <- prop.table(exp(pm$px - max(pm$px)))
  pm
}

