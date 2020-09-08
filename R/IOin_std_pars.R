#' IOin_std_pars
#'
#' Input standard soil parameters for RHESSys
#' @param m
#' @param k
#' @param m_v
#' @param k_v
#' @param pa
#' @param po
#' @param gw1
#' @param gw2
#'
#' @author Will Burke
#'
#' @export
#'

#  FORMAT -  this is pretty constrained, so a named vector works well here


IOin_std_pars = function(m,
                         k,
                         m_v,
                         k_v,
                         pa,
                         po,
                         gw1,
                         gw2) {


  # TODO check here for real world ranges of the parameters, warn if out of bounds

  std_pars = c(m = m, k = k, m_v = m_v, k_v = k_v, pa = pa, po = po, gw1 = gw1, gw2 = gw2)

  return(std_pars)

}
