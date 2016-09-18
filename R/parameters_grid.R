#' Create parameter sets from grid search
#'
#'

parameters_grid = function(m, k, m_v, k_v, pa, po, gw1, gw2, ...){
  parameters = expand.grid(m=m, k=k, m_v=m_v, k_v=k_v, pa=pa, po=po, gw1=gw1, gw2=gw2, ...)
  return(parameters)
}
