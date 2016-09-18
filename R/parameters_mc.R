#' Combines Monte Carlo parameter set
#'
#' Combines vectors of parameter inputs into data frame of parameter sets that
#' \code{run_rhessys()} cycles through.
#'
#' Each input parameter vector should be of equal length. Extra parameters beyond the
#' standard RHESSys calibrated parameters (e.g. def parameters) will need to be
#' accompanied by extra filenames for awk command. See \code{run_rhessys()} inputs.
#'


parameters_mc = function(m, k, m_v, k_v, pa, po, gw1, gw2, ...){
  parameters = cbind(m, k, m_v, k_v, pa, po, gw1, gw2, ...)
  return(parameters)
}

