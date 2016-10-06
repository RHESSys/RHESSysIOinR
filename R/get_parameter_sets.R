#' Create parameter sets
#'
#'
#' Combines vectors of parameter inputs into data frame of parameter sets that
#' \code{run_rhessys()} cycles through.
#'
#' Each input parameter vector should be of equal length. Extra parameters beyond the
#' standard RHESSys calibrated parameters (e.g. def parameters) will need to be
#' accompanied by extra filenames for awk command. See \code{run_rhessys()} inputs.
#'
#' @export
get_parameter_sets <- function(m, k, m_v, k_v, pa, po, gw1, gw2, awk_filenames, method, ...){

  if (method == "GRID"){
    parameters <- expand.grid(m=m, k=k, m_v=m_v, k_v=k_v, pa=pa, po=po, gw1=gw1, gw2=gw2, ...)
  }

  if (method == "MC"){
    # Add flags for when all parameters are not equal length

    parameters <- cbind(m, k, m_v, k_v, pa, po, gw1, gw2, ...)
  }

  return(parameter_sets)
}



