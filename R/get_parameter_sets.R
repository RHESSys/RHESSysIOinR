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
get_parameter_sets <- function(m, k, m_v, k_v, pa, po, gw1, gw2, parameter_sub_list, method, runs, ...){

  # Make a list of supplied parameter values
  if (is.null(parameter_sub_list[1]) == F){
    parameter_values <- c(list(m, k, m_v, k_v, pa, po, gw1, gw2), lapply(parameter_sub_list, function(x) x[[1]]))
  }

  if (method == "all_combinations"){
    parameter_sets <- expand.grid(parameter_values)
  }

  if (method == "monte_carlo"){
    # Pull random values from min/max of range supplied\
    # Need to pass total number of samples to be used.

    # parameter_sets <- cbind(parameter_values)
  }

  if (method == "exact_values"){
    # Add flags for when all parameters are not equal length

    parameter_sets <- as.data.frame(parameter_values)
  }

  return(parameter_sets)
}



