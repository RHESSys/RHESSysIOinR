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
get_parameter_sets <- function(m, k, m_v, k_v, pa, po, gw1, gw2, parameter_change_list=NULL, dated_seq_data=NULL, method, runs, ...){

  # This should be reconfigured so that every input into run_rhessys is treated as a variable.
  # This would permit every input to be looped over and would provide a definitive record of all
  # inputs into each model run. RHESSys_command could then be sourced from this output data frame.

  # The previous objective is made challenging since variables like make_dated_seq and can both
  # multiple inputs (equivavlent to different parameter values) or a single input with muliple
  # dated-sequences.

  # For non-parameters, basically just need to prepopulate a data-frame with all possibilities,
  # with a signifier of row number for non-parameters.


  # Make a list of supplied parameter values
  if (is.null(parameter_change_list[1]) == F & is.null(dated_seq_data[1,]) == F){
    print(apply(dated_seq_data, 1, function(x) x[5]))
    parameter_values <- c(list(m, k, m_v, k_v, pa, po, gw1, gw2), lapply(parameter_change_list, function(x) x[[1]]), lapply(dated_seq_data, function(x) x[[5]]))
  } else if (is.null(parameter_change_list[1]) == F){
    parameter_values <- c(list(m, k, m_v, k_v, pa, po, gw1, gw2), lapply(parameter_change_list, function(x) x[[1]]))
  } else if (is.null(dated_seq_data[1]) == F){
    parameter_values <- c(list(m, k, m_v, k_v, pa, po, gw1, gw2), apply(dated_seq_data, 1, function(x) x[5]))
  } else {
    parameter_values <- list(m, k, m_v, k_v, pa, po, gw1, gw2)
  }

  #delete:   list(as.vector(apply(dated_seq_data, 1, function(x) x[5])))

  if (method == "all_combinations"){
    parameter_sets <- expand.grid(parameter_values)
  }

  if (method == "monte_carlo"){
    # Pull random values from min/max of range supplied\
    # Need to pass total number of samples to be used.
    print("Currently not implemented")
    # parameter_sets <- cbind(parameter_values)
  }

  if (method == "exact_values"){
    # Add flags for when all parameters are not equal length

    parameter_sets <- as.data.frame(parameter_values)
  }

  return(parameter_sets)
}



