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
get_parameter_sets <- function(rhessys_version, tec_file, world_file, world_hdr_file,
                               flow_file, start_date, end_date, output_folder,
                               output_filename, command_options,
                               m, k, m_v, k_v, pa, po, gw1, gw2,
                               parameter_change_list=NULL, dated_seq_data=NULL,
                               method, runs, ...){

  # This should be reconfigured so that every input into run_rhessys is treated as a variable.
  # This would permit every input to be looped over and would provide a definitive record of all
  # inputs into each model run. RHESSys_command could then be sourced from this output data frame.

  # The previous objective is made challenging since variables like make_dated_seq and can both
  # multiple inputs (equivavlent to different parameter values) or a single input with muliple
  # dated-sequences.

  # For non-parameters, basically just need to prepopulate a data-frame with all possibilities,
  # with a signifier of row number for non-parameters.

#   rhessys_version_seq <- seq_along(rhessys_version)
#   tec_file_seq <- seq_along(tec_file)
#   world_file_seq <- seq_along(world_file)
#   world_hdr_file_seq <- seq_along(world_hdr_file)
#   flow_file_seq <- seq_along(flow_file)
#   start_date_seq <- seq_along(start_date)
#   end_date_seq <- seq_along(end_date)
#   output_folder_seq <- seq_along(output_folder)
#   output_filename_seq <- seq_along(output_filename)
#   command_options_seq <- seq_along(command_options)

  # Make a list of supplied parameter values
#   if (is.null(parameter_change_list[1]) == F & is.null(dated_seq_data[1,]) == F){
#     print(apply(dated_seq_data, 1, function(x) x[5]))
#     parameter_values <- c(list(m, k, m_v, k_v, pa, po, gw1, gw2), lapply(parameter_change_list, function(x) x[[1]]), lapply(dated_seq_data, function(x) x[[5]]))
#   } else if (is.null(parameter_change_list[1]) == F){
#     parameter_values <- c(list(m, k, m_v, k_v, pa, po, gw1, gw2), lapply(parameter_change_list, function(x) x[[1]]))
#   } else if (is.null(dated_seq_data[1]) == F){
#     parameter_values <- c(list(m, k, m_v, k_v, pa, po, gw1, gw2), apply(dated_seq_data, 1, function(x) x[5]))
#   } else {
#     parameter_values <- list(m, k, m_v, k_v, pa, po, gw1, gw2)
#   }

  #delete:   list(as.vector(apply(dated_seq_data, 1, function(x) x[5])))


  input_values <- list(rhessys_version = rhessys_version,
                             tec_file = tec_file,
                             world_file = world_file,
                             world_hdr_file = world_hdr_file,
                             flow_file = flow_file,
                             start_date = start_date,
                             end_date = end_date,
                             output_folder = output_folder,
                             output_filename = output_filename,
                             command_options = command_options)

  parameter_values <- list(m = m, k = k, m_v = m_v, k_v = k_v,
                           pa = pa, po = po, gw1 = gw1, gw2 = gw2)

  if (is.null(dated_seq_data[1]) == F){
    dated_seq_seq = list(dated_seq_data = seq_along(dated_seq_data))
  } else {
    dated_seq_seq = NULL
  }

  if (is.null(parameter_change_list[1]) == F){
    names(parameter_change_list) <- lapply(parameter_change_list,function(x) x[[2]])
    parameter_change_values <- sapply(parameter_change_list, function(x) x[[1]],simplify = FALSE,USE.NAMES = TRUE)
  } else {
    parameter_change_values = NULL
  }



  if (method == "all_combinations"){

    parameter_sets <- expand.grid(c(input_values, dated_seq_seq, parameter_values, parameter_change_values))
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



