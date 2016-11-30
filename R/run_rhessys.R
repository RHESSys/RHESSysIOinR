#' Run multiple RHESSys model simulations
#'
#' \code{run_rhessys} permits the running of multiple rhessys simulations
#'
#'
#'
#' @export
run_rhessys = function(rhessys_version, tec_file, world_file, world_hdr_file,
                       flow_file, start_date, end_date, output_folder,
                       output_filename, command_options,
                       parameter_type = c("all_combinations", "monte_carlo","exact_values"),
                       m, k, m_v, k_v, pa, po, gw1, gw2, parameter_change_list = NULL,
                       tec_data = NULL, dated_seq_file = NULL, dated_seq_data = NULL,
                       output_variables = NULL, ...){

  # Process parameters
  parameter_type <- match.arg(parameter_type)
  parameter_sets <- get_parameter_sets(rhessys_version = rhessys_version, tec_file = tec_file,
                                       world_file = world_file, world_hdr_file = world_hdr_file,
                                       flow_file = flow_file, start_date = start_date,
                                       end_date = end_date, output_folder = output_folder,
                                       output_filename = output_filename, command_options = command_options,
                                       m = m, k = k, m_v = m_v, k_v = k_v,
                                       pa = pa, po = po, gw1 = gw1, gw2 = gw2,
                                       parameter_change_list = parameter_change_list,
                                       dated_seq_data = dated_seq_data,
                                       method = parameter_type)
  parameter_sets_l <- length(parameter_sets[,1])
  total_variables <- length(parameter_sets)
  write.csv(parameter_sets, paste(output_folder, output_filename, "_parameter_sets.csv", sep=""))

  # Write tec file
  if (is.null(tec_data) == FALSE) make_tec_file(tec_file = tec_file, tec_data = tec_data)


  # Step through each parameter set
  for (aa in seq_len(parameter_sets_l)){
    print(paste("-------------- Run", aa ,"of", parameter_sets_l, "--------------"))

    # Call awk script to substitute non-standard parameters (and variables)
    if (is.null(parameter_change_list[1]) == F){
      parameter_change_list_l = length(parameter_change_list)
      for (bb in seq_along(parameter_change_list)){
        change_parameters(par_value = parameter_sets[aa, total_variables-parameter_change_list_l+bb],
                              awk_file = parameter_change_list[[bb]][[2]],
                              input_file = parameter_change_list[[bb]][[3]],
                              output_folder = parameter_change_list[[bb]][[4]])
      }
    }

    # Write dated sequence file
    # Currently only applies to dated sequences with a single date.
    # make_dated_seq_file can handle multi-dates, but run_rhessys is not set up
    # to feed it multiple dates.
    if (is.null(dated_seq_data) == FALSE) make_dated_seq_file(dated_seq_file = dated_seq_file, dated_seq_data = dated_seq_data[[parameter_sets$dated_seq_data[aa]]])

    # Call RHESSys
    rhessys_command(rhessys_version = parameter_sets$rhessys_version[aa], tec_file = parameter_sets$tec_file[aa],
                world_file = parameter_sets$world_file[aa], world_hdr_file = parameter_sets$world_hdr_file[aa],
                flow_file = parameter_sets$flow_file[aa], start_date = parameter_sets$start_date[aa],
                end_date = parameter_sets$end_date[aa], output_folder = parameter_sets$output_folder[aa],
                output_filename = parameter_sets$output_filename[aa], command_options = parameter_sets$command_options[aa],
                m = parameter_sets$m[aa], k = parameter_sets$k[aa],
                m_v = parameter_sets$m_v[aa], k_v = parameter_sets$k_v[aa],
                pa = parameter_sets$pa[aa], po = parameter_sets$po[aa],
                gw1 = parameter_sets$gw1[aa], gw2 = parameter_sets$gw2[aa])

    # Process RHESSys output
    if (is.null(output_variables[1]) == F){
      if (aa == 1){
        initialize_output_variables(output_variables = output_variables, output_folder = output_folder)
      }
      select_output_variables(output_variables = output_variables, output_folder = output_folder)
    }
  }
}

