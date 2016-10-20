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
                       tec_data = NULL, output_variables = NULL, ...){

  # Process parameters
  parameter_type <- match.arg(parameter_type)
  parameter_sets <- get_parameter_sets(m, k, m_v, k_v, pa, po, gw1, gw2,
                                       parameter_change_list = parameter_change_list,
                                       method = parameter_type)
  parameter_sets_l <- length(parameter_sets[,1])
  write.csv(parameter_sets, paste(output_folder, "/", output_filename, "_parameter_sets.csv", sep=""))

  # Need to null out output variables
  for (ee in seq_along(output_variables)){
    tmp = sprintf("rm %s/allsim/%s", output_folder, output_variables[[ee]][[1]])
    print(tmp)
    system(tmp)
    tmp = sprintf("echo > %s/allsim/%s", output_folder, output_variables[[ee]][[1]])
    print(tmp)
  }

  # Write tec file
  if (is.null(tec_data) == FALSE) make_tec_file(tec_file = tec_file, tec_data = tec_data)

  for (aa in seq_len(parameter_sets_l)){
    print(paste("-------------- Run", aa ,"of", parameter_sets_l, "--------------"))

    # Call awk script to substitute non-standard parameters (and variables)
    if (is.null(parameter_change_list[1]) == F){
      for (bb in seq_along(parameter_change_list)){
        change_parameters(par_value = parameter_sets[aa, 8+bb],
                              awk_file = parameter_change_list[[bb]][[2]],
                              input_file = parameter_change_list[[bb]][[3]],
                              output_folder = parameter_change_list[[bb]][[4]])
      }
    }

    # Call RHESSys
    rhessys_command(rhessys_version = rhessys_version, tec_file = tec_file,
                world_file = world_file, world_hdr_file = world_hdr_file,
                flow_file = flow_file, start_date = start_date,
                end_date = end_date, output_folder = output_folder,
                output_filename = output_filename, command_options = command_options,
                m = parameter_sets[aa,1], k = parameter_sets[aa,2],
                m_v = parameter_sets[aa,3], k_v = parameter_sets[aa,4],
                pa = parameter_sets[aa,5], po = parameter_sets[aa,6],
                gw1 = parameter_sets[aa,7], gw2 = parameter_sets[aa,8])

    # Process RHESSys output
    if (is.null(output_variables[1]) == F){
      setwd(paste(output_folder, "/allsim", sep=""))
      select_output_variables(output_variables = output_variables)
      setwd("../../../../")
    }
  }
}

