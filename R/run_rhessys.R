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
                       m, k, m_v, k_v, pa, po, gw1, gw2, parameter_sub_list = NULL,
                       output_variables = NULL, ...){

  # Process parameters
  parameter_type <- match.arg(parameter_type)
  parameter_sets <- get_parameter_sets(m, k, m_v, k_v, pa, po, gw1, gw2,
                                       parameter_sub_list = parameter_sub_list,
                                       method = parameter_type)

  for (aa in seq_len(parameter_sets_l)){
    print(paste("-------------- Run", aa ,"of", parameter_sets_l, "--------------"))

    # Call awk script to substitute non-standard parameters (and variables)
    if (is.null(parameter_sub_list[1]) == F){
      for (bb in seq_along(parameter_sub_list)){
        substitute_parameters(par_value = parameter_sets[aa, 8+bb],
                              awk_file = parameter_sub_list[[bb]][[2]],
                              input_file = parameter_sub_list[[bb]][[3]],
                              output_folder = parameter_sub_list[[bb]][[4]])
      }
    }

    # Call RHESSys
    rhessys_command(rhessys_version = rhessys_version, tec_file = tec_file,
                world_file = world_file, world_hdr_file = world_hdr_file,
                flow_file = flow_file, start_date = start_date,
                end_date = end_date, output_folder = output_folder,
                output_filename = output_filename, command_options = command_options,
                m = m, k = k, m_v = m_v, k_v = k_v, pa = pa, po = po, gw1 = gw1, gw2 = gw2)

    # Process RHESSys output
    if (is.null(output_variables[1])==F){
      setwd(paste(output_folder, "/allsim", sep=""))
      select_output_variables(output_variables = output_variables)
      setwd("../../../")
    }
  }
}

