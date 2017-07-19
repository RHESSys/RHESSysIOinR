#' Run multiple RHESSys model simulations
#'
#' \code{run_rhessys} permits the calibration and simulation of the rhessys
#' model. Multiple model runs can be run, either in series or in parallel (such
#' as with cluster computing).
#'
#'
#'
#' @export
run_rhessys <- function(rhessys_version, tec_file, world_file, world_hdr_prefix,
                       flow_file, start_date, end_date, output_folder,
                       output_filename, command_options,
                       parameter_method = c("all_combinations", "monte_carlo", "exact_values"),
                       m, k, m_v, k_v, pa, po, gw1, gw2, parameter_change_list = NULL,
                       tec_data = NULL, dated_seq_file = NULL, dated_seq_data = NULL,
                       output_variables = NULL, ...){

  # ---------------------------------------------------------------------
  # Input checks

  parameter_method <- match.arg(parameter_method)

  # Check that there are either parameters to be computed or a data frame, but not both.


  # ---------------------------------------------------------------------
  # Generate option sets

  option_sets <- generate_option_sets(parameter_method=parameter_method,
                                      input_rhessys=input_rhessys,
                                      input_preexisting_table=input_preexisting_table,
                                      input_def_list=input_def_list,
                                      input_standard_par_list=input_standard_par_list,
                                      input_dated_seq_file=input_dated_seq_file,
                                      input_tec_data=input_tec_data)

  # Disaggregate output
  option_sets$option_sets_def_par


  # ---------------------------------------------------------------------
  # Generate RHESSys input files


  generate_input_files(hdr_input_list=hdr_input_list,
                       option_sets_def = option_sets$option_sets_def_par,
                       def_par_input_list=def_par_input_list,
                       dated_seq_input_list=dated_seq_input_list,
                       parameter_method=parameter_method,
                       world_hdr_prefix=world_hdr_prefix,
                       world_file=world_file)



  # ---------------------------------------------------------------------
  # Generate all-option table

  option_sets_all <- generate_all_option_sets(parameter_method,
                                              input_rhessys,
                                              option_sets_def_par,
                                              option_sets_standard_par,
                                              option_sets_dated_seq)

  parameter_sets_l <- length(parameter_sets[,1])
  total_variables <- length(parameter_sets)



  # ---------------------------------------------------------------------
  # Write tec file (possibly move to generate_input_files)

  if (is.null(input_tec_data) == FALSE) make_tec_file(tec_file = tec_file, tec_data = input_tec_data)


  # ---------------------------------------------------------------------

  # Step through each parameter set
  for (aa in seq_len(parameter_sets_l)){
    print(paste("-------------- Run", aa ,"of", parameter_sets_l, "--------------"))

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





# ---------------------

run_rhessys <- function(option_sets_df){

  for (aa in seq_len(parameter_sets_l)){
    print(paste("-------------- Run", aa ,"of", parameter_sets_l, "--------------"))

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
