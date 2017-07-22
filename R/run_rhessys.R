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
  # ***Check that there are either parameters to be computed or a data frame, but not both***

  parameter_method <- match.arg(parameter_method)

  # ---------------------------------------------------------------------
  # Generate option sets

  option_sets <- generate_option_sets(parameter_method=parameter_method,
                                      input_rhessys=input_rhessys,
                                      input_hdr_list=input_hdr_list,
                                      input_preexisting_table=input_preexisting_table,
                                      input_def_list=input_def_list,
                                      input_standard_par_list=input_standard_par_list,
                                      input_dated_seq_file=input_dated_seq_file)


  # ---------------------------------------------------------------------
  # Generate RHESSys input files

  generate_input_files(input_rhessys = input_rhessys,
                       input_hdr_list = input_hdr_list,
                       option_sets_def_par = option_sets$option_sets_def_par,
                       option_sets_hdr = option_sets$option_sets_hdr,
                       input_tec_data = input_tec_data,
                       world_hdr_prefix = world_hdr_prefix,
                       world_file = world_file)

  # ---------------------------------------------------------------------

  option_sets_all <- option_sets$option_sets_all

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


