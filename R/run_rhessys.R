#' Run multiple RHESSys model simulations
#'
#' \code{run_rhessys} permits the calibration and simulation of the rhessys
#' model. Multiple model runs can be run, either in series or in parallel (such
#' as with cluster computing).
#'
#'
#'
#' @export
run_rhessys <- function(parameter_method = c("all_combinations", "lhc", "monte_carlo", "exact_values"),
                        input_rhessys,
                        input_hdr_list,
                        input_preexisting_table,
                        input_def_list = input_def_list,
                        input_standard_par_list,
                        input_dated_seq_file,
                        input_tec_data,
                        output_variables){

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
                       option_sets_par = option_sets$option_sets_par,
                       option_sets_hdr = option_sets$option_sets_hdr,
                       input_tec_data = input_tec_data,
                       world_hdr_prefix = world_hdr_prefix,
                       world_file = world_file)

  # ---------------------------------------------------------------------

  option_sets_rhessys <- option_sets$option_sets_rhessys
  option_sets_rhessys_rows <- nrow(option_sets_rhessys)

  # Step through each parameter set
  for (aa in seq_len(option_sets_rhessys_rows)){
    print(paste("----------------- Run", aa ,"of", option_sets_rhessys_rows, "-----------------"))

    # Call RHESSys
    rhessys_command(rhessys_version = option_sets_rhessys$rhessys_version[aa],
                    world_file = option_sets_rhessys$world_file[aa],
                    world_hdr_file = option_sets_rhessys$world_hdr_file[aa],
                    tec_file = option_sets_rhessys$tec_file[aa],
                    flow_file = option_sets_rhessys$flow_file[aa],
                    start_date = option_sets_rhessys$start_date[aa],
                    end_date = option_sets_rhessys$end_date[aa],
                    output_file = option_sets_rhessys$output_file[aa],
                    input_parameters = option_sets_rhessys$input_parameters[aa],
                    command_options = option_sets_rhessys$command_options[aa])


    # Process RHESSys output
    if (is.null(output_variables[1]) == F){
      if (aa == 1){
        initialize_output_variables(output_variables = output_variables, output_folder = input_rhessys$output_folder)
      }
      select_output_variables(output_variables = output_variables, output_folder = input_rhessys$output_folder)
    }
  }
}


