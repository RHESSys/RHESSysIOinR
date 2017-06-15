#' Run multiple RHESSys model simulations
#'
#' \code{run_rhessys} permits the running of multiple rhessys simulations
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


  # ---------------------------------------------------------------------
  # Process parameters



  # Generate hdr file and def files
  process_hdr_def_file(hdr_input_list=hdr_input_list, def_par_input_list=def_par_input_list, dated_seq_input_list=dated_seq_input_list, parameter_method=parameter_method, world_hdr_prefix=world_hdr_prefix, world_file=world_file)



  # Generate def files and associated hdr file for all unique parameter sets
  # Add 'group number' to all-options list
  # group_cols <- unlist(sapply(parameter_change_list,function(x) x[3]))
  # dots <- lapply(group_cols, as.symbol) # https://stackoverflow.com/questions/21208801/
  # parameter_sets$group_num = parameter_sets %>%
  #   group_indices_(.dots=dots) # https://stackoverflow.com/questions/23026145/
  #
  # # Produce data-frame with identifier for unique def and hdr sets.
  # group_cols2 <- c(group_cols, "group_num")
  # dots <- lapply(group_cols2, as.symbol)
  # parameter_sets_groups <- parameter_sets %>%
  #   dplyr::select_(.dots=dots) %>%
  #   group_by(group_num) %>%
  #   dplyr::filter(row_number()==1) # https://stackoverflow.com/questions/22959635/
  #
  # # Generate def files
  # parameter_change_list_l = length(parameter_change_list)
  # change_parameters(par_value = parameter_sets[aa, total_variables-parameter_change_list_l+bb],
  #                   awk_file = parameter_change_list[[bb]][[2]],
  #                   input_file = parameter_change_list[[bb]][[3]],
  #                   output_folder = parameter_change_list[[bb]][[4]])
  #
  #
  # parameter_sets[group_cols]


  # -----
  # Process core parameters
  core_par <- process_core_parameters(core_par_list = core_par_list, parameter_method = parameter_method)



  # ---------------------------------------------------------------------
  # Generate all-options table

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


  # Export parameter sets
  parameter_output <- parameter_sets
  if (is.null(dated_seq_data) == FALSE){
    parameter_output$dated_seq_data <- sapply(parameter_sets$dated_seq_data, function(x) dated_seq_data[[x]][[5]])
  }
  write.csv(parameter_output, paste(output_folder, output_filename, "_parameter_sets.csv", sep=""), row.names = FALSE)




  # ---------------------------------------------------------------------
  # Write tec file

  if (is.null(tec_data) == FALSE) make_tec_file(tec_file = tec_file, tec_data = tec_data)


  # ---------------------------------------------------------------------

  # Step through each parameter set
  for (aa in seq_len(parameter_sets_l)){
    print(paste("-------------- Run", aa ,"of", parameter_sets_l, "--------------"))

    # Call awk script to substitute non-standard parameters (and variables)
    if (is.null(parameter_change_list[1]) == F){
      parameter_change_list_l = length(parameter_change_list)
      for (bb in seq_along(parameter_change_list)){
#        change_parameters(par_value = parameter_sets[aa, total_variables-parameter_change_list_l+bb],
#                              awk_file = parameter_change_list[[bb]][[2]],
#                              input_file = parameter_change_list[[bb]][[3]],
#                              output_folder = parameter_change_list[[bb]][[4]])
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

