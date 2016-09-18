#' Run RHESSys model simulations
#'
#'

run_rhessys = function(rhessys_version, tec_file, world_file, world_hdr_file,
                       flow_file, start_date, end_date, output_folder,
                       output_filename, command_options, parameter_type,
                       m, k, m_v, k_v, pa, po, gw1, gw2, awk_filenames=NA,
                       output_selection, ...){

  # Processes parameters
  if (parameter_type == "MC"){
    parameters = parameters_mc(m, k, m_v, k_v, pa, po, gw1, gw2, ...)
  } else {
    parameters = parameters_grid(m, k, m_v, k_v, pa, po, gw1, gw2, ...)
  }

  # Need 'for' loop instead of apply function since calls to RHESSys cannot be done simultaneously
  for (aa in seq_along(parameters[,1])){
    print(paste("----------------", aa ,"----------------"))

    # Evaluate each awk
    if (is.na(awk_filenames[1])==F){
      for (bb in seq_along(awk_filenames)){
        awk_command(parameters[aa, 8+bb], awk_file = awk_filenames[[bb]][[1]],
                input_file = awk_filenames[[bb]][[2]], output_folder = awk_filenames[[bb]][[3]])
      }
    }

    rhessys_command(rhessys_version = rhessys_version, tec_file = tec_file,
                world_file = world_file, world_hdr_file = world_hdr_file,
                flow_file = flow_file, start_date = start_date,
                end_date = end_date, output_folder = output_folder,
                output_filename = output_filename, command_options = command_options,
                m = m, k = k, m_v = m_v, k_v = k_v, pa = pa, po = po, gw1 = gw1, gw2 = gw2)

    # Process RHESSys output
    select_results(output_folder=output_folder, output_selection=output_selection)

  }
}

