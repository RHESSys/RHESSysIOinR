#' Run multiple RHESSys model simulations
#'
#' \code{run_rhessys} permits the calibration and simulation of the rhessys
#' model. Multiple model runs can be run, either in series or in parallel (such
#' as with cluster computing).
#'
#'
#'
#' @export
run_rhessys <- function(parameter_method,
                        input_rhessys,
                        input_hdr_list,
                        input_preexisting_table = NULL,
                        input_def_list,
                        input_standard_par_list,
                        input_clim_base_list,
                        input_dated_seq_list = NULL,
                        input_tec_data,
                        output_variables = NULL,
                        output_initiation = 1){

  # ---------------------------------------------------------------------
  # Input checks
  # ***Check that there are either parameters to be computed or a data frame, but not both***

  if(!parameter_method %in% c("all_combinations","lhc","monte_carlo","exact_values")){stop("Invalid parameter_method.")}

  # Check input_rhessys inputs - mostly check if they exist
  if(!file.exists(input_rhessys$rhessys_version)){stop(paste("RHESSys Version",input_rhessys$rhessys_version,"does not exist."))}
  if(!file.exists(input_rhessys$tec_file)){stop(paste("Tec file",input_rhessys$tec_file,"does not exist."))}
  if(!file.exists(input_rhessys$world_file)){stop(paste("World file",input_rhessys$world_file,"does not exist."))}
  if(file.exists(file.path(dirname(input_rhessys$world_file), input_rhessys$world_hdr_prefix))){warning(paste("Header folder",input_rhessys$world_hdr_prefix,"already exists, contents will be overwritten."))}
  if(!file.exists(input_rhessys$flow_file)){stop(paste("Flow table",input_rhessys$flow_file,"does not exist."))}
  if(!dir.exists(input_rhessys$output_folder)){stop(paste("Output folder",input_rhessys$output_folder,"does not exist."))}
  if(length(list.files(path = input_rhessys$output_folder,pattern = paste(input_rhessys$output_filename,"*",sep="")))>0){warning(paste("Files with prefix",input_rhessys$output_filename,"alerady exist in",input_rhessys$output_folder))}

  # check start end date using clim input -- AFTER checking input_hdr_
  # check command line args against list of them?

  # Check input_hdr_list
  if(!is.list(input_hdr_list)){stop("input_hdr_list argument is not a list")}

  #input_hdr_list$basin_def <- c("ws_p300/defs/basin_p300.def")
  #input_hdr_list$hillslope_def <- c("ws_p300/defs/hill_p300.def")
  #input_hdr_list$zone_def <- c("ws_p300/defs/zone_p300.def")
  #input_hdr_list$soil_def <- c("ws_p300/defs/patch_p300.def")
  #input_hdr_list$landuse_def <- c("ws_p300/defs/lu_p300.def")
  #input_hdr_list$stratum_def <- c("ws_p300/defs/veg_p300_conifer.def", "ws_p300/defs/veg_p300_shrub.def")
  #input_hdr_list$base_stations <- c("ws_p300/clim/Grove_lowprov_clim_1942_2453.base")


  # ---------------------------------------------------------------------
  # Generate option sets

  option_sets <- generate_option_sets(parameter_method = parameter_method,
                                      input_rhessys = input_rhessys,
                                      input_hdr_list = input_hdr_list,
                                      input_preexisting_table = input_preexisting_table,
                                      input_def_list = input_def_list,
                                      input_standard_par_list = input_standard_par_list,
                                      input_dated_seq_list = input_dated_seq_list)


  # ---------------------------------------------------------------------
  # Generate RHESSys input files

  generate_input_files(input_rhessys = input_rhessys,
                       input_hdr_list = input_hdr_list,
                       input_clim_base_list = input_clim_base_list,
                       input_tec_data = input_tec_data,
                       option_sets_all = option_sets$option_sets_all,
                       option_sets_def_par = option_sets$option_sets_def_par,
                       option_sets_par = option_sets$option_sets_par,
                       option_sets_hdr = option_sets$option_sets_hdr,
                       option_sets_dated_seq = option_sets$option_sets_dated_seq)

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
      select_output_variables_w_awk(output_variables = output_variables,
                                    output_folder = input_rhessys$output_folder,
                                    run = aa,
                                    output_initiation = output_initiation)
    }
  }
  return()
}


