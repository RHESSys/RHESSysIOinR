#' Run RHESSys model simulations
#'
#' Permits the calibration and simulation of the rhessys model. Multiple model
#' runs can be run, either in series or in parallel (such as with cluster computing).
#' @param parameter_method One of the following methods: "all_combinations","lhc","monte_carlo","exact_values"
#' @param input_rhessys List containing the following named elements: "rhessys_version" (path to rhessys binary),
#' "tec_file"(name for tec file to be built), "world_file"(path to existing worldfile), "world_hdr_prefix"(prefix for headers to create),
#' "flow_file"(path to existing flowtable), "start_date"(format c('yyyy mm dd hr')), "end_date"(format c('yyyy mm dd hr')),
#' "output_folder"(path to output folder), "output_filename"(prefix for output files to create), "command_options"(additional commandline options)
#' @param input_hdr_list List of named elements, named for each def file type (basin_def, hillslope_def, zone_def, soil_def, landuse_def, patch_def,
#' stratum_def) as well as an element named "base_stations". Each element should contain the path to the corresponding def file.
#' @param input_preexisting_table Optional means of inputing parameters based on a prexisting setup. Defaults to NULL
#' @param input_def_list To overwrite def file parameters. Format is a list of lists, with each sub-list having the format:
#' list(<path to def/just use the input_hdr_list$yourheader>, <parameter name>, <value>)
#' @param input_standard_par_list List of standard (command line) parameters
#' @param input_clim_base_list List of the climate base station components - also see clim_auto
#' @param input_dated_seq_list List of dated sequences, defaults to NULL
#' @param input_tec_data Input tec events, see input_tec function
#' @param output_variables List of output variables to subset via the indicated method, Defaults to NULL
#' @param output_method "awk" will use awk, any other non NULL input will use R based output selection
#' @param return_data TRUE/FALSE if the function should return a data.table of the selected output - for now only works if doing 1 run
#' @export

run_rhessys <- function(parameter_method,
                        output_method,
                        input_rhessys,
                        input_hdr_list,
                        input_preexisting_table = NULL,
                        input_def_list,
                        input_standard_par_list,
                        input_clim_base_list,
                        input_dated_seq_list = NULL,
                        input_tec_data,
                        output_variables = NULL,
                        output_initiation = 1,
                        return_data = FALSE){

  # ------------------------------ Input checks ------------------------------
  if (!parameter_method %in% c("all_combinations","lhc","monte_carlo","exact_values")) {stop("Invalid parameter_method.")}
  req_rhessys_input = c( "rhessys_version", "tec_file", "world_file", "world_hdr_prefix", "flow_file",
                          "start_date", "end_date", "output_folder", "output_filename", "command_options")
  if (any(!req_rhessys_input %in% names(input_rhessys))) {
    stop(paste0("Missing rhessys_input(s):", names(input_rhessys)[!reqs_rhessys_input %in% names(input_rhessys)]))
  }
  # Following inputs must exist to run
  if (!file.exists(input_rhessys$rhessys_version)) {stop(paste("RHESSys version", input_rhessys$rhessys_version, "does not exist."))}
  if (!file.exists(input_rhessys$world_file)) {stop(paste("World file", nput_rhessys$world_file, "does not exist."))}
  if (!file.exists(input_rhessys$flow_file)) {stop(paste("Flow table", input_rhessys$flow_file, "does not exist."))}
  # auto generate output folder - if people don't like this, add an IF and make it an argument option
  if (!dir.exists(input_rhessys$output_folder)) {
    dir.create(input_rhessys$output_folder)
    cat("Created output folder: ", input_rhessys$output_folder)
  }

  # clim inputs check - needs to be added

  # this will trigger all the time, so maybe not needed
  # if (length(list.files(path = input_rhessys$output_folder, pattern = paste(input_rhessys$output_filename, "*", sep=""))) > 0) {
  #   print(paste("Output files with prefix",input_rhessys$output_filename,"alerady exist in",input_rhessys$output_folder,"and will be overwritten."),quote = FALSE)}

  # ------------------------------ Generate option sets ------------------------------
  option_sets <- generate_option_sets(parameter_method = parameter_method,
                                      input_rhessys = input_rhessys,
                                      input_hdr_list = input_hdr_list,
                                      input_preexisting_table = input_preexisting_table,
                                      input_def_list = input_def_list,
                                      input_standard_par_list = input_standard_par_list,
                                      input_dated_seq_list = input_dated_seq_list)

  # ------------------------------ Generate RHESSys input files ------------------------------
  generate_input_files(input_rhessys = input_rhessys,
                       input_hdr_list = input_hdr_list,
                       input_clim_base_list = input_clim_base_list,
                       input_tec_data = input_tec_data,
                       input_dated_seq_list = input_dated_seq_list,
                       option_sets_all = option_sets$option_sets_all,
                       option_sets_def_par = option_sets$option_sets_def_par,
                       option_sets_par = option_sets$option_sets_par,
                       option_sets_hdr = option_sets$option_sets_hdr,
                       option_sets_dated_seq = option_sets$option_sets_dated_seq)

  option_sets_rhessys <- option_sets$option_sets_rhessys
  option_sets_rhessys_rows <- nrow(option_sets_rhessys)

  # ------------------------------ Iterate Parameter Sets ------------------------------
  for (aa in seq_len(option_sets_rhessys_rows)) {
    print(paste("----------------- Run", aa ,"of", option_sets_rhessys_rows, "-----------------"))

    # ------------------------------ Csll RHESSys ------------------------------
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

    # ------------------------------ Process Output ------------------------------
    if (!is.null(output_variables[1])) {
      # make allsim folder in output location - won't overwrite, warning for if exists is supressed
      dir.create(file.path(input_rhessys$output_folder,"allsim"), showWarnings = FALSE)
      if (output_method == "awk") {
        select_output_variables_w_awk(output_variables = output_variables,
                                      output_folder = input_rhessys$output_folder,
                                      run = aa,
                                      output_initiation = output_initiation)
      } else if (output_method == "r") {
        data_out = select_output_variables_R(output_variables = output_variables,
                                             output_folder = input_rhessys$output_folder,
                                             output_filename = input_rhessys$output_filename,
                                             run = aa,
                                             return_data = return_data)
      } else {
        select_output_variables(output_variables = output_variables,
                                output_folder = input_rhessys$output_folder,
                                run = aa,
                                output_initiation = output_initiation)
      }
    } # end output IF
  } # end parameter iteration FOR

  if (return_data && seq_len(option_sets_rhessys_rows) == 1) {
    return(data_out)
  }
  return()

}
