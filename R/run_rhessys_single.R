#' Run RHESSys simulation
#'
#' Writes files and assembles inputs to run a single instance of RHESSys.
#' @param input_rhessys List containing the following named elements: "rhessys_version" (path to rhessys binary),
#' "tec_file"(name for tec file to be built), "world_file"(path to existing worldfile), "world_hdr_prefix"(prefix for headers to create),
#' "flow_file"(path to existing flowtable), "start_date"(format c('yyyy mm dd hr')), "end_date"(format c('yyyy mm dd hr')),
#' "output_folder"(path to output folder), "output_filename"(prefix for output files to create), "command_options"(additional commandline options)
#' @param hdr_files List of named elements, named for each def file type (basin_def, hillslope_def, zone_def, soil_def, landuse_def, patch_def,
#' stratum_def) as well as an element named "base_stations". Each element should contain the path to the corresponding def file.
#' @param std_pars List of standard (command line) parameters.
#' @param tec_data Input tec events, see input_tec function
#' @param def_pars To overwrite def file parameters. Format is a list of lists, with each sub-list having the format:
#' list(<path to def/just use the input_hdr_list$yourheader>, <parameter name>, <value>). Defaults to NULL
#' @param clim_base Data for input climate basestation to be written. Defaults to NULL, which assumes you havean existing basestation pointed
#' to in input_rhessys.
#' @param output_method "awk" will use awk, any other non NULL input will use R based output selection
#' @param output_variables List of output variables to subset via the indicated method, Defaults to NULL
#' @param return_data TRUE/FALSE if the function should return a data.table of the selected output - for now only works if doing 1 run
#' @param runID The unique ID used to track input and output files if running multiple scenarios, and thus multiple instances of run_rhessys_core.
#' @export

run_rhessys_single <- function(input_rhessys,
                               hdr_files,
                               tec_data,
                               std_pars = NULL,
                               def_pars = NULL,
                               clim_base = NULL,
                               output_method = NULL,
                               output_variables = NULL,
                               return_data = FALSE,
                               runID = NULL) {

  # ------------------------------ Input checks ------------------------------
  req_rhessys_input = c( "rhessys_version", "tec_file", "world_file", "world_hdr_prefix", "flow_file",
                          "start_date", "end_date", "output_folder", "output_filename", "command_options")
  if (any(!req_rhessys_input %in% names(input_rhessys))) {
    stop(paste0("Missing rhessys_input(s):", names(input_rhessys)[!req_rhessys_input %in% names(input_rhessys)]))
  }
  # Following inputs must exist to run
  if (!file.exists(input_rhessys$rhessys_version)) { stop(paste("RHESSys version", input_rhessys$rhessys_version, "does not exist."))}
  if (!file.exists(input_rhessys$world_file)) { stop(paste("World file", input_rhessys$world_file, "does not exist."))}
  if (!file.exists(input_rhessys$flow_file)) { stop(paste("Flow table", input_rhessys$flow_file, "does not exist."))}
  # auto generate output folder
  if (!dir.exists(input_rhessys$output_folder)) {
    dir.create(input_rhessys$output_folder)
    cat("Created output folder: ", input_rhessys$output_folder)
  }
  # just for safety since this is optional
  if (is.null(input_rhessys$prefix_command)) {
    input_rhessys$prefix_command = NULL
  }

  # ------------------------------ Def file parameters ------------------------------
  # check the def files all exist - except for the fire grid prefix
  if (any(!file.exists(unlist(input_hdr[names(input_hdr) != "fire_grid_prefix"] )))) {
    stop("Def file(s) '", unlist(input_hdr[names(input_hdr) != "fire_grid_prefix"])[!file.exists(unlist(input_hdr[names(input_hdr) != "fire_grid_prefix"]))],"' is/are not exist at specified path." )
  }
  # TODO if keeping the fire grid header method - add check for those files if header is included

  # if there are def file pars to change
  if (!is.null(def_pars)) {
    if (!is.data.frame(def_pars)) {
      def_pars_df = data.frame(matrix(unlist(def_pars), nrow=length(def_pars), byrow=T))
    } else {
      def_pars_df = def_pars
    }

    # check the def files to change are in the list of existing ones - warning only since maybe this is intentional?
    if (any(!unique(def_pars_df$X1) %in% unlist(hdr_files))) {
      warning("Def file '",unique(def_pars_df$X1)[!unique(def_pars_df$X1) %in% unlist(hdr_files)],"' is not included in header def files." )
    }

    def_files = data.frame(old = unique(def_pars_df$X1), new = NA)
    for (f in unique(def_pars_df$X1)) {
      # subset def file pars and put in format expected by the change_def_file function
      def_par_subset = data.frame(t(def_pars_df[def_pars_df$X1 == f,3]))
      names(def_par_subset) = def_pars_df[def_pars_df$X1 == f,2]
      new_file = change_def_file(def_file = f, par_sets = def_par_subset, file_name_ext = runID)
      def_files[def_files[,1] == f, 2] = new_file
    }
    cat("\n===== Wrote def files =====")
  }

  # ------------------------------ Standard parameters ------------------------------
  if (!is.null(std_pars)) {
    # value of 1 == null as far as RHESSys is concerned, setting all NULLs to 1 to be safe, then ignoring if all inputs are 1s
    # std_pars = lapply(std_pars, function(X) {if (!is.null(X) && X == 1) X = NULL; return(X)})
    std_pars = lapply(std_pars, function(X) {if (is.null(X)) X = 1; return(X)})

    if (std_pars[["m"]] != 1 | std_pars[["k"]] != 1) {
      std_pars_out = paste("-s", std_pars$m, std_pars$k)
      if (std_pars[["soil_dep"]] != 1) {
        std_pars_out = paste(std_pars_out, std_pars$soil_dep)
      }
    } else {
      std_pars_out = NULL
    }
    if (std_pars[["m_v"]] != 1 | std_pars[["k_v"]] != 1) {
      std_pars_out = paste(std_pars_out, "-sv", std_pars$m_v, std_pars$k_v)
    }
    if (std_pars[["pa"]] != 1 | std_pars[["po"]] != 1) {
      std_pars_out = paste(std_pars_out, "-svalt", std_pars$pa, std_pars$po)
    }
    if (std_pars[["gw1"]] != 1 | std_pars[["gw2"]] != 1) {
      std_pars_out = paste(std_pars_out, "-gw", std_pars$gw1, std_pars$gw2)
    }
    if (std_pars[["vgseng1"]] != 1 | std_pars[["vgseng2"]] != 1 | std_pars[["vgseng3"]] != 1) {
      std_pars_out = paste(std_pars_out, "-vgsen", std_pars$vgseng1, std_pars$vgseng2, std_pars$vgseng3)
    }

  }

  # ------------------------------ Climate ------------------------------
  # TODO add climate and dated seqeunce functionality in here
  # if (!is.null(clim)) {
  #
  # }

  # ------------------------------ Header file ------------------------------
  # TODO add check for single path to existing hdr file
  world_hdr_name_out = make_hdr_file2(input_rhessys, hdr_files, def_files)

  # ------------------------------ Temporal event control (tec) file ------------------------------
  if (!is.null(tec_data)) {
    write.table(tec_data, file = input_rhessys$tec_file, col.names = FALSE, row.names = FALSE, quote = FALSE)
    cat("\n===== Wrote tec file =====\n")
  }

  # ------------------------------ Call RHESSys ------------------------------
  output_path = file.path(input_rhessys$output_folder, input_rhessys$output_filename)

  rhessys_command(rhessys_version = input_rhessys$rhessys_version,
                  world_file = input_rhessys$world_file,
                  world_hdr_file = world_hdr_name_out,
                  tec_file = input_rhessys$tec_file,
                  flow_file = input_rhessys$flow_file,
                  start_date = input_rhessys$start_date,
                  end_date = input_rhessys$end_date,
                  output_file = output_path,
                  input_parameters = std_pars_out,
                  command_options = input_rhessys$command_options,
                  prefix_command = input_rhessys$prefix_command)

  # ------------------------------ Process Output ------------------------------
  data_out = output_control(output_method,
                            output_variables,
                            return_data)

  if (return_data) {
    return(data_out)
  }
  return()

}
