#' Run RHESSys simulation
#'
#' Writes files and assembles inputs to run a single instance of RHESSys.
#' @param input_rhessys List containing the following named elements: "rhessys_version" (path to rhessys binary),
#' "tec_file"(name for tec file to be built), "world_file"(path to existing worldfile), "world_hdr_prefix"(prefix for headers to create),
#' "flow_file"(path to existing flowtable), "start_date"(format c('yyyy mm dd hr')), "end_date"(format c('yyyy mm dd hr')),
#' "output_folder"(path to output folder), "output_filename"(prefix for output files to create), "command_options"(additional commandline options)
#' @param hdr_files List of named elements, named for each def file type (basin_def, hillslope_def, zone_def, soil_def, landuse_def, patch_def,
#' stratum_def) as well as an element named "base_stations". Each element should contain the path to the corresponding def file.
#' @param cmd_pars List of standard (command line) parameters.
#' @param tec_data Input tec events, see input_tec function
#' @param def_pars To overwrite def file parameters. Format is a list of lists, with each sub-list having the format:
#' list(<path to def/just use the input_hdr_list$yourheader>, <parameter name>, <value>). Defaults to NULL
#' @param clim_base Data for input climate basestation to be written. Defaults to NULL, which assumes you havean existing basestation pointed
#' to in input_rhessys.
#' @param output_filter An output filter, either an R list with 1 to n number of filters read in/modified/generated via IOin_output_filter.R
#' (or associated functions - build_output_filter.R, read_output_filter.R, modify_output_filter.R), or a file path pointing to an
#' existing output filter.
#' @param par_option TRUE/FALSE if the -par command line options should be used if running multiple runs and outputting current worldfile state.
#' Can also just set as a number to set specific par ID.
#' @param return_cmd TRUE/FALSE if run_rhessys_single should return the command line call INSTEAD of running.
# @param write_cmd Path to write the rhessys cmd call to. Will be appended with run # if needed.
#' @param write_run_metadata TRUE/FALSE if a text file containing run metadata should be written to the same location as your output.
#' @param runID The unique ID used to track input and output files if running multiple scenarios, and thus multiple instances of run_rhessys_core.
#' @export

run_rhessys_single <- function(input_rhessys,
                               hdr_files,
                               tec_data = NULL,
                               def_pars = NULL,
                               clim_base = NULL,
                               output_filter = NULL,
                               cmd_pars = NULL,
                               par_option = TRUE,
                               return_cmd = FALSE,
                               write_run_metadata = FALSE,
                               runID = NULL) {

  cat("\n--------------------------------------------\n")
  cat("===== Beginning RHESSysIO file writing =====\n")
  cat("--------------------------------------------\n\n")
  # ------------------------------ Input checks ------------------------------
  req_rhessys_input = c( "rhessys_version", "tec_file", "world_file", "world_hdr_prefix", "flow_file",
                          "start_date", "end_date")
  if (any(!req_rhessys_input %in% names(input_rhessys))) {
    stop(paste0("Missing rhessys_input(s):", names(input_rhessys)[!req_rhessys_input %in% names(input_rhessys)]))
  }
  # Following inputs must exist to run
  if (!file.exists(input_rhessys$rhessys_version)) { stop(paste("RHESSys version", input_rhessys$rhessys_version, "does not exist."))}
  if (!file.exists(input_rhessys$world_file)) { stop(paste("World file", input_rhessys$world_file, "does not exist."))}
  if (!file.exists(input_rhessys$flow_file)) { stop(paste("Flow table", input_rhessys$flow_file, "does not exist."))}
  # auto generate output folder
  if (!is.null(input_rhessys$output_folder) && !dir.exists(input_rhessys$output_folder)) {
    dir.create(input_rhessys$output_folder)
    cat("Created output folder: ", input_rhessys$output_folder)
  }

  # ------------------------------ Def file parameters ------------------------------
  # if input hdr files are a data object
  if (!is.character(hdr_files)) {
    # check the def files all exist - except for the fire grid prefix, and clim if clim is also given as input
    not_check = 'fire_grid_prefix'
    if (!is.null(clim_base)) {not_check = c(not_check, 'base_stations')}

    if (any(!file.exists(unlist(hdr_files[!names(hdr_files) %in% not_check])))) {
      stop("Def file(s) '",
           unlist(hdr_files[!names(hdr_files) %in% not_check])[!file.exists(unlist(hdr_files[!names(hdr_files) %in% not_check]))],
           "' is/are not exist at specified path.")
    }
    # TODO if keeping the fire grid header method - add check for those files if header is included
  } else {
    if (!file.exists(hdr_files)) {
      stop("Existing hdr file was not found at path:", hdr_files)
    }
  }

  # if there are def file pars to change
  if (!is.null(def_pars)) {
    if (is.character(hdr_files)) {
      stop("Cannot use an existing unmodified hdr file to reference modified def files - please use the IOin_hdr() function to input hdr file data in R.")
    }

    if (!is.data.frame(def_pars)) {
      def_pars_df = data.frame(matrix(unlist(def_pars), nrow = length(def_pars), byrow = T))
    } else {
      def_pars_df = def_pars
    }

    # check the def files to change are in the list of existing ones - warning only since maybe this is intentional?
    if (any(!unique(def_pars_df$X1) %in% unlist(hdr_files))) {
      stop("Def file '",unique(def_pars_df$X1)[!unique(def_pars_df$X1) %in% unlist(hdr_files)],"' being modified is not included in the input header list of def files (input_hdr)." )
    }

    def_files = data.frame(old = unique(def_pars_df$X1), new = NA)
    for (f in unique(def_pars_df$X1)) {
      # subset def file pars and put in format expected by the change_def_file function
      def_par_subset = data.frame(t(def_pars_df[def_pars_df$X1 == f,3]))
      names(def_par_subset) = def_pars_df[def_pars_df$X1 == f,2]
      new_file = change_def_file(def_file = f, par_sets = def_par_subset, file_name_ext = runID)
      def_files[def_files[,1] == f, 2] = new_file
    }
    cat("===== Wrote def files =====\n")
  } else {
    def_files = NULL
  }

  # ------------------------------ Standard parameters ------------------------------
  if (!is.null(cmd_pars)) {
    # value of 1 == null as far as RHESSys is concerned, setting all NULLs to 1 to be safe, then ignoring if all inputs are 1s
    # cmd_pars = lapply(cmd_pars, function(X) {if (!is.null(X) && X == 1) X = NULL; return(X)})
    cmd_pars = lapply(cmd_pars, function(X) {if (is.null(X)) X = 1; return(X)})

    if (cmd_pars[["m"]] != 1 | cmd_pars[["k"]] != 1) {
      cmd_pars_out = paste("-s", cmd_pars$m, cmd_pars$k)
      if (cmd_pars[["soil_dep"]] != 1) {
        cmd_pars_out = paste(cmd_pars_out, cmd_pars$soil_dep)
      }
    } else {
      cmd_pars_out = NULL
    }
    if (cmd_pars[["m_v"]] != 1 | cmd_pars[["k_v"]] != 1) {
      cmd_pars_out = paste(cmd_pars_out, "-sv", cmd_pars$m_v, cmd_pars$k_v)
    }
    if (cmd_pars[["pa"]] != 1 | cmd_pars[["po"]] != 1) {
      cmd_pars_out = paste(cmd_pars_out, "-svalt", cmd_pars$pa, cmd_pars$po)
    }
    if (cmd_pars[["gw1"]] != 1 | cmd_pars[["gw2"]] != 1) {
      cmd_pars_out = paste(cmd_pars_out, "-gw", cmd_pars$gw1, cmd_pars$gw2)
    }
    if (cmd_pars[["vgseng1"]] != 1 | cmd_pars[["vgseng2"]] != 1 | cmd_pars[["vgseng3"]] != 1) {
      cmd_pars_out = paste(cmd_pars_out, "-vgsen", cmd_pars$vgseng1, cmd_pars$vgseng2, cmd_pars$vgseng3)
    }
  } else {
    cmd_pars_out = NULL
  }

  # ------------------------------ Climate ------------------------------
  # TODO potentially add dated sequence functionality of some sort here
  if (!is.null(clim_base)) {
    write.table(clim_base, file = hdr_files$base_stations, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "       ")
    cat("===== Wrote clim base station file =====\n")
  }

  # ------------------------------ Header file ------------------------------
  # TODO add check for single path to existing hdr file
  if (is.list(hdr_files)) {
    world_hdr_name_out = make_hdr_file(input_rhessys, hdr_files, def_files, runID)
  } else if (is.character(hdr_files)) {
    world_hdr_name_out = hdr_files
  }


  # ------------------------------ Temporal event control (tec) file ------------------------------
  if (!is.null(tec_data) && is.data.frame(tec_data)) {
    write.table(tec_data, file = input_rhessys$tec_file, col.names = FALSE, row.names = FALSE, quote = FALSE)
    cat("===== Wrote tec file =====\n")
  } else if (!is.null(tec_data)) {
    if (!file.exists(tec_data)) {
      stop("Existing tec file was not found at path:", tec_data)
    }
  } else if (!is.null(input_rhessys$tec_file)) {
    if (!file.exists(input_rhessys$tec_file)) {
      stop("Existing tec file was not found at path:", input_rhessys$tec_file)
    }
  }

  # ------------------------------ Output Filters ------------------------------
  if (!is.null(output_filter)) {
    filter_path = write_output_filter(output_filter, runID)
    output_path = NULL
  } else {
    output_path = file.path(input_rhessys$output_folder, input_rhessys$output_filename)
    if (!is.null(runID)) {
      output_path = paste0(output_path, "_run", runID)
    }
    filter_path = NULL
  }

  # ------------------------------ Par option ------------------------------
  par_option_ID = NULL
  if (is.numeric(par_option)) {
    par_option_ID = par_option
  } else if (par_option & !is.null(runID)) {
    par_option_ID = runID
  }


  # ------------------------------ Write Run Info ------------------------------
  if (write_run_metadata & is.null(runID)) {
    run_info_file = write_run_info(rhessys_version = input_rhessys$rhessys_version,
                                   world_file = input_rhessys$world_file,
                                   world_hdr_file = world_hdr_name_out,
                                   tec_file = input_rhessys$tec_file,
                                   flow_file = input_rhessys$flow_file,
                                   start_date = input_rhessys$start_date,
                                   end_date = input_rhessys$end_date,
                                   output_path = output_path,
                                   input_parameters = cmd_pars_out,
                                   output_filter = filter_path,
                                   command_options = input_rhessys$command_options,
                                   prefix_command = input_rhessys$prefix_command,
                                   return_cmd = return_cmd)
  } else {
    run_info_file = NULL
  }

  cat("\n-------------------------------------------\n")
  cat("===== Finished RHESSysIO file writing =====\n")
  cat("-------------------------------------------\n\n")


  # ------------------------------ Call RHESSys ------------------------------
  rh_cmd = rhessys_command(rhessys_version = input_rhessys$rhessys_version,
                  world_file = input_rhessys$world_file,
                  world_hdr_file = world_hdr_name_out,
                  tec_file = input_rhessys$tec_file,
                  flow_file = input_rhessys$flow_file,
                  start_date = input_rhessys$start_date,
                  end_date = input_rhessys$end_date,
                  output_file = output_path,
                  input_parameters = cmd_pars_out,
                  output_filter = filter_path,
                  par_option_ID = par_option_ID,
                  command_options = input_rhessys$command_options,
                  prefix_command = input_rhessys$prefix_command,
                  return_cmd = return_cmd)

  if (return_cmd) {
    return(rh_cmd)
  }

  if (!is.null(output_path)) {
    cat("\n===== Wrote output to: '",output_path ,"' =====\n", sep = "")
  }

  # check if output files were actually written + have more than header
  # this will mostly just add notes in cases where RHESSys errored at start
  # so that metadata from those runs won't be confused with metadata from successful runs
  if (write_run_metadata & is.null(runID)) {
    # this function is overbuilt but checks if there's more than just a single header/line
    check_rh_output = function(filepath) {
      if (!file.exists(filepath)) {
        return(FALSE)
      } else {
        if (file.size(filepath) < 5000) {
          f <- file(filepath, open="rb")
          chunk <- readBin(f, "raw", 5000)
          close(f)
          nlines <- sum(chunk == as.raw(10L))
          if (nlines > 1) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        } else {
          return(TRUE)
        }
      }
    }
    fex = sapply(run_info_file$output_files, check_rh_output)
    # fex = file.exists(run_info_file$output_files)
    file = file(run_info_file$info_filepath, "a", encoding = "UTF-8")
    cat(paste0("     Successful Output?:\t", paste0(fex, collapse = ", "), "\n"), file = file, sep = "")
    close(file)
  }

  return()

}
