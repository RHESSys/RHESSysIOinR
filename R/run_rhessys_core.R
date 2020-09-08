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

run_rhessys_core <- function(input_rhessys,
                             hdr_files,
                             std_pars,
                             tec_data,
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

  # ------------------------------ Def file parameters ------------------------------
  # check the def files all exist
  if (any(!file.exists(unlist(hdr_files)))) {
    stop("Def file(s) '", unlist(hdr_files)[!file.exists(unlist(hdr_files))],"' is/are not exist at specified path." )
  }

  # if there are def file pars to change
  if (!is.null(def_pars)) {
    if (!is.data.frame(def_pars)) {
      def_pars_df = data.frame(matrix(unlist(def_pars), nrow=length(def_pars), byrow=T))
    } else {
      def_pars_df = def_pars
    }

    # check the def files to change are in the list of existing ones - warning only since maybe this is intentional
    if (any(!unique(def_pars_df$X1) %in% unlist(hdr_files))) {
      warning("Def file '",unique(def_pars_df$X1)[!unique(def_pars_df$X1) %in% unlist(hdr_files)],"' is not included in header def files." )
    }

    for (f in unique(def_pars_df$X1)) {
      # subset def file pars and put in format expected by the change_def_file function
      def_par_subset = data.frame(t(def_pars_df[def_pars_df$X1 == f,3]))
      names(def_par_subset) = def_pars_df[def_pars_df$X1 == f,2]
      change_def_file(def_file = f, par_sets = def_par_subset)
    }
    cat("\n===== Wrote def files =====")
  }

  # ------------------------------ Standard parameters ------------------------------
  if (!is.null(std_pars)) {

    if (!is.null(std_pars$m) & !is.null(std_pars$k)) {
      std1 = paste("-s", std_pars$m, std_pars$k)
    }
    if (!is.null(std_pars$m_v) & !is.null(std_pars$k_v)) {
      std2 = paste("-sv", std_pars$m_v, std_pars$k_v)
    }
    if (!is.null(std_pars$pa) & !is.null(std_pars$po)) {
      std3 = paste("-svalt", std_pars$pa, std_pars$po)
    }
    if (!is.null(std_pars$gw1) & !is.null(std_pars$gw2)) {
      std4 = paste("-gw", std_pars$gw1, std_pars$gw2)
    }

    std_pars_out = paste(std1, std2, std3, std4)

  }

  # ------------------------------ Header file ------------------------------
  # Create hdr output folder
  world_hdr_path <- file.path(dirname(input_rhessys$world_file), input_rhessys$world_hdr_prefix)
  if (!dir.exists(world_hdr_path)) {dir.create(world_hdr_path)}

  # check the hdr items being used
  hdr_def_opts = c("basin_def", "hillslope_def", "zone_def", "soil_def", "landuse_def", "stratum_def", "fire_def", "spinup_def", "base_stations")
  if (any(!names(hdr_files[!is.null(hdr_files)]) %in% hdr_def_opts)) {
    warning("header definition for ",
            names(hdr_files[!is.null(hdr_files)])[!names(hdr_files[!is.null(hdr_files)]) %in% hdr_def_opts],
            "is invalid and won't be added to header")
  }
  # ignore any that aren't valid, specifically to ignore patch header files which it seems should be soil defs
  hdr_files = hdr_files[names(hdr_files[!is.null(hdr_files)]) %in% hdr_def_opts]

  # get needed info for hdr file format
  hdr_values = lapply(hdr_files, function(x) c(length(x), x))
  hdr_vars = mapply(function(x, y) {
    x[1] =  paste0("num_", y)
    x[2:length(x)] = y
    return(x)
    }, hdr_values, names(hdr_values))

  # make combined df
  hdr_df = data.frame(unlist(hdr_values), unlist(hdr_vars), row.names = NULL)
  # fix the names up
  hdr_df[,2] = gsub("def", "default_file", hdr_df[,2])
  hdr_df[,2] = gsub("base_stations", "base_stations_file", hdr_df[,2])
  hdr_df[,2][startsWith(hdr_df[,2], "num")] = paste0(hdr_df[,2][startsWith(hdr_df[,2], "num")],"s")

  # replace with modified defs where needed
  old_paths = unique(def_pars_df$X1)
  new_paths = file.path(gsub(".def","",old_paths) ,basename(old_paths))
  # this is silly
  # hdrpaths = as.list(data.frame(unname(t(data.frame(old_paths, new_paths)))))
  #ugh this is bad
  for (i in seq_along(old_paths)) {
    hdr_df[,1] = gsub(old_paths[i], new_paths[i], hdr_df[,1])
  }

  world_hdr_name_out <- file.path(world_hdr_path, paste(input_rhessys$world_hdr_prefix,".hdr",sep=""))
  write.table(hdr_df, file = world_hdr_name_out, col.names = FALSE, row.names=FALSE, quote = FALSE, sep="\t\t")
  cat("\n===== Wrote hdr file '",world_hdr_name_out,"' =====", sep = "")

  # NOTE ON WRITE SPEEDS
  # write times w data.table::fwrite is only ~100 microsec faster for the header for example so leaving all the writing as write.table for now

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
                  command_options = input_rhessys$command_options)

  # ------------------------------ Process Output ------------------------------

  data_out = output_control(output_method,
                            output_variables,
                            return_data)

  if (return_data) {
    return(data_out)
  }
  return()

}
