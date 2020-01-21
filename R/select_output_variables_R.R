#' Selects RHESSys output variables
#'
#' Somewhat optimized R-based read and subsetting of RHESSys output
#'
#' @param output_variables Datafrane with named columns: "variables" containing variables matching variables of interest
#' found in the header of rhessys output files, "out_file" points to the files containing the associated variables,
#' this can be either the path to that file, can use the abbreviation for the different output types (patch daily = pd, patch daily grow = pdg etc.)
#' and the files in output_folder will be parsed appropriately, or can use the fully written out space.time suffix, e.g. "patch.daily" or "grow_patch.daily"
#' @param output_folder Folder where rhessys output is located (e.g. 'out')
#' @param run Simulation number. Used to reset files in allsim at beginning of simulation
#' @param return_data TRUE/FALSE if the function should return a data.table of the selected output - for now only works if doing 1 run
#' @param out Output location, if not will use default "allsim" within output folder
#' @export

select_output_variables_R <- function(output_variables, output_folder, output_filename, run, max_run = NULL, return_data = FALSE, out = NULL, no_write = FALSE) {

  # could get rid of this but its the main difference from the other select functions so it's probably worthwhile
  if (is.null(max_run) && run == 1) {
    warning("Argument 'max_run' is missing. Output data will not be converted from a single column to a table.")
  }

  sufs = c("grow_basin.hourly", "basin.hourly", "grow_hillslope.hourly",
           "hillslope.hourly", "grow_zone.hourly", "zone.hourly", "grow_patch.hourly",
           "patch.hourly", "grow_stratum.hourly", "stratum.hourly", "grow_basin.daily",
           "basin.daily", "grow_hillslope.daily", "hillslope.daily", "grow_zone.daily",
           "zone.daily", "grow_patch.daily", "patch.daily", "grow_stratum.daily",
           "stratum.daily", "grow_basin.monthly", "basin.monthly", "grow_hillslope.monthly",
           "hillslope.monthly", "grow_zone.monthly", "zone.monthly", "grow_patch.monthly",
           "patch.monthly", "grow_stratum.monthly", "stratum.monthly", "grow_basin.yearly",
           "basin.yearly", "grow_hillslope.yearly", "hillslope.yearly",
           "grow_zone.yearly", "zone.yearly", "grow_patch.yearly", "patch.yearly",
           "grow_stratum.yearly", "stratum.yearly", "fire.hourly", "fire.daily",
           "fire.monthly", "fire.yearly")

  abrevs = c("bhg", "bh", "hhg", "hh", "zhg", "zh", "phg", "ph", "chg",
             "ch", "bdg", "bd", "hdg", "hd", "zdg", "zd", "pdg", "pd", "cdg",
             "cd", "bmg", "bm", "hmg", "hm", "zmg", "zm", "pmg", "pm", "cmg",
             "cm", "byg", "by", "hyg", "hy", "zyg", "zy", "pyg", "py", "cyg",
             "cy", "fh", "fd", "fm", "fy")

  if (all(output_variables$out_file %in% sufs)) {
    files_in = file.path(output_folder, paste0(output_filename, "_", output_variables$out_file))
    if (any(!file.exists(files_in))) stop(paste0("Cannot find ", files[!file.exists(files)]))
  } else if (all(output_variables$out_file %in% abrevs)) {
    files_in = file.path(output_folder, paste0(output_filename, "_", sufs[match(output_variables$out_file, abrevs)]))
    if (any(!file.exists(files_in))) stop(paste0("Cannot find ", files[!file.exists(files)]))
  } else {    # probs should add a check for if file exists etc. - need to double check on time penalty though
    files_in = output_variables$out_file
    if (any(!file.exists(files_in))) stop(paste0("Cannot find ", files[!file.exists(files)]))
  }

  files_out = file.path(output_folder,"allsim", output_variables$variable)

  if (run == 1){
    z = suppressWarnings(file.remove(files_out))
    cat(noquote(paste0("> ", files_out," \n")))
    #system(sprintf("echo > %s/allsim/%s", output_folder, output_variables$variable[dd])) # I think this is just to print what is being output?
  }

  files_read = unique(files_in)
  file_index = match(files_in, files_read)
  read_data = lapply(files_read, data.table::fread)

  # no error checking rn, should add something
  data_subset = lapply(seq_along(file_index), function(X) data.table:::subset.data.table(read_data[[file_index[X]]], select = output_variables$variable[X]))

  if (return_data && no_write) {
    return(data_subset)
  }

  if (run == 1) {
    data_subset = lapply(seq_along(files_out), function(X) {colnames(data_subset[[X]]) = paste0(output_variables$variable[X],"_",run); data_subset[[X]]})
    z = lapply(seq_along(files_out), function(X) data.table::fwrite(data_subset[[X]], files_out[X]))

  } else {

    # OLD
    # read_append = lapply(files_out, data.table::fread)
    # data_append = function(X) {
    #   data.table::fwrite(read_append[[X]][, data.table::set(x = read_append[[X]], j = paste0(output_variables$variable[X],"_",run), value = data_subset[[X]])], files_out[X])
    # }
    # z = lapply(seq_along(files_out), data_append)

    # FASTER - appends in a single col, optionally collects and renames at end (see below)
    data_append = function(X) {
      data.table::fwrite(data_subset[[X]], files_out[X], append = TRUE)
      return()
    }
    z = lapply(seq_along(files_out), data_append)
  }

  # this only works if
  if (!is.null(max_run) && run == max_run) {
    read_final = lapply(files_out, data.table::fread)
    final_wide = lapply(seq_along(files_out), function(X) data.table::data.table(matrix(read_final[[X]][[1]], ncol = max_run)))
    final_wide = lapply(seq_along(files_out), function(X) {colnames(final_wide[[X]]) = paste0(output_variables$variable[X],"_", c(1:max_run)); final_wide[[X]]})
    z = lapply(seq_along(files_out), function(X) data.table::fwrite(final_wide[[X]], files_out[X]) )
  }

  if (return_data && run == 1) {
    return(data_subset)
  }
  return()

}


