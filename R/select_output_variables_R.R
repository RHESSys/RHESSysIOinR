#' Selects RHESSys output variables
#'
#' Somewhat optimized R-based read and subsetting of RHESSys output
#'
#' @param output_variables Datafrane with named columns: "variables" containing variables matching variables of interest
#' found in the header of rhessys output files, "out_file" points to the files containing the associated variables,
#' this can be either the path to that file, can use the abbreviation for the different output types
#' (patch daily = pd, patch daily grow = pdg etc.) and the files in output_folder will be parsed appropriately,
#' or can use the fully written out space.time suffix, e.g. "patch.daily" or "grow_patch.daily"
#' @param output_folder Folder where rhessys output is located (e.g. 'out')
#' @param output_filename Base file name of standard rhessys output
#' @param run Simulation number. Used to reset files in allsim at beginning of simulation
#' @param max_run Max number of runs to collect output for - at run == max run, output will be pivoted from long to wide,
#' and date and spatial info will be added.
#' @param return_data TRUE/FALSE if the function should return a data.table of the selected output - for now only works if doing 1 run
#' @param out Output location, if not will use default "allsim" within output folder
#' @param no_write TRUE/FALSE to only return data within R, and not write any data. Only works is return_data == TRUE
#' @export

select_output_variables_R <-
  function(output_variables,
           output_folder,
           output_filename,
           run,
           max_run = NULL,
           return_data = FALSE,
           out = NULL,
           no_write = FALSE) {

  # could get rid of this but its the main difference from the other select functions so it's probably worthwhile
  if (is.null(max_run) && run == 1) {
    warning("Argument 'max_run' is missing. Output data will not be converted from a single column to a table.")
  }

  if (run == 1 && !dir.exists(file.path(output_folder, "allsim"))) {
    dir.create(file.path(output_folder, "allsim"))
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
    files_in = file.path(output_folder,
                         paste0(output_filename, "_", output_variables$out_file))
    if (any(!file.exists(files_in)))
      stop(paste0("Cannot find ", files_in[!file.exists(files_in)]))

  } else if (all(output_variables$out_file %in% abrevs)) {
    files_in = file.path(output_folder, paste0(output_filename, "_", sufs[match(output_variables$out_file, abrevs)]))
    if (any(!file.exists(files_in)))
      stop(paste0("Cannot find ", files_in[!file.exists(files_in)]))

  } else {
    # probs should add a check for if file exists etc. - need to double check on time penalty though
    files_in = output_variables$out_file
    if (any(!file.exists(files_in)))
      stop(paste0("Cannot find ", files_in[!file.exists(files_in)]))
  }

  files_out = file.path(output_folder, "allsim", output_variables$variable)

  if (run == 1) {
    z = suppressWarnings(file.remove(files_out))
    cat("\nWriting selection(s) to file(s):\n")
    cat(noquote(paste0("> ", files_out, " \n")))
  }

  # read files
  files_read = unique(files_in)
  file_index = match(files_in, files_read)
  read_data = lapply(files_read, data.table::fread)

  # subset the output vars - no error checking rn, should add something
  data_subset = lapply(seq_along(file_index), function(X)
    data.table:::subset.data.table(read_data[[file_index[X]]], select = output_variables$variable[X]))

  if (no_write && !return_data) {
    warning("'no_write' argument requires 'return_data == TRUE', no data written or returned")
    return()
  }
  if (return_data && no_write) {
    return(data_subset)
  }

  if (run == 1) {
    data_subset = lapply(seq_along(files_out), function(X) {
      colnames(data_subset[[X]]) = paste0(output_variables$variable[X], "_", run)
      data_subset[[X]]
    })
    z = lapply(seq_along(files_out), function(X)
      data.table::fwrite(data_subset[[X]], files_out[X]))

  } else {
    # appends in a single col, optionally collects and renames at end (see below)
    data_append = function(X) {
      data.table::fwrite(data_subset[[X]], files_out[X], append = TRUE)
      return()
    }
    z = lapply(seq_along(files_out), data_append)
    cat("\nAppended selection(s)\n")
  }

  # only at max run
  if (!is.null(max_run) && run == max_run) {
    # loop for now for simplicity - lapply gets messy with large output for some reason
    for (i in seq_along(files_out)) {
      read_final = data.table::fread(files_out[i])
      dt_out = data.table::as.data.table(matrix(read_final[[1]], ncol = max_run))
      colnames(dt_out) = paste0("run_", c(1:ncol(dt_out)))

      # get matching columns
      out_vars = colnames(read_data[[file_index[i]]])[colnames(read_data[[file_index[i]]]) %in%
                                                        c(
                                                          "day",
                                                          "month",
                                                          "year",
                                                          "basinID",
                                                          "hillID",
                                                          "zoneID",
                                                          "patchID",
                                                          "stratumID",
                                                          "area"
                                                        )]

      # get date from normal output of respective spatial level, cbind date w output
      date_subset = data.table:::subset.data.table(read_data[[file_index[i]]], select = out_vars)

      if (nrow(date_subset) == nrow(dt_out)) {
        dt_out = cbind(date_subset, dt_out)
      } else {
        cat("\nDates and data had mismatched lengths, not added to table output")
      }

      data.table::fwrite(x = dt_out, files_out[i])
    }
    cat("\nOutput transformed to table(s)\n")
  }

  if (return_data && run == 1) {
    return(data_subset)
  }

  return()

}


