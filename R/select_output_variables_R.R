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
#' @export

select_output_variables_R <- function(output_variables, output_folder, output_filename, run, output_initiation, return_data = FALSE) {

  # maybe set this in a first run IF
  # to make this as fast as possible, copied output (using dput), idk how much a difference this really makes though
  # space = c("basin","hillslope","zone","patch","stratum","fire")
  # time = c("hourly", "daily", "monthly", "yearly")
  # grow = c("grow_", "")
  # all_inputs = rbind(expand.grid(grow,space[!space == "fire"], time, stringsAsFactors = FALSE),
  #                    expand.grid("",space[space == "fire"], time, stringsAsFactors = FALSE))
  # space_ab = c("b", "h", "z", "p", "c","f")
  # time_ab = c("h", "d", "m", "y")
  # grow_ab = c("g", "")
  # all_abrevs = rbind(expand.grid(grow_ab, space_ab[!space_ab == "f"], time_ab, stringsAsFactors = FALSE),
  #                     expand.grid("", space_ab[space_ab == "f"], time_ab, stringsAsFactors = FALSE))
  # sufs = paste0(all_inputs[,1], all_inputs[,2], ".", all_inputs[,3])
  # abrevs = paste0(all_abrevs[,2], all_abrevs[,3], all_abrevs[,1])

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
    if (any(!file.exists(files_in))) stop(paste0("Cant find ", files[!file.exists(files)]))
  } else if (all(output_variables$out_file %in% abrevs)) {
    files_in = file.path(output_folder, paste0(output_filename, "_", sufs[match(output_variables$out_file, abrevs)]))
    if (any(!file.exists(files_in))) stop(paste0("Cant find ", files[!file.exists(files)]))
  } else {    # probs should add a check for if file exists etc. - need to double check on time penalty though
    files_in = output_variables$out_file
    if (any(!file.exists(files_in))) stop(paste0("Cant find ", files[!file.exists(files)]))
  }

  files_out = file.path(output_folder,"allsim", output_variables$variable)

  if (run == 1 && output_initiation == 1){
    z = suppressWarnings(file.remove(files_out))
    cat(noquote(paste0("> ", files_out," \n")))
    #system(sprintf("echo > %s/allsim/%s", output_folder, output_variables$variable[dd])) # I think this is just to print what is being output?
  }

  files_read = unique(files_in)
  file_index = match(files_in, files_read)
  read_data = lapply(files_read, data.table::fread)

  # no error checking rn, should add something
  data_subset = lapply(seq_along(file_index), function(X) data.table:::subset.data.table(read_data[[file_index[X]]], select = output_variables$variable[X]))

  # the rest here could be optimized different ways
  # read and write with data table is faster than Unix paste, so going w that for now.
  # alternative - write to sqlite dbase or just rdata format intermediate, only to text last itr

  if (run == 1) {
    lapply(seq_along(files_out), function(X) colnames(data_subset[[X]]) = paste0(output_variables$variable[X],"_",run))
    z = lapply(seq_along(files_out), function(X) data.table::fwrite(data_subset[[X]], files_out[X]))
  } else {
    read_append = lapply(files_out, data.table::fread)
    data_append = function(X) {
      data.table::fwrite(read_append[[X]][, data.table::set(x = read_append[[X]], j = paste0(output_variables$variable[X],"_",run), value = data_subset[[X]])], files_out[X])
    }
    z = lapply(seq_along(files_out), data_append)
  }

  if (return_data && run == 1) {
    return(data_subset)
  }
  return()

}


