#' Write Header File
#'
#' New version of header file creation function.
#' @inheritParams run_rhessys_single
#' @param def_files Data frame of def file parameter changes
#'
#' @author Will Burke

make_hdr_file = function(input_rhessys,
                         hdr_files,
                         def_files,
                         runID) {

  # Create hdr output folder
  if (!is.null(input_rhessys$world_hdr_path)) {
    world_hdr_path <- file.path(dirname(input_rhessys$world_file), input_rhessys$world_hdr_path)
  } else {
    world_hdr_path <- file.path(dirname(input_rhessys$world_file), input_rhessys$world_hdr_prefix)
  }
  if (!dir.exists(world_hdr_path)) {dir.create(world_hdr_path)}

  # check the hdr items being used
  hdr_def_opts = c("basin_def", "hillslope_def", "zone_def", "soil_def", "landuse_def",
                   "stratum_def", "fire_def", "fire_grid_prefix", "spinup_def", "base_stations")
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
  }, hdr_values, names(hdr_values), SIMPLIFY = F)

  # make combined df
  hdr_df = data.frame(unlist(hdr_values), unlist(hdr_vars), row.names = NULL)
  # fix the names up
  hdr_df[,2] = gsub("def", "default_file", hdr_df[,2])
  hdr_df[,2] = gsub("base_stations", "base_stations_file", hdr_df[,2])
  hdr_df[,2][startsWith(hdr_df[,2], "num")] = paste0(hdr_df[,2][startsWith(hdr_df[,2], "num")],"s")

  # replace with modified defs where needed
  if (!is.null(def_files)) {
    rep_ind = lapply(def_files$old, function(x, y) {which(y == x)}, hdr_df[,1])
    hdr_df[unlist(rep_ind),1] = def_files$new
  }

  if (!is.null(runID)) {
    runID = paste0("_",runID)
  }
  world_hdr_name_out <- file.path(world_hdr_path, paste0(input_rhessys$world_hdr_prefix, runID, ".hdr"))
  utils::write.table(hdr_df, file = world_hdr_name_out, col.names = FALSE, row.names = FALSE, quote = FALSE, sep = "\t\t")
  cat("===== Wrote hdr file '",world_hdr_name_out,"' =====\n", sep = "")

  # NOTE ON WRITE SPEEDS
  # write times w data.table::fwrite is only ~100 microsec faster for the header
  # for example so leaving all the writing as write.table for now

  return(world_hdr_name_out)

}

