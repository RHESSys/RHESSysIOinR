#' Write Header File
#'
#' New version of header file creation function.
#' @inheritParams run_rhessys_single
#' @param def_pars_df Data frame of def file parameter changes
#'
#' @author Will Burke

make_hdr_file2 = function(input_rhessys,
                          hdr_files,
                          def_files) {

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
  # dumb loop
  for (i in seq_along(def_files$old)) {
    hdr_df[,1] = gsub(def_files$old[i], def_files$new[i], hdr_df[,1])
  }

  world_hdr_name_out <- file.path(world_hdr_path, paste(input_rhessys$world_hdr_prefix,".hdr",sep=""))
  write.table(hdr_df, file = world_hdr_name_out, col.names = FALSE, row.names=FALSE, quote = FALSE, sep="\t\t")
  cat("\n===== Wrote hdr file '",world_hdr_name_out,"' =====", sep = "")

  # NOTE ON WRITE SPEEDS
  # write times w data.table::fwrite is only ~100 microsec faster for the header
  # for example so leaving all the writing as write.table for now

  return(world_hdr_name_out)

}

