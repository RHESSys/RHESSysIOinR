#' write_run_info
#'
#' Write a text file containing metadata for each RHESSys run, including date and time, RHESSys binary used,
#' input files used, and if output files were created and where they are located. Defaulting to write the file where
#' your output is.
#'
#'

write_run_info = function (rhessys_version,
                           world_file,
                           world_hdr_file,
                           tec_file,
                           flow_file,
                           start_date,
                           end_date,
                           output_path = NULL,
                           input_parameters,
                           output_filter = NULL,
                           run_metadata_basename = "run_metadata",
                           command_options,
                           prefix_command = NULL,
                           return_cmd = FALSE) {

  if (!is.null(output_filter)) {
    filter_data = read_output_filter(output_filter)
    output_loc = unique(lapply(filter_data, function(X) X$output$path))[1]
    # make a new file with a unique date-time in the filename, keep that to add to after run completes/fails
    filename = paste0(run_metadata_basename, "_", gsub( ":", "-", sub( " ", "--", Sys.time())), ".txt")
    filepath = file.path(output_loc, filename)
    output_files = sapply(filter_data, FUN = function(X) paste0(file.path(X$output$path, X$output$filename),".",X$output$format ))

    output_str = paste0("          Output Filter:\t", output_filter, "\n",
                        "         Output File(s):\t", paste0(output_files, collapse = ", "), "\n")
  } else if (!is.null(output_path)) {
    # for old version, untested
    filename = paste0(run_metadata_basename, "_", gsub( ":", "-", sub( " ", "--", Sys.time())), ".txt")
    filepath = file.path(output_path, filename)
    output_str = paste0("Old RHESSys Output Path:\t", output_path, "\n")
  } else {
    stop("No output filter or old RHESSys output designated")
  }

  output_text = paste0("==================== RHESSYS RUN METADATA ====================\n",
                       "                   Date:\t", Sys.Date(), "\n",
                       "                   Time:\t", format(Sys.time(), "%H:%M:%S %Z"), "\n",
                       "      Working Directory:\t", getwd(), "\n",
                       "         RHESSys Binary:\t", rhessys_version, "\n",
                       "              Worldfile:\t", world_file, "\n",
                       "              Flowtable:\t", flow_file, "\n",
                       "            Header File:\t", world_hdr_file, "\n",
                       "               Tec File:\t", tec_file, "\n",
                       "             Start Date:\t", start_date, "\n",
                       "               End Date:\t", end_date, "\n",
                       "Command Line Parameters:\t", ifelse(is.null(input_parameters), NA, input_parameters) , "\n",
                       "   Command Line Options:\t", ifelse(is.null(command_options), NA, command_options), "\n",
                       "              Run in R?:\t", !return_cmd, "\n",
                       output_str
                       )

  file = file(filepath, "w", encoding = "UTF-8")
  cat(output_text, file = file, sep = "")
  close(file)

  return(list("info_filepath" = filepath, "output_files" = output_files))

}
