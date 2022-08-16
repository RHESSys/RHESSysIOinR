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
                           output_file = NULL,
                           input_parameters,
                           output_filter = NULL,
                           run_metadata_basename = "run_metadata",
                           command_options,
                           prefix_command = NULL,
                           return_cmd = NULL) {


  # open output filter (maybe again) and get output location
  # TODO, get and pass this from earlier output filter processing, maybe better to separate tho?
  #output_filter = "~/Projects/P301/output/filters/p301_filter.yml"
  filter_data = read_output_filter(output_filter)
  # TODO CHECK THIS WITH MULTIPLE FILTERS
  output_loc = filter_data$filter$output$path

  # make a new file with a unique date-time in the filename, keep that to add to after run completes/fails
  filename = paste0(run_metadata_basename, "_", gsub( ":", "-", sub( " ", "--", Sys.time())), ".txt")
  filepath = file.path(output_loc, filename)

  output_text = paste0("===== RHESSYS RUN METADATA =====\n",
                       "Date: ", Sys.Date(), "\n",
                       "Time: ", format(Sys.time(), "%H:%M:%S %Z"), "\n",
                       "Working Directory: ", getwd(), "\n",
                       "RHESSys Binary: ", rhessys_version, "\n",
                       "Worldfile: ", world_file, "\n",
                       "Flowtable: ", flow_file, "\n",
                       "Header File: ", world_hdr_file, "\n",
                       "Tec File: ", tec_file, "\n",
                       "Start Date: ", start_date, "\n",
                       "End Date: ", end_date, "\n",
                       "Output Filter: ", output_filter, "\n",
                       "Output File(s)": output_file, "\n",
                       "Command Line Parameters: ", input_parameters, "\n",
                       "Command Line Options: ", command_options, "\n",
                       "Run in R?: ", is.null(return_cmd), "\n"
                       )



  file = file(filepath, "w", encoding = "UTF-8")
  cat(output_text, file = file, sep = "")
  close(file)


}
