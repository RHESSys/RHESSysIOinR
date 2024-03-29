% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_run_info.R
\name{write_run_info}
\alias{write_run_info}
\title{write_run_info}
\usage{
write_run_info(
  rhessys_version,
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
  return_cmd = FALSE
)
}
\description{
Write a text file containing metadata for each RHESSys run, including date and time, RHESSys binary used,
input files used, and if output files were created and where they are located. Defaulting to write the file where
your output is.
}
