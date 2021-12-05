#' build_output_filter
#'
#' Creates a single output filter in R list format. This can be combined with other filters
#' and/or written to file to be read by RHESSys when running.
#' @param timestep The timestep of the filter: 'daily', 'monthly', 'annual', 'hourly'
#' @param output_format The format for the RHESSys output files to be written: 'csv' or 'netcdf'
#' @param output_path The path where RHESSys output will be written.
#' @param output_filename The filename for RHESSys output
#' @param spatial_level The spatial level for output to be collected at/aggregated to: 'basin', 'hillslope', 'zone', 'patch', 'stratum'
#' @param spatial_ID IDs to subset the spatial units by.
#' @param variables The RHESSys internal variable names to output. If aggregating from a finer spatial level use syntax: '<spatial>.<varaible>'
#'
#' @author Will Burke
#' @export


# ---------- build_output_filter ----------

# Make a single output filter in R
# Filter is a data object with following format
# list object containing 1 list named 'filter'
# list 'filter' contains 3 items:
#   <chr> 'timestep' - 'daily', 'monthly', 'annual', 'hourly'
#   <list, length 3> 'output'
#     <chr, length 1> 'format' - 'csv' or 'netcdf'
#     <chr, length 1> 'path'
#     <chr, length 1> 'filename'
#   <list, length 2> '<spatial level>' - 'basin', 'hillslope', 'zone', 'patch', 'stratum'
#     <int, length n> 'ids' - the basin or patch id or ids
#     <chr, length n> 'variables' - the rhessys internal variable names,
#         if aggregating up to a larger spatial level, use <spatial>.<varaible> format.

build_output_filter = function(timestep = c('daily', 'monthly', 'annual', 'hourly'),
                               output_format = c('csv', 'netcdf'),
                               output_path,
                               output_filename,
                               spatial_level = c('basin', 'hillslope', 'zone', 'patch', 'stratum'),
                               spatial_ID,
                               variables) {

  if (is.null(spatial_ID)) {
    spatial_ID = ""
  }

  filter_obj = list("filter" = list(
    "timestep" = timestep,
    "output" = list(
      "format" = output_format,
      "path" = output_path,
      "filename" = output_filename
    ),
    list("ids" = spatial_ID,
         "variables" = variables)
  ))
  # Possibly add check that all spatial_level (if more than one) are identical for each output filter
  names(filter_obj$filter)[3] = spatial_level[[1]]

  # if you're allowed to have multiple spatial levels per filter, take the basin part out and append that
  return(filter_obj)

}
