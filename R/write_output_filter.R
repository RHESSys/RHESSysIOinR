#' write_output_filter
#'
#' Writes an output filter file based on an input R list containing the needed elements.
#' @inheritParams run_rhessys_single
#'
#' @author Will Burke
#' @export

write_output_filter = function(output_filter, runID = NULL) {

  # if it's just a path to an existing filter for a single run
  if (is.character(output_filter) && is.null(runID)) {
    return(output_filter)

  } else if (is.character(output_filter) && !is.null(runID)) {
    # if it's a path but there's multi runs - need to read in so output name can be iterated
    file_name = output_filter
    output_filter = read_output_filter(output_filter)

  }
  # if it's an r list object
  # if there's a file name, remove it
  file_name = NULL
  if ("file_name" %in% names(output_filter)) {
    file_name = output_filter[[which(names(output_filter) == "file_name")]]
    output_filter = output_filter[-which(names(output_filter) == "file_name")]
  }
  # test the filters are valid
  testvalid = lapply(X = output_filter, FUN = valid_filter)
  # check for and remove duplicates
  if (any(duplicated(output_filter))) {
    output_filter = output_filter[!duplicated(output_filter)]
  }
  # check if a file name was given, otherwise autogen - can add unique id if we want
  if (is.null(file_name)) {
    file_name = "output_filter"
  }
  # add run ID to output if needed
  if (!is.null(runID)) {
    output_filter = lapply(output_filter, FUN = function(X, runID) {X$output$filename = paste0(X$output$filename, "_", runID); return(X)}, runID = runID)
    file_name = paste0(file_name, "_", runID)
  }
  # write the output filter
  yaml::write_yaml(x = output_filter, file = file_name)

  return(file_name)

}
