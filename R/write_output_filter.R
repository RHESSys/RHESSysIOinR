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
  aa = function(x) { return(x$output$filename)}
  if ("file_name" %in% names(output_filter)) {
    # file_name = lapply(output_filter,aa)
    # output_filter = output_filter[-which(names(output_filter) == "file_name")]

    file_name = output_filter[[which(names(output_filter) == "file_name")]]
    output_filter = output_filter[-which(names(output_filter) == "file_name")]
  }
  # test the filters are valid
  # testvalid = lapply(X = output_filter, FUN = valid_filter)
  # check for and remove duplicates
  if (any(duplicated(output_filter))) {
    output_filter = output_filter[!duplicated(output_filter)]
  }
  # check if a file name was given, otherwise autogen - can add unique id if we want
  if (is.null(file_name)) {
    file_name = "output_filter"
  }


  # add run ID to OUTPUT and FILTER filenames
  if (!is.null(runID)) {
    output_filter = lapply(output_filter, FUN = function(X, runID) {X$output$filename = paste0(X$output$filename, "_", runID); return(X)}, runID = runID)
    file_name = paste0(file_name, "_", runID)
  }

  # creating the output string manually now
  indent = "  "
  format_filter = function(fb, indent) {
    f =fb$filter
    if (is.null(f))
       { f = fb }
    level = names(f)[!names(f) %in% c("timestep", "output")]
    # to automate a check for existing quotes
    # (!grepl("\"|\'", f$output$path))
    fpath = (f$output$path)
    fname = (f$output$filename)
    fout = paste0("filter:\n", indent, "timestep: ", f$timestep, "\n", indent, "output:\n", indent, indent, "format: ", f$output$format, "\n",
                  indent, indent, "path: ", dQuote(fpath, F), "\n", indent, indent, "filename: ", dQuote(fname, F), "\n",
                  indent, level, ":\n", indent, indent, "ids: ", f[[level]]$ids, "\n", indent, indent, "variables: ", f[[level]]$variables)
    return(fout)

  }

  filter_strings = lapply(output_filter, format_filter, indent)
  filter_string = paste0(filter_strings, collapse = "\n")

  # write the output filter
  #yaml::write_yaml(x = output_filter, file = file_name)

  # workaround beacuse brians code assumes integers
  # yaml_out = yaml::as.yaml(x = output_filter, line.sep = "\n" )
  # yaml_out = gsub("\\.0", "", yaml_out)
  # yaml_out = gsub("\'\"|\"\'", "\'", yaml_out)

  # TODO remove all of this when formally fixed
  # hacky solution but works
  # yaml_out = gsub(",\\n\\s+",", ", yaml_out)

  file = file(file_name, "w", encoding = "UTF-8")
  cat(filter_string, file = file, sep = "")
  close(file)

  cat("===== Wrote output filter file '", file_name, "' =====\n", sep = "")

  return(file_name)

}
