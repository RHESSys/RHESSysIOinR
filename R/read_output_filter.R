#' read_output_filter
#'
#' Reads a yaml format RHESSys output filter and creates a R list. Handles tabs in input file,
#' @param filter_in Path to the yaml filter file
#' @author Will Burke
#' @export

read_output_filter = function(filter_in) {

  # ----- handle tabs instead of spaces -----
  read_try = try(yaml::yaml.load_file(filter_in, readLines.warn = F), silent = T)
  if (class(read_try) == "try-error" && grepl("cannot open the connection", attr(read_try, 'condition'))) {
    stop("Could not load '", filter_in, "', no such file at the specified path.")
  }
  if (class(read_try) == "try-error" && grepl("Scanner error", attr(read_try, 'condition'))) {
    filter_tabs = readLines(filter_in)
    filter_clean = gsub(pattern = "\\t", replacement = "    ", x = filter_tabs)

    # ----- handle multiple filters mapped as 'filter' -----
    read_try2 = try(yaml::yaml.load(filter_clean), silent = T)
    if (class(read_try2) == "try-error" && grepl("Duplicate map key: 'filter'", attr(read_try2, 'condition'))) {
      filter_loc = grep("filter", filter_clean)
      replacement = paste0('filter',seq_along(filter_loc))
      filter_clean[filter_loc] = mapply(gsub, pattern = 'filter', replacement, filter_clean[filter_loc])
      # if it fails here, there's something else wrong, not just the filter missing unique IDs
      read_try2 = yaml::yaml.load(filter_clean)
    }
    read_try = read_try2
  }

  # ----- handle multiple filters mapped as 'filter' -----
  if (class(read_try) == "try-error" && grepl("Duplicate map key: 'filter'", attr(read_try, 'condition'))) {
    filter_clean = readLines(filter_in)
    filter_loc = grep("filter", filter_clean)
    replacement = paste0('filter', seq_along(filter_loc))
    filter_clean[filter_loc] = mapply(gsub, pattern = 'filter', replacement, filter_clean[filter_loc])
    # if it fails here, there's something else wrong, not just the filter missing unique IDs
    read_try = yaml::yaml.load(filter_clean)
  }

  filter = read_try
  names(filter) = rep("filter", length(filter))

  return(filter)
}
