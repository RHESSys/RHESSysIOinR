#' modify_output_filter
#'
#' Modify a single existing output filter, either from file or based on an existing R object. Will error if given multiple filters.
#' This could be done via normal list modification in R as well.
#' @inheritParams read_output_filter
#' @inheritParams build_output_filter
#' @author Will Burke
#'
#' @export

modify_output_filter = function(filter_in,
                                timestep = NULL,
                                output_format = NULL,
                                output_path = NULL,
                                output_filename = NULL,
                                spatial_level = NULL,
                                spatial_ID = NULL,
                                variables = NULL) {

  if (!is.list(filter_in)) {
    filter = read_output_filter(filter_in = filter_in)
  }

  if (length(filter) > 1) {
    stop("Can only modify 1 filter at a time.")
  }

  if (!is.null(timestep)) { filter[[1]]$timestep = timestep }

  if (!is.null(output_format)) { filter[[1]]$output$format = output_format }

  if (!is.null(output_path)) { filter[[1]]$output$path = output_path }

  if (!is.null(output_filename)) { filter[[1]]$output$filename = output_filename }

  if (!is.null(spatial_level)) {
    if (is.null(variables)) {
      warning("If spatial level is being changed, variables will almost certainly need to be changed.")
    }

    filter[[1]][names(filter[[1]]) %in% c('basin', 'hillslope', 'zone', 'patch', 'stratum')] = NULL

    filter[[1]]$tmp = list("ids" = spatial_ID, "variables" = variables)

    names(filter[[1]])[names(filter[[1]]) == "tmp"] = spatial_level

  }

  if (!is.null(spatial_ID)) {
    filter[[1]][[which(names(filter[[1]]) %in% c('basin', 'hillslope', 'zone', 'patch', 'stratum'))]]$ids = spatial_ID
  }

  if (!is.null(variables)) {
    filter[[1]][[which(names(filter[[1]]) %in% c('basin', 'hillslope', 'zone', 'patch', 'stratum'))]]$variables = variables
  }

  return(filter)

}
