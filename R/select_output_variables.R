#' Selects RHESSys output variables
#'
#' This function does etc, etc... Requires that an awk file be created that indicates
#' the columns that correspond to the variable(s) of interest. See \code{create_awk}
#'
#' @param output_variables A list containing information on each output variable
#' to be selected. Each variable needs to be contained in an additional list
#' in the  containing variable of interest, location/name of awk file (relative to
#' output_file location), and the location/name of rhessys output file with variable
#' of interest.
#' @param variable ??
#' @param awk_file ??
#' @param results_file ??

#' @export
select_output_variables <- function(output_variables){
  for (cc in seq_along(output_variables)){
    tmp <- sprintf("\rm t%s", output_variables[[cc]][[1]])
    system(tmp, ignore.stderr = T)
    tmp <- sprintf("awk -f %s < %s > t%s", output_variables[[cc]][[2]],output_variables[[cc]][[3]],output_variables[[cc]][[1]])
    system(tmp, ignore.stderr = T)
    tmp <- sprintf("paste %s t%s > new%s", output_variables[[cc]][[1]], output_variables[[cc]][[1]], output_variables[[cc]][[1]])
    system(tmp, ignore.stderr = T)
    tmp <- sprintf("mv new%s %s", output_variables[[cc]][[1]], output_variables[[cc]][[1]])
    system(tmp, ignore.stderr = T)
  }
}

