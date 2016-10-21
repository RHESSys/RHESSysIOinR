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
select_output_variables <- function(output_variables, output_folder){
  for (cc in seq_along(output_variables)){
    tmp <- sprintf("rm %sallsim/t%s", output_folder, output_variables[[cc]][[1]])
    system(tmp)
    tmp <- sprintf("awk -f %s < %s%s > %sallsim/t%s", output_variables[[cc]][[2]], output_folder, output_variables[[cc]][[3]], output_folder, output_variables[[cc]][[1]])
    system(tmp)
    tmp <- sprintf("paste %sallsim/%s %sallsim/t%s > %sallsim/new%s", output_folder, output_variables[[cc]][[1]], output_folder, output_variables[[cc]][[1]], output_folder, output_variables[[cc]][[1]])
    system(tmp)
    tmp <- sprintf("mv %sallsim/new%s %sallsim/%s", output_folder, output_variables[[cc]][[1]], output_folder, output_variables[[cc]][[1]])
    system(tmp)
  }
}

