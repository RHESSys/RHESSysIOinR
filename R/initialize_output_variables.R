#' Initialize output variables
#'
#' This function nulls outputs from previous runs and initializes new output variables.
#'
#' @export
initialize_output_variables <- function(output_variables, output_folder){
  for (dd in seq_along(output_variables)){
    system(sprintf("rm %sallsim/%s", output_folder, output_variables[[dd]][[1]]))
    system(sprintf("echo > %sallsim/%s", output_folder, output_variables[[dd]][[1]]))
  }
}
