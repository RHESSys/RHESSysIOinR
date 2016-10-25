#' Selects parameter (and variables) to be dynamically searched
#'
#' This function calls an awk file for substituting parameter values in RHESSys.
#' May also be used for substituting variables in worldfiles.
#'
#' @param par_value ??
#' @param awk_file ??
#' @param input_file ??
#' @param output_folder ??
#'
#'
#' @export
change_parameters <- function(par_value, awk_file, input_file, output_folder){

  tmp <- sprintf("awk -f %s par=%f < %s > %s", awk_file, par_value, input_file, output_folder)
  system(tmp)
}
