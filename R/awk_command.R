#' Awk script to substitute parameter values
#'
#'   Run from command line
#'

awk_command = function(par_value, awk_file, input_file, output_file){

  tmp = sprintf("awk -f %s par=%f < %s > %s", awk_file, par_value, input_file, output_file)
  system(tmp, ignore.stderr = T)
}
