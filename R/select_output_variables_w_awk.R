#' Selects RHESSys output variables
#'
#' This function does etc, etc... Requires that an awk file be created that indicates
#' the columns that correspond to the variable(s) of interest. See \code{create_awk}
#'
#' @param output_variables A data frame containing containing variable of interest, location/name of awk file (relative to
#' output_file location), and the location/name of rhessys output file with variable
#' of interest.
#' @param output_folder Folder where rhessys output is located (e.g. 'out')
#' @param run Simulation number. Used to reset files in allsim at beginning of simulation

#' @export
select_output_variables_w_awk <- function(output_variables, output_folder, run){

  if (run == 1){
   for (dd in seq_len(nrow(output_variables))){
      system(sprintf("rm %s/allsim/%s", output_folder, output_variables$variable[dd]))
      system(sprintf("echo > %s/allsim/%s", output_folder, output_variables$variable[dd]))
    }
  }

  for (cc in seq_len(nrow(output_variables))){
    system(sprintf("rm %s/allsim/t%s", output_folder, output_variables$variable[cc]))
    system(sprintf("awk -f %s < %s/%s > %s/allsim/t%s", output_variables$awk_path[cc], output_folder, output_variables$out_file[cc], output_folder, output_variables$variable[cc]))
    system(sprintf("paste %s/allsim/%s %s/allsim/t%s > %s/allsim/new%s", output_folder, output_variables$variable[cc], output_folder, output_variables$variable[cc], output_folder, output_variables$variable[cc]))
    system(sprintf("mv %s/allsim/new%s %s/allsim/%s", output_folder, output_variables$variable[cc], output_folder, output_variables$variable[cc]))
  }
}

