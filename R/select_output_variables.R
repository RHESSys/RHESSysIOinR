#' Selects RHESSys output variables
#'
#'  for now simply paralleling the approach used with awks but with R input
#'
#' @param output_variables A data frame containing containing variable of interest, location/name of awk file (relative to
#' output_file location), and the location/name of rhessys output file with variable
#' of interest.
#' @param output_folder Folder where rhessys output is located (e.g. 'out')
#' @param run Simulation number. Used to reset files in allsim at beginning of simulation
#' @export
select_output_variables <- function(output_variables, output_folder,run,output_initiation){

    if (run == 1 && output_initiation == 1){
      for (dd in seq_len(nrow(output_variables))){
        system(sprintf("rm %s/allsim/%s", output_folder, output_variables$variable[dd]))
        system(sprintf("echo > %s/allsim/%s", output_folder, output_variables$variable[dd]))
      }
    }

    for (cc in seq_len(nrow(output_variables))){
      system(sprintf("rm %s/allsim/t%s", output_folder, output_variables$variable[cc]))
      fin_name = sprintf("%s/%s", output_folder, output_variables$out_file[cc])
      results = read.table(fin_name, header=T)
      fout_name = sprintf("%s/allsim/t%s", output_folder, output_variables$variable[cc])
      write(signif(results[,output_variables$variable[cc]],5), ncolumns=1, file=fout_name)
      system(sprintf("paste %s/allsim/%s %s/allsim/t%s > %s/allsim/new%s", output_folder, output_variables$variable[cc], output_folder, output_variables$variable[cc], output_folder, output_variables$variable[cc]))
      system(sprintf("mv %s/allsim/new%s %s/allsim/%s", output_folder, output_variables$variable[cc], output_folder, output_variables$variable[cc]))
    }
  }


