#' Selects
#'
#'

#' @export
select_output_variables = function(output_folder, output_variables){

  setwd(paste(output_folder, "/allsim", sep=""))

  if (is.na(output_variables[1])==F){
    for (cc in seq_along(output_variables)){
      tmp = sprintf("\rm t%s", output_variables[[cc]][[1]])
      system(tmp, ignore.stderr = T)
      tmp = sprintf("awk -f %s < %s > %s", output_variables[[cc]][[2]],output_variables[[cc]][[3]],output_variables[[cc]][[4]])
      system(tmp, ignore.stderr = T)
      tmp = sprintf("paste %s t%s > new%s", output_variables[[cc]][[1]], output_variables[[cc]][[1]], output_variables[[cc]][[1]])
      system(tmp, ignore.stderr = T)
      tmp = sprintf("mv new%s %s", output_variables[[cc]][[1]], output_variables[[cc]][[1]])
      system(tmp, ignore.stderr = T)
    }
  }
  setwd("../../../")
}

