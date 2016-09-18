#'
#'
#'

select_results = function(output_file, output_selection){

  setwd(paste(output_file, "/allsim", sep=""))

  if (is.na(output_selection[1])==F){
    for (cc in seq_along(output_selection)){
      tmp = sprintf("\rm t%s", output_selection[[cc]][[1]])
      system(tmp, ignore.stderr = T)
      tmp = sprintf("awk -f %s < %s > %s", output_selection[[cc]][[2]],output_selection[[cc]][[3]],output_selection[[cc]][[4]])
      system(tmp, ignore.stderr = T)
      tmp = sprintf("paste %s t%s > new%s", output_selection[[cc]][[1]], output_selection[[cc]][[1]], output_selection[[cc]][[1]])
      system(tmp, ignore.stderr = T)
      tmp = sprintf("mv new%s %s", output_selection[[cc]][[1]], output_selection[[cc]][[1]])
      system(tmp, ignore.stderr = T)
    }
  }
  setwd("../../../")
}

