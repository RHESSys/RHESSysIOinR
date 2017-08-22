#' Select RHESSys output variables
#'
#' This function...
#'
#'
#' @export
select_rhessys_output <- function(output_prefix, output_variables, run){

  # This function is under construction. The code below imports new RHESSys
  # output and selects the appropriate variables of interest and binds them to a
  # data.frame with results from previous runs. The data frame with all running
  # simulation results should then be outputed (not yet implemented). This data
  # frame needs to also be imported for each simulation (not yet implemented).
  # Also need to incorporate unique output file names. The unique file name
  # should be transferred to column name of running results for each output
  # variable. Also need to deal with cluster and deciding which outputs get
  # grouped. Also if rheesys output has unique output names (unlike with current
  # code), will need to add routine in this function to delete raw output so
  # that memory doesn't blowup.


  happy <- readin_rhessys_output(output_prefix, c=1, p=1, g=1)
  happy2 <- mapply(function(x,y,z) as.data.frame(z[[x]][y]), x = output_variables$out_type, y = output_variables$variable, MoreArgs = list(z = happy))
  happy3 <- lapply(happy2, function(x) as.data.frame(x))
  happy4 <- lapply(happy3, setNames, nm = aa)

  if (run == 1){
    out <- happy4
  } else {
    out <- mapply(function(x,y) cbind(x,y), x = out, y = happy4)

  }
}

