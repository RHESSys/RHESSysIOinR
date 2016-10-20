#' Separate Canopy Output by Canopy
#'
#' Function separates RHESSys canopy output by canopy and outputs a data frame that
#' may be analyzed via \code{ggplot}.
#'
#' @param variable Variable of interest from canopy output.
#' @param dates Date column from canopy output
#' @param num_canopies Number of canopy (stratum) per patch
#'
#' @export
separate_canopy_output <- function(variable, dates, num_canopies = 2){

  select_rows <- function(x,y,z) y[seq(x,length(y),z)]
  variable_by_canopy <- sapply(seq_len(num_canopies), select_rows, y=variable, z=num_canopies, simplify=FALSE)
  dates_by_canopy <- sapply(seq_len(num_canopies), select_rows, y=dates, z=num_canopies, simplify = FALSE)
  names_by_canopy <- sapply(seq_len(num_canopies), function(x,y) rep(x,length(y)), variable_by_canopy[[1]], simplify = FALSE)

  tmp <- lapply(seq_len(num_canopies), function(x,w,y,z) data.frame(var=w[[x]],dates=y[[x]],names=z[[x]]), w=variable_by_canopy, y=dates_by_canopy, z=names_by_canopy)
  canopy_df <- Reduce(rbind, tmp)

  return(canopy_df)
}


