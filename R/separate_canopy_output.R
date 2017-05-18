#' Separate Canopy Output by Layers
#'
#' Function separates RHESSys canopy output by canopy and outputs a data frame that
#' may be analyzed via \code{ggplot}.
#'
#' @param data Data frame with variables that need to be separated.
#' @param num_canopies Number of canopy (stratum) per patch
#'
#' @export
separate_canopy_output <- function(data, num_canopies = 2){

  select_rows <- function(x,y,z) y[seq(x,nrow(y),z),]
  variables_by_canopy <- sapply(seq_len(num_canopies), select_rows, y=data, z=num_canopies, simplify=FALSE)
  names_by_canopy <- sapply(seq_len(num_canopies), function(x,y) rep(x,nrow(y)), y=variables_by_canopy[[1]], simplify = FALSE)

  tmp <- lapply(seq_len(num_canopies), function(x,y,z) cbind(y[[x]],canopy_layer=z[[x]]), y=variables_by_canopy, z=names_by_canopy)
  canopy_df <- Reduce(rbind, tmp)

  return(canopy_df)
}
