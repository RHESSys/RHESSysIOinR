#' Make Dated Sequence File from R
#'
#' Description...
#'
#' @param dated_seq_file Name and directory of dated sequence file to be created
#' @param dated_seq_data Data frame ...
#'
#' @export
make_dated_seq_file <- function(dated_seq_file, dated_seq_data){

  # Add some data checks here

  dated_seq_l <- length(dated_seq_data[,1])
  dated_seq_final = rbind(c(dated_seq_l, NA, NA, NA, NA), dated_seq_data)
  write.table(dated_seq_final, file = dated_seq_file, na="", col.names = FALSE, row.names=FALSE, quote = FALSE)
}


