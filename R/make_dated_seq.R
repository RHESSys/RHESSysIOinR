#' Make Dated Sequence File from R
#'
#' Description...
#'
#' @param input_dated_seq Data frame ...
#' @param dated_seq_file File ...
#'
#' @export
make_dated_seq <- function(input_dated_seq, dated_seq_file){

  # Add some data checks here

  dated_seq_l <- length(input_dated_seq$year)
  dated_seq_final = bind_rows(c(year=dated_seq_l, month=NA, day=NA, hour=NA, value=NA),input_dated_seq)
  write.table(dated_seq_final, file = dated_seq_file, na="", col.names = FALSE, row.names=FALSE, quote = FALSE)

}


