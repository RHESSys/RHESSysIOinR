#' Make Tec File from R
#'
#' Description...
#'
#' @param tec_file Name and directory of tec file to be created
#' @param tec_data Data frame containing the data needed for the tec file. Data
#'   frame columns include 'year', 'month', 'day', 'hour' and the name of tec
#'   command (e.g. "print_daily_on").
#'
#' @export
make_tec_file <- function(tec_file, tec_data){

  # Add some data checks here

  write.table(tec_data, file = tec_file, col.names = FALSE, row.names=FALSE, quote = FALSE)
}


