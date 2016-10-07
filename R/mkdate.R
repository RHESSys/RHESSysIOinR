#' Date code used in RHESSys
#'
#' Description...
#'
#' @param x ???
#'
#' @export
mkdate = function (x)
{
  x$date = as.Date(paste(x$year, x$month, x$day, sep = "-"))
  x$wy = ifelse(x$month >= 10, x$year + 1, x$year)
  x$yd = as.integer(format(as.Date(x$date), format = "%j"))
  x$wyd = cal.wyd(x)
  x
}
