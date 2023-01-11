#' Code used to calculate wateryear day in RHESSys
#'
#' Description...
#'
#' @param x ???
#'
#' @export
cal.wyd = function (x) {
  tmp = aggregate(x$yd, by = list(x$year), max)
  colnames(tmp) = c("year", "n")
  tmp$year = as.integer(as.character(tmp$year))
  x$wyd = 0
  tmp2 = subset(tmp, tmp$n == 365)
  new = ifelse((x$year %in% tmp2$year), ifelse(x$yd >= 274,
                                               x$yd - 273, x$yd + 92), ifelse(x$yd >= 275, x$yd - 274,
                                                                              x$yd + 92))
  new
}
