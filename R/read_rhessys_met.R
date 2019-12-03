#' Imports RHESSys Climate Data
#'
#' This function imports RHESSys climate data into R
#'
#' @param prename String giving the prefix for the climate data to be imported (e.g
#'   "clim/seqname").
#' @param dates TRUE/FALSE to return just the start and end dates, to use with run_rhessys
#'
#' @export
#'
read_rhessys_met = function (prename, dates = FALSE) {

  # tmin
  tname = paste(prename, ".tmin", sep = "")
  clim = read.table(tname, skip = 1, header = F)
  colnames(clim) = c("tmin")
  date_head = scan(file = tname, what = "%d%d%d%d", n = 4,quiet = TRUE)
  date_tmin = sprintf("%s/%s/%s", date_head[2], date_head[3], date_head[1])

  # tmax
  tname = paste(prename, ".tmax", sep = "")
  tmp = read.table(tname, skip = 1, header = F)
  clim$tmax = tmp$V1
  date_head = scan(file = tname, what = "%d%d%d%d", n = 4,quiet = TRUE)
  date_tmax = sprintf("%s/%s/%s", date_head[2], date_head[3], date_head[1])

  # rain
  tname = paste(prename, ".rain", sep = "")
  tmp = read.table(tname, skip = 1, header = F)
  clim$rain = tmp$V1 * 1000
  date_head = scan(file = tname, what = "%d%d%d%d", n = 4,quiet = TRUE)
  date_rain = sprintf("%s/%s/%s", date_head[2], date_head[3], date_head[1])

  if (length(unique(c(date_rain, date_tmax, date_tmin))) > 1) {
    stop("Start date is different across climate files")
  }

  clim$date = chron::seq.dates(from = date_tmin, length = length(clim$tmin))
  clim$year = as.numeric(as.character(chron::years(clim$date)))
  clim$month = as.numeric(months(clim$date))
  clim$day = as.numeric(chron::days(clim$date))
  clim$wy = ifelse((clim$month >= 10), clim$year + 1, clim$year)

  if (!dates) {
    return(clim)
  } else if (dates) {
    start_end = as.Date(c(min(clim$date),max(clim$date)), format = "%m/%d/%y")
    start_end = gsub("-", " ",start_end)
    start_end = paste(start_end, c("01", "24"))
    return(start_end)
  }

}
