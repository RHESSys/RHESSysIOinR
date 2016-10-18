#' Imports RHESSys Climate Data
#'
#' This function imports RHESSys climate data into R
#'
#' @param prename String giving the prefix for the climate data to be imported (e.g
#'   "clim/seqname").
#'
#' @examples TBD
#'
#' @export
#'
read_rhessys_met = function (prename)
{
  tname = paste(prename, ".tmin", sep = "")
  clim = read.table(tname, skip = 1, header = F)
  colnames(clim) = c("tmin")
  tname = paste(prename, ".tmax", sep = "")
  tmp = read.table(tname, skip = 1, header = F)
  clim$tmax = tmp$V1
  tname = paste(prename, ".rain", sep = "")
  tmp = read.table(tname, skip = 1, header = F)
  clim$rain = tmp$V1 * 1000
  tmp = scan(file = tname, what = "%d%d%d%d", n = 4)
  tmp2 = sprintf("%s/%s/%s", tmp[2], tmp[3], tmp[1])
  clim$date = chron::seq.dates(from = tmp2, length = length(clim$tmin))
  rm(tmp, tmp2, tname)
  clim$year = as.numeric(as.character(chron::years(clim$date)))
  clim$month = as.numeric(months(clim$date))
  clim$day = as.numeric(chron::days(clim$date))
  clim$wy = ifelse((clim$month >= 10), clim$year + 1, clim$year)
  clim
}
