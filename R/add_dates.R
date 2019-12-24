#' add_dates
#' 
#' Add dates, day of year, water year, water year day to RHESSys output (as appropriate for time step)
#' @param DF input Data Frame or Data Table
#' @author Will Burke
#' 
#' @export

# this is pretty optimized but plz continue to optimize as much as possible

add_dates = function(DF)
{
  if ("day" %in% colnames(DF)) {
    DF$date = lubridate::ymd(paste(DF$year, DF$month, DF$day, sep = "-"))
    DF$wy = data.table::fifelse(DF$month >= 10, DF$year + 1, DF$year)
    yd = lubridate::yday(c(DF$date, seq.Date(DF$date[length(DF$date)], by = "day", length.out = 93)[2:93]))
    DF$yd = yd[1:(length(yd) - 92)]
    DF$wyd = yd[93:length(yd)]
    
  } else if (!"day" %in% colnames(DF) & "month" %in% colnames(DF)) {
    DF$wy = data.table::fifelse(DF$month >= 10, DF$year + 1, DF$year)
    DF$yr_mn = zoo::as.yearmon(paste(DF$year, DF$month, sep = "-"))
    
  }
  return(DF)
}