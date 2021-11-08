#' read_clim
#'
#' Read in rhessys formatted climate to R - works for any daily input, can have mismatched dates, missing values will be filled by NA
#' @param clim_in Climate file - prefix will return all matching data, including suffix returns just that time series
#' (e.g. 'site.rain' only return the precipitation time series).
#' @param dates_out Should start and end dates be output?
#' @author Will Burke
#' @export

read_clim = function(clim_in, dates_out = FALSE) {

  # if its the base station, return it read in and in list form
  if (endsWith(clim_in, ".base")) {
    base_in = readLines(clim_in)
    base = strsplit(base_in, "\\s+")
    return(base)
  }

  opts = c(".rain", ".tmin", ".tmax", ".tavg", ".dayl", ".daytime_rain_duration",
  ".LAI_scalar", ".Ldown", ".Kdown_direct", ".Kdown_diffuse", ".ndep_NO3", ".ndep_NH4",
  ".PAR_direct", ".PAR_diffuse", ".relative_humidity", ".tday", ".tnightmax", ".tsoil",
  ".vpd", ".wind", ".CO2", ".lapse_rate_tmin", ".lapse_rate_tmax",".tdewpoint")

  # for a single file ( or multiple maybe?)
  if (any(endsWith(clim_in, opts))) {
    if (file.exists(clim_in)) {
      files_in = clim_in
    } else {
      print(noquote("Specified clim file does not exist"))
      return(NULL)
    }
  } else {
    fileopts = paste0(clim_in,opts)
    files_in = fileopts[file.exists(fileopts)]
  }

  if (length(files_in) == 0) {
    warning("Could not find any clim files matching path and prefix: ", clim_in, ". Returning 'NULL'.")
    return(NULL)
  }

  read_in = lapply(files_in, readLines)
  starts_in = sapply(read_in, "[[", 1)
  lengths_in = sapply(read_in, length)
  trimstart = sapply(strsplit(starts_in,"\\s+"), function(x) paste(x[1],x[2],x[3]))

  start_dates = as.POSIXct(trimstart,format = "%Y %m %d")

  datefun = function(x, y) {
    seq.POSIXt(from = x, by = "DSTday", length.out = y - 1)
  }
  date_seqs = mapply(datefun, start_dates, lengths_in, SIMPLIFY = FALSE)

  dataonly = mapply(function(x) as.numeric(x[2:length(x)]), read_in, SIMPLIFY = FALSE)

  premerge = mapply("data.frame", date = date_seqs, dataonly, stringsAsFactors = FALSE, SIMPLIFY = FALSE)

  clim = Reduce(function(x,y) merge(x = x, y = y, by = "date", all = TRUE), premerge)
  names(clim)[2:ncol(clim)] = gsub("\\.","", unlist(str_extract_all(files_in, str_c(opts,collapse="|"))))

= gsub("\\.","", opts[endsWith(files_in, opts)])

  if (dates_out) {
    start_end = as.Date(c(min(clim$date), max(clim$date)), format = "%m/%d/%y")
    start_end = gsub("-", " ",start_end)
    start_end = paste(start_end, c("01", "24"))
    return(start_end)
  }

  clim$date = as.POSIXlt(clim$date)
  clim$year = clim$date$year + 1900
  clim$month = clim$date$mon + 1
  clim$day = clim$date$mday
  #clim$day_of_year = clim$date$yday
  clim$date = as.POSIXct(clim$date)
  clim$wy = data.table::fifelse(clim$month >= 10, clim$year + 1, clim$year)
  clim$yd = lubridate::yday(clim$date)
  wy_date = c(clim$date[93:length(clim$date)], seq.POSIXt(from = clim$date[length(clim$date)], by = "DSTday", length.out = 93)[2:93])
  clim$wyd = lubridate::yday(wy_date)

  return(clim)
}

