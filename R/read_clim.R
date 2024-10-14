#' read_clim
#'
#' Read in rhessys formatted climate to R - works for any daily input, can have mismatched dates, missing values will be filled by NA.
#' Works for both standard and netcdf clim, if you specify a basestation.
#' @param clim_in Climate file - prefix will return all matching data, including suffix returns just that time series
#' (e.g. 'site.rain' only return the precipitation time series).
#' @param dates_out Should start and end dates be output?
#' @param return_base Should info from the basestation only be returned. If FALSE, data will be returned.
#' @author Will Burke
#' @importFrom ncdf4 nc_open
#' @importFrom ncdf4 ncvar_get
#' @importFrom stringr  str_remove
#' @export

read_clim = function(clim_in, dates_out = FALSE, return_base = FALSE) {

  # ============================== functions and inputs ==============================
  options(scipen = 999)
  datefun = function(x, y) {
    seq.POSIXt(from = x, by = "DSTday", length.out = y - 1)
  }
  isncdf = F
  opts = c(".rain", ".tmin", ".tmax", ".tavg", ".dayl", ".daytime_rain_duration",
  ".LAI_scalar", ".Ldown", ".Kdown_direct", ".Kdown_diffuse", ".ndep_NO3", ".ndep_NH4",
  ".PAR_direct", ".PAR_diffuse", ".relative_humidity", ".tday", ".tnightmax", ".tsoil",
  ".vpd", ".wind", ".CO2", ".lapse_rate_tmin", ".lapse_rate_tmax",".tdewpoint")

  # remove trailing '.'
  if (endsWith(clim_in, ".")) {
    clim_in = gsub("\\.$","",clim_in)
  }

  # if its the base station, return it read in and in list form
  if (endsWith(clim_in, ".base") & return_base) {
    cat("\nReading in basestation file only:\n")
    base_in = readLines(clim_in)
    base = strsplit(base_in, "\\s+")
    return(base)
  }

  # ============================== find clim files ==============================
  # if clim_in ends w .base or can find file when appended w .base, use that,
  # otherwise assume it's pointing to a specific clim file
  if (endsWith(clim_in, ".base") | file.exists(paste0(clim_in,".base"))) {
    if (endsWith(clim_in, ".base")) {
      base_in = clim_in
    } else if (file.exists(paste0(clim_in,".base"))) {
      base_in = paste0(clim_in,".base")
    }
    cat("Using basestation: ",base_in)
    base = data.frame(matrix(unlist(strsplit(readLines(base_in), "\\s+")), ncol = 2, byrow = T))
    names(base) = c("value","variable")

    if (base$variable[1] == "grid cells") {
      isncdf = T
      files_in = base$value[grepl("filename",base$variable)]
    } else {
      isncdf = F
      files_base = base$value[base$variable == "climate_prefix"]
      fileopts = paste0(files_base, opts)
      files_in = fileopts[file.exists(fileopts)]
    }
  } else {
    # could not find basestation, just try to use old version of matching files clim_in as base name
    # for a single specified file
    if (any(endsWith(clim_in, opts))) {
      if (file.exists(clim_in)) {
        files_in = clim_in
      } else {
        print(noquote("Specified single clim file does not exist"))
        return(NULL)
      }
    } else {
      # multiple files that match extensions
      fileopts = paste0(clim_in, opts)
      files_in = fileopts[file.exists(fileopts)]
    }
  }

  if (length(files_in) == 0) {
    warning("Could not find any clim files matching path and prefix/basestation: ", clim_in, ". Returning 'NULL'.")
    return(NULL)
  }

  if (isncdf) {
    # ============================== read netcdf clim ==============================
    nc_in = lapply(files_in, ncdf4::nc_open)
    # each list item is a 3 dim grid of the data
    nc_array = lapply(nc_in, function(X){ncdf4::ncvar_get(X, attributes(X$var)$names[1])})
    # wide format
    nc_df_list = lapply(nc_array, function(X){as.data.frame(matrix(X, nrow = dim(X)[3], byrow = T))})
    shortnames = sapply(nc_in, function(X){X$var[[1]]$longname})
    nc_df_list2 = mapply(function(X,Y){names(X) = paste0(Y,"_",seq_along(names(X)));return(X) },nc_df_list,shortnames, SIMPLIFY = F)
    clim = do.call(cbind, nc_df_list2)
    # add dates while in wide
    days = ncdf4::ncvar_get(nc_in[[1]], "time")
    clim$date = as.Date("1900-01-01") + days

  } else {
  # ============================== read standard clim ==============================
  read_in = lapply(files_in, readLines)
  starts_in = sapply(read_in, "[[", 1)
  lengths_in = sapply(read_in, length)
  trimstart = sapply(strsplit(starts_in,"\\s+"), function(x) paste(x[1],x[2],x[3]))
  start_dates = as.POSIXct(trimstart,format = "%Y %m %d")
  date_seqs = mapply(datefun, start_dates, lengths_in, SIMPLIFY = FALSE)
  dataonly = mapply(function(x) as.numeric(x[2:length(x)]), read_in, SIMPLIFY = FALSE)
  premerge = mapply("data.frame", date = date_seqs, dataonly, stringsAsFactors = FALSE, SIMPLIFY = FALSE)
  clim = Reduce(function(x,y) {
    tmp =  merge(x = x, y = y, by = "date", all = TRUE)
    tmp2 = subset(tmp, !is.na(date))
    return(tmp2)
  }, premerge)
  nm = stringr::str_remove(files_in, clim_in)
  nm = stringr::str_remove(nm,".")
  names(clim)[2:ncol(clim)] = nm
  clim = subset(clim, !is.na(date))
  }

  # ============================== dates out ==============================
  if (dates_out) {
    start_end = as.Date(c(min(clim$date,na.rm=T), max(clim$date,na.rm=T)), format = "%m/%d/%y")
    start_end = gsub("-", " ",start_end)
    start_end = paste(start_end, c("01", "24"))
    return(start_end)
  }

  clim$date = as.POSIXlt(clim$date)
  clim = clim |> subset(!is.na(date))
  clim$year = clim$date$year + 1900
  clim$month = clim$date$mon + 1
  clim$day = clim$date$mday
  clim$wy = ifelse(clim$month >= 10, clim$year + 1, clim$year)
  clim$yd = lubridate::yday(clim$date)
  clim$wyd = get_waterYearDay(clim$date)

  return(clim)
}

