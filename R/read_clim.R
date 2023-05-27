#' read_clim
#'
#' Read in rhessys-formatted station or gridded climate files to R - works for
#' any daily input, can have mismatched dates, missing values will be filled by
#' NA
#' @param clim_in Climate file - prefix will return all matching data, including
#'   suffix returns just that time series (e.g. 'site.rain' only return the
#'   precipitation time series).
#' @param clim_type Specify the type of climate file to be read. Options are
#'   "station" (default) and "grid".
#' @param dates_out Alternative output: Start and end dates of clim file.
#' @param grid_out Alternative output: Vector of grid (basestation) IDs.
#' @param elevation_out Alternative output: Vector of elevations associated with
#'   each grid ID.
#' @return Standard (non-alternative) output is a table containing all data from
#'   the climate file. For a station-based climate file, a single data frame
#'   with columns for each inputted clim variable type (e.g. rain, tmax, tmin)
#'   is generated. For gridded data, output consists of a list of data frames
#'   with each data frame containing a single variable type. A separate column
#'   corresponding to each grid basestation ID is included in each data frame.
#' @author Will Burke, Ryan Bart
#' @export

read_clim = function(clim_in, clim_type = "station", dates_out = FALSE, grid_out = FALSE, elevation_out = FALSE) {

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

  parse_date <- function(x){
    x$date = as.POSIXlt(x$date)
    x = x %>% subset(!is.na(date))
    x$year = x$date$year + 1900
    x$month = x$date$mon + 1
    x$day = x$date$mday
    #x$day_of_year = x$date$yday
    x$wy = ifelse(x$month >= 10, x$year + 1, x$year)
    x$yd = lubridate::yday(x$date)
    x$wyd = RHESSysIOinR::get_waterYearDay(x$date)
    return(x)
  }

  if(clim_type == "station"){
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

    # old ver
    #clim = Reduce(function(x,y) merge(x = x, y = y, by = "date", all = TRUE), premerge)
    #names(clim)[2:ncol(clim)] = gsub("\\.","", unlist(str_extract_all(files_in, str_c(opts,collapse="|"))))

    clim = Reduce(function(x,y) {
      tmp =  merge(x = x, y = y, by = "date", all = TRUE)
      tmp2 = subset(tmp, !is.na(date))
      return(tmp2)
    }, premerge)

    # old ver: Doesn't work when using a single clim_in that has a extension
    #nm = stringr::str_remove(files_in, clim_in)
    #nm = stringr::str_remove(nm,".")
    nm = file_ext(files_in)
    names(clim)[2:ncol(clim)] = nm

    clim = subset(clim, !is.na(date))
    clim = parse_date(clim)

    if (dates_out) {
      start_end = as.Date(c(min(clim$date,na.rm=T), max(clim$date,na.rm=T)), format = "%m/%d/%y")
      start_end = gsub("-", " ",start_end)
      start_end = paste(start_end, c("01", "24"))
      return(start_end)
    }
  }

  if(clim_type == "grid"){
    starts_in = sapply(read_in, "[[", 2)
    grid_ids = sapply(read_in, "[[", 3)
    grid_elev = sapply(read_in, "[[", 4)
    lengths_in = sapply(read_in, length)
    trimstart = sapply(strsplit(starts_in,"\\s+"), function(x) paste(x[1],x[2],x[3]))
    start_dates = as.POSIXct(trimstart,format = "%Y %m %d")

    if (grid_out) {return(grid_ids[[1]])}
    if (elevation_out) {return(grid_elev[[1]])}

    datefun <- function(x, y) {
      seq.POSIXt(from = x, by = "DSTday", length.out = y - 4)
    }
    date_seqs <- mapply(datefun, start_dates, lengths_in, SIMPLIFY = FALSE)

    dataonly <- mapply(function(x) strsplit(x[5:length(x)], split = " "), read_in, SIMPLIFY = FALSE)
    dataonly <- mapply(function(x) t(do.call(cbind, x)), dataonly, SIMPLIFY = FALSE)
    # Change values to double, matrix to data frame
    dataonly <- mapply(function(x) as.data.frame(matrix(as.double(x), ncol = ncol(x))), dataonly, SIMPLIFY = FALSE)
    # Add grid ids as column names
    dataonly <- mapply(function(x, y) setNames(x, as.double(strsplit(y, split = " ")[[1]])), dataonly, grid_ids, SIMPLIFY = FALSE)
    # Bind dates to data
    clim <- mapply(function(x,y) cbind(date = x,y), date_seqs, dataonly, SIMPLIFY = FALSE)
    # Parse date
    clim <- mapply(parse_date, clim, SIMPLIFY = FALSE)
    # Assign list names
    names(clim) <- file_ext(files_in)

    # If getting climate range, only gather from first data frame in list
    if (dates_out) {
      start_end = as.Date(c(min(clim[[1]]$date,na.rm=T), max(clim[[1]]$date,na.rm=T)), format = "%m/%d/%y")
      start_end = gsub("-", " ",start_end)
      start_end = paste(start_end, c("01", "24"))
      return(start_end)
    }
  }

  return(clim)
}

