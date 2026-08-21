#' IOin_tec_std
#'
#' Input function to construct a dataframe of standard tec events, including start, grow start, and output state
#'
#' @param start start date of run
#' @param end End date of run - the last entire day to be run
#' @param output_state TRUE/FALSE if an output_current_state tec event should be scheduled at the end of the simulation
#'
#' @author Will Burke
#'
#' @export

IOin_tec_std = function(start, end, output_state = TRUE, daily = TRUE, monthly = FALSE, yearly = FALSE) {

  inc_tec_date = function(tecdate) {
    tecdate = as.POSIXct(paste0(tecdate[1:4], collapse = " "), format = "%Y %m %d %H")
    tecdate = tecdate + lubridate::hours(1)
    tecdate_split = unlist(strsplit(format(tecdate, "%Y %m %d %H"), split = " "))
    return(tecdate_split)
  }
  
  if (class(start) == "Date") {
    start_rh = format.Date(x = start, "%Y %m %d")
    start_rh = paste0(start_rh, " 1")
  } else {
    start_rh = start
  }

  if (class(end) == "Date") {
    end_rh = format.Date(x = end, "%Y %m %d")
    end_rh = paste0(end_rh, " 24")
  } else {
    end_rh = end
  }
  
  # consistent padding from this
  start_split = unlist(strsplit(format(as.POSIXct(start_rh, format = "%Y %m %d %H"), "%Y %m %d %H"), split = " "))
  end_split = unlist(strsplit(format(as.POSIXct(end_rh, format = "%Y %m %d %H"), "%Y %m %d %H"), split = " "))

  input_tec_data <- data.frame(
      year = integer(),
      month = integer(),
      day = integer(),
      hour = integer(),
      name = character()
    )
  
  if (daily) {
    input_tec_data = rbind(input_tec_data, c(start_split, "print_daily_on"), deparse.level = 0)
    start_split = inc_tec_date(start_split)
    input_tec_data = rbind(input_tec_data, c(start_split, "print_daily_growth_on"))
    start_split = inc_tec_date(start_split)
  }
  if (monthly) {
    input_tec_data = rbind(input_tec_data, c(start_split, "print_monthly_on"))
    start_split = inc_tec_date(start_split)
  }
  if (yearly) {
    input_tec_data = rbind(input_tec_data, c(start_split, "print_yearly_on"))
    start_split = inc_tec_date(start_split)
      input_tec_data = rbind(input_tec_data, c(start_split, "print_yearly_growth_on"))
      start_split = inc_tec_date(start_split)
  }
  if (output_state) {
    end_time = as.POSIXct(end_rh, format="%Y %m %d %H")
    output_time = end_time - lubridate::hours(1)
    if (format(output_time, "%H") == "00") {
      output_time = output_time - lubridate::minutes(60)
    }
    output_split = unlist(strsplit(format(output_time, "%Y %m %d %H"), split = " "))
    input_tec_data = rbind(input_tec_data, c(output_split, "output_current_state"))    
  }
  names(input_tec_data) = c("year", "month", "day", "hour", "name")
  
  return(input_tec_data)
}
