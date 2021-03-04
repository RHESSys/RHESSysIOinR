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


start = "1988 10 1 1"

start = as.Date("1988-10-1")

end = "2000 9 30 24"

end = as.Date("2000-9-30")

IOin_tec_std = function(start, end, output_state = TRUE) {

  # if inputs are rhessys format
  # if (grepl(pattern = "\\d{4} \\d{1,2} \\d{1,2} \\d{1,2}", x = start)) {
  # }

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

  start_split = unlist(strsplit(as.character(start_rh), split = " "))
  end_split = unlist(strsplit(as.character(end_rh), split = " "))

  input_tec_data <- data.frame(
      year = integer(),
      month = integer(),
      day = integer(),
      hour = integer(),
      name = character()
    )

  input_tec_data[1, ] <- c(start_split, "print_daily_on")
  input_tec_data[2, ] <- c(start_split[1:3], as.numeric(start_split[4])+1, "print_daily_growth_on")

  if (output_state) {
    input_tec_data[3, ] <- c(end_split[1:3], as.numeric(end_split[4])-1, "output_current_state")
  }

  return(input_tec_data)
}
