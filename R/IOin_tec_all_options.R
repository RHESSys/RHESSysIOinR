#'IOin_tec_all_options
#'
#'Generic input function to construct a dataframe for all possible RHESSys
#'temporal event control (TEC) commands except csv commands. The function is
#'flexible, allowing the user to construct complex tec files. However, due to
#'the number of potential combinations of tec events, there are few checks on
#'appropriateness of tec event sequences (for example, turning off a tec event
#'before it is turned on). Full list of tec commands in RHESSys is available at
#'init/construct_tec.c
#'
#'@section Note: All arguments accept a date or vector of dates in the format of
#'  "year month day hour" e.g. c("2000 10 1 1", "2001 9 30 1").
#'
#'  For more information on redefine worlds, see wiki page \href{https://github.com/RHESSys/RHESSys/wiki/Redefining-the-worldfile}{Redefining-the-worldfile}
#'
#'@param print_hourly_on Start printing hourly output data
#'@param print_hourly_growth_on Start printing hourly carbon-related output data
#'@param print_hourly_off Stop printing hourly output data
#'@param print_hourly_growth_off Stop printing hourly carbon-related output data
#'@param print_daily_on Start printing daily output data
#'@param print_daily_growth_on Start printing daily carbon-related output data
#'@param print_daily_off Stop printing daily output data
#'@param print_daily_growth_off Stop printing daily carbon-related output data
#'@param print_monthly_on Start printing monthly output data
#'@param print_monthly_off Stop printing monthly output data
#'@param print_yearly_on Start printing yearly output data
#'@param print_yearly_growth_on Start printing yearly carbon-related output data
#'@param print_yearly_off Stop printing yearly output data
#'@param print_yearly_growth_off Stop printing yearly carbon-related output data
#'@param redefine_strata Redefine strata based on a file
#'@param redefine_world Redefine world based on a file
#'@param redefine_world_multiplier Redefine world based on a multiplier of
#'  existing worldfile values
#'@param redefine_world_thin_remain Redefine world based on a thinning event
#'  where removed vegetation is transferred to CWD and litter stores
#'@param redefine_world_thin_harvest Redefine world based on a thinning event
#'  where removed vegetation exits watershed
#'@param redefine_world_thin_snags Redefine world based on a thinning event
#'  where stems are left in place.
#'@param roads_on Start processing roads
#'@param roads_off Stop processing roads
#'@param output_current_state Output worldfile
#'
#'@author Ryan R Bart
#'
#'@export

IOin_tec_all_options = function(print_hourly_on = NULL,
                                print_hourly_growth_on = NULL,
                                print_hourly_off = NULL,
                                print_hourly_growth_off = NULL,
                                print_daily_on = NULL,
                                print_daily_growth_on = NULL,
                                print_daily_off = NULL,
                                print_daily_growth_off = NULL,
                                print_monthly_on = NULL,
                                print_monthly_off = NULL,
                                print_yearly_on = NULL,
                                print_yearly_growth_on = NULL,
                                print_yearly_off = NULL,
                                print_yearly_growth_off = NULL,
                                redefine_strata = NULL,
                                redefine_world = NULL,
                                redefine_world_multiplier = NULL,
                                redefine_world_thin_remain = NULL,
                                redefine_world_thin_harvest = NULL,
                                redefine_world_thin_snags = NULL,
                                roads_on = NULL,
                                roads_off = NULL,
                                output_current_state = NULL) {


  # ---------------------------------------------------------------------

  options(stringsAsFactors = F)

  # Initialize data frame
  input_tec_data <- data.frame(
    year = integer(),
    month = integer(),
    day = integer(),
    hour = integer(),
    name = character()
  )

  # Function for processing each command argument into a data frame
  make_tec_input <- function(tec_command, tec_command_name){
    input_tec_df <- lapply(tec_command, function(x){unlist(strsplit(as.character(x), split = " "))})
    input_tec_df <- as.data.frame(do.call(rbind, input_tec_df))
    input_tec_df <- cbind(input_tec_df, rep_len(tec_command_name, length.out = nrow(input_tec_df)))
    names(input_tec_df) <- c("year", "month", "day", "hour", "name")
    input_tec_df$year <- as.integer(input_tec_df$year)
    input_tec_df$month <- as.integer(input_tec_df$month)
    input_tec_df$day <- as.integer(input_tec_df$day)
    input_tec_df$hour <- as.integer(input_tec_df$hour)
    return(input_tec_df)
  }


  # ---------------------------------------------------------------------
  # Run make_tec_input function and combines outputs into a data frame

  # ----
  # Hourly

  if (!is.null(print_hourly_on)){
    name <- "print_hourly_on"
    input_tec_df <- make_tec_input(tec_command = print_hourly_on, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(print_hourly_growth_on)){
    name <- "print_hourly_growth_on"
    input_tec_df <- make_tec_input(tec_command = print_hourly_growth_on, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(print_hourly_off)){
    name <- "print_hourly_off"
    input_tec_df <- make_tec_input(tec_command = print_hourly_off, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(print_hourly_growth_off)){
    name <- "print_hourly_growth_off"
    input_tec_df <- make_tec_input(tec_command = print_hourly_growth_off, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }


  # ----
  # Daily

  if (!is.null(print_daily_on)){
    name <- "print_daily_on"
    input_tec_df <- make_tec_input(tec_command = print_daily_on, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(print_daily_growth_on)){
    name <- "print_daily_growth_on"
    input_tec_df <- make_tec_input(tec_command = print_daily_growth_on, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(print_daily_off)){
    name <- "print_daily_off"
    input_tec_df <- make_tec_input(tec_command = print_daily_off, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(print_daily_growth_off)){
    name <- "print_daily_growth_off"
    input_tec_df <- make_tec_input(tec_command = print_daily_growth_off, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }


  # ----
  # Monthly

  if (!is.null(print_monthly_on)){
    name <- "print_monthly_on"
    input_tec_df <- make_tec_input(tec_command = print_monthly_on, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(print_monthly_off)){
    name <- "print_monthly_off"
    input_tec_df <- make_tec_input(tec_command = print_monthly_off, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }


  # ----
  # Yearly

  if (!is.null(print_yearly_on)){
    name <- "print_yearly_on"
    input_tec_df <- make_tec_input(tec_command = print_yearly_on, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(print_yearly_growth_on)){
    name <- "print_yearly_growth_on"
    input_tec_df <- make_tec_input(tec_command = print_yearly_growth_on, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(print_yearly_off)){
    name <- "print_yearly_off"
    input_tec_df <- make_tec_input(tec_command = print_yearly_off, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(print_yearly_growth_off)){
    name <- "print_yearly_growth_off"
    input_tec_df <- make_tec_input(tec_command = print_yearly_growth_off, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }


  # ----
  # Redefine worlds

  if (!is.null(redefine_strata)){
    name <- "redefine_strata"
    input_tec_df <- make_tec_input(tec_command = redefine_strata, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(redefine_world)){
    name <- "redefine_world"
    input_tec_df <- make_tec_input(tec_command = redefine_world, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(redefine_world_multiplier)){
    name <- "redefine_world_multiplier"
    input_tec_df <- make_tec_input(tec_command = redefine_world_multiplier, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(redefine_world_thin_remain)){
    name <- "redefine_world_thin_remain"
    input_tec_df <- make_tec_input(tec_command = redefine_world_thin_remain, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(redefine_world_thin_harvest)){
    name <- "redefine_world_thin_harvest"
    input_tec_df <- make_tec_input(tec_command = redefine_world_thin_harvest, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(redefine_world_thin_snags)){
    name <- "redefine_world_thin_snags"
    input_tec_df <- make_tec_input(tec_command = redefine_world_thin_snags, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }


  # ----
  # Roads

  if (!is.null(roads_on)){
    name <- "roads_on"
    input_tec_df <- make_tec_input(tec_command = roads_on, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }
  if (!is.null(roads_off)){
    name <- "roads_off"
    input_tec_df <- make_tec_input(tec_command = roads_off, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }


  # ----
  # Outputs

  if (!is.null(output_current_state)){
    name <- "output_current_state"
    input_tec_df <- make_tec_input(tec_command = output_current_state, tec_command_name = name)
    input_tec_data <- rbind(input_tec_data, input_tec_df)
  }


  # ---------------------------------------------------------------------
  # Sort so that tec file is chronological order

  input_tec_data <- input_tec_data[order(input_tec_data$year, input_tec_data$month, input_tec_data$day, input_tec_data$hour),]
  # print(input_tec_data)

  # ---------------------------------------------------------------------
  # Checks on data

  # Check for duplicate dates
  # if (input_tec_data) {
  #   stop(cat("Duplicate date found at ..."))
  # }

  return(input_tec_data)
}


