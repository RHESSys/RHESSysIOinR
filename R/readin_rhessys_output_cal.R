#' Extracts output from a multiple RHESSys runs into R
#'
#' Imports multiple-run RHESSys output that has been consolidated by variable. This
#' function imports and manipulates data so that it is 'tidy' and easily imported
#' into ggplot.
#'
#' This function processes the output from multiple run calibration or simulations which consolidate a selection of the output into the allsim folder.
#'
#' @param var_names Vector of the variables names that are to be imported into R. Variables should all have the same number of layers.
#' @param path Path to the directory containing data
#' @param initial_date Initial date for the data e.g. lubridate::ymd("1941-10-01")
#' @param timestep Timestep used for modeling (e.g. yearly, monthly, or daily). Default is daily.
#' @param parameter_file Optional file containing parameters to be included in analysis (e.g. RHESSysIOinR output x_parameter_sets.csv)
#' @param num_layers Number of layers in data. For most output (e.g. patch, basin), this will generally have a value of one. The exception being when using two canopies.
#'
#' @export
readin_rhessys_output_cal <- function(var_names, path, initial_date, timestep = "daily", parameter_file = NULL, num_layers = 1){

  # Read in 'allsim' output into list
  a <- var_names %>%
    sapply(., function(., path) file.path(path, .), path=path) %>%
    lapply(., read_tsv, col_names = FALSE, skip = 2, col_types = cols(X1 = col_skip()))

  # Inputs for processing
  if (timestep == "yearly"){
    dates <- rep(seq(initial_date, initial_date + lubridate::years(length(a[[1]][[1]])/num_layers) - 1, by = "year"), times = num_layers)
  } else if (timestep == "monthly") {
    dates <- rep(seq(initial_date, initial_date + months(length(a[[1]][[1]])/num_layers) - 1, by = "month"), times = num_layers)
  } else {
    dates <- rep(seq(initial_date, initial_date + lubridate::days(length(a[[1]][[1]])/num_layers) - 1, by = "day"), times = num_layers)
  }

  # Process data to tidy data frame (part 1)
  b <- a %>%
    lapply(., separate_canopy_output, num_canopies = num_layers) %>%       # Add variable to signify if output has multiple layers
    lapply(., function(., dates) cbind(dates, .), dates=dates) %>%           # Add dates column to data frames
    lapply(., function(.) tidyr::gather(., run, value, c(-dates,-canopy_layer)))    # Rearrange data frame

  # Process data to tidy data frame (part 2)
  var_names_list <- lapply(as.list(var_names), function(x) data.frame(var_type = rep(x,length(b[[1]]$value))))
  c <- b %>%
    Map(function(., y) cbind(y, .), ., var_names_list) %>%               # Add column signifying output variable
    Reduce(rbind, .)                                                     # rbind the data frames together

  # Add all variables related to run (e.g. parameters, dated_seq)
  if (is.null(parameter_file) == FALSE){
    parameter_output <- parameter_file %>%
      read.csv(., header = TRUE) %>%
      cbind(run = sapply(seq_len(length(.[,1])),function (x) paste("X",as.character(x+1),sep="")),.)
    parameter_output$run <- as.character(parameter_output$run)
    done <- dplyr::left_join(c, parameter_output, by = "run")
  } else {
    done <- c
  }

  return(done)
}


