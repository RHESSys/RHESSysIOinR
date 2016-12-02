#' Extracts output from a multiple RHESSys runs into R
#'
#' Imports multiple-run RHESSys output that has been consolidated by variable. This
#' function imports and manipulates data so that it is 'tidy' and easily imported
#' into ggplot.
#'
#' @param var_names Vector of the variables to be imported into R
#' @param path Path to the directory containing data
#' @param initial_date Initial date for the data e.g. ymd("1941-10-1")
#' @param z Optional parameters for including in analysis
#'
#' @export
readin_rhessys_output2 <- function(var_names, path, initial_date, parameter_file = NULL){

  # Read in 'allsim' output into list
  a <- var_names %>%
    sapply(., function(., path) file.path(path, .), path=path) %>%
    lapply(., read.table, header = FALSE)

  # Inputs for processing
  dates <- seq(initial_date, initial_date + lubridate::days(length(happy[[1]]$V1)) - 1, by = "day")

  # Process data to tidy data frame (part 1)
  b <- a %>%
    lapply(., function(., dates) cbind(dates, .), dates=dates) %>%       # Add dates column to data frames
    lapply(., function(.) tidyr::gather(., run, value, -dates))          # Rearrange data frame

  # Process data to tidy data frame (part 2)
  var_names_list <- lapply(as.list(var_names), function(x) data.frame(var = rep(x,length(b[[1]]$value))))
  c <- b %>%
    Map(function(., y) cbind(y, .), ., var_names_list) %>%               # Add column signifying output variable
    Reduce(rbind, .)                                                     # rbind the data frames together

  # Add all variables related to run (e.g. parameters, dated_seq)
  if (is.null(parameter_file) == FALSE){
    parameter_output <- parameter_file %>%
      read.csv(., header = TRUE) %>%
      cbind(run = sapply(seq_len(length(.[,1])),function (x) paste("V",as.character(x),sep="")),.)
    done <- dplyr::left_join(c, parameter_output, by = "run")
  } else {
    done <- c
  }

  return(done)
}


