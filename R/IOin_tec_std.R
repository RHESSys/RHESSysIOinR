#' IOin_tec_std
#'
#' Input function to construct a dataframe of standard tec events, including start, grow start, and output state
#'
#' @param start_end Vector of two charaters, indicating start and end dates, same as used to input start and end date for input_rhessys.
#' Used to auto generate tec events for printing (normal and grow), and output current state at end
#' @param start start date of run - used along with 'end' arg
#' @param end End date of run
#' @param output_state TRUE/FALSE if an output_current_state tec event should be scheduled at the end of the simulation
#'
#' @author Will Burke
#'
#' @export

IOin_tec_std = function(start_end = NULL, start = NULL, end = NULL, output_state = TRUE){

  if (is.null(start_end) & is.null(start) & is.null(end)){
    cat("Either start_end or both start and end arguments are needed")
    return(NULL)
  }

  if (is.null(start_end) & ((is.null(start) & !is.null(end)) | (!is.null(start) & is.null(end))) ) {
    cat("Both start and end arguments are needed")
    return(NULL)
  }

  # check if inputs are dates or already in RHESSys date format

  # put small IFs here to split dates into start and end if input as start_end, then rest of function is unified



  if (!is.null(start_end)) {
    date_split = strsplit(start_end,split = " ")
    input_tec_data <-
      data.frame(
        year = integer(),
        month = integer(),
        day = integer(),
        hour = integer(),
        name = character(),
        stringsAsFactors = FALSE
      )
    input_tec_data[1, ] <-
      data.frame(
        as.numeric(date_split[[1]][1]),
        as.numeric(date_split[[1]][2]),
        as.numeric(date_split[[1]][3]),
        as.numeric(date_split[[1]][4]),
        "print_daily_on",
        stringsAsFactors = FALSE
      )
    input_tec_data[2, ] <-
      data.frame(
        as.numeric(date_split[[1]][1]),
        as.numeric(date_split[[1]][2]),
        as.numeric(date_split[[1]][3]),
        (as.numeric(date_split[[1]][4]) + 1),
        "print_daily_growth_on",
        stringsAsFactors = FALSE
      )
    if (output_state) {
      input_tec_data[3, ] <-
        data.frame(
          as.numeric(date_split[[2]][1]),
          as.numeric(date_split[[2]][2]),
          as.numeric(date_split[[2]][3]), 1,
          "output_current_state",
          stringsAsFactors = FALSE
        )
    }

  }

  if (!is.null(start) & !is.null(end) & is.null(start_end)) {
    start_split = unlist(strsplit(as.character(start),split = " "))
    end_split = unlist(strsplit(as.character(end),split = " "))
    input_tec_data <-
      data.frame(
        year = integer(),
        month = integer(),
        day = integer(),
        hour = integer(),
        name = character(),
        stringsAsFactors = FALSE
      )
    input_tec_data[1, ] <-
      data.frame(
        as.numeric(start_split[1]),
        as.numeric(start_split[2]),
        as.numeric(start_split[3]),
        as.numeric(start_split[4]),
        "print_daily_on",
        stringsAsFactors = FALSE
      )
    input_tec_data[2, ] <-
      data.frame(
        as.numeric(start_split[1]),
        as.numeric(start_split[2]),
        as.numeric(start_split[3]),
        (as.numeric(start_split[4]) + 1),
        "print_daily_growth_on",
        stringsAsFactors = FALSE
      )

    if (output_state) {
      input_tec_data[3, ] <-
        data.frame(
          as.numeric(end_split[1]),
          as.numeric(end_split[2]),
          as.numeric(end_split[3]),
          1,
          "output_current_state",
          stringsAsFactors = FALSE
        )
    }
  }


  # if (!is.null(...)) {
  #   b = list(...)
  #   if (class(b[[1]])=="data.frame"){
  #     return(b[[1]])
  #   } else{
  #     if(length(b[[1]])!=5){
  #       c = unlist(b)
  #       b = split(c,rep(c(1:(length(c)/5)),each=5))
  #     }
  #     a = data.frame(t(data.frame(b)),stringsAsFactors = FALSE) # list to df, transpose df autoconverts to mat, back to df
  #     a[,1] = as.integer(a[,1]) # this could be a sapply or something but whatever
  #     a[,2] = as.integer(a[,2])
  #     a[,3] = as.integer(a[,3])
  #     a[,4] = as.integer(a[,4])
  #     rownames(a) = c() # gross rownames gone
  #     colnames(a) = c("year","month","day","hour","name") # not sure if needed, but nice anyways
  #     return(a)
  #   }
  # }

  # COMBINE METHODS - INSERT TEC EVENTS INTO AUTO GENERATED ONES, IN CORRECT ORDER

  return(input_tec_data)
}
