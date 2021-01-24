#' IOin_tec_std
#'
#' Input function to construct a dataframe of standard tec events, including start, grow start, and output state
#'
#' @param start start date of run - used along with 'end' arg
#' @param end End date of run
#' @param output_state TRUE/FALSE if an output_current_state tec event should be scheduled at the end of the simulation
#'
#' @author Will Burke
#'
#' @export

IOin_tec_std = function(start, end, output_state = TRUE) {


  # check if inputs are dates or already in RHESSys date format


  start_split = unlist(strsplit(as.character(start), split = " "))
  end_split = unlist(strsplit(as.character(end), split = " "))
  input_tec_data <-
    data.frame(
      year = integer(),
      month = integer(),
      day = integer(),
      hour = integer(),
      name = character()
    )
  input_tec_data[1,] <-
    data.frame(
      as.numeric(start_split[1]),
      as.numeric(start_split[2]),
      as.numeric(start_split[3]),
      as.numeric(start_split[4]),
      "print_daily_on"
    )
  input_tec_data[2,] <-
    data.frame(
      as.numeric(start_split[1]),
      as.numeric(start_split[2]),
      as.numeric(start_split[3]),
      (as.numeric(start_split[4]) + 1),
      "print_daily_growth_on"
    )

  if (output_state) {
    input_tec_data[3,] <-
      data.frame(
        as.numeric(end_split[1]),
        as.numeric(end_split[2]),
        (as.numeric(end_split[3]) - 1),
        1,
        "output_current_state"
      )
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
