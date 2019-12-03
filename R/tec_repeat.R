#' tec_repeat
#'
#' Repeats tec events at chosen intervals. Outputs a dataframe of the events for use in the tec input for RHESSysIOinR,
#' optionally will copy an input redefine file with generated filenames.
#' @param start Vector containing start date. Format is "c(year,month,day,hour)".
#' @param end Vector containing end date. Format is "c(year,month,day,hour)".
#' @param interval Interval to repeat chosen tec event. If left NULL will be executed 1 time.
#' @param unit Unit of time for the repeat interval
#' @param event_name The name/type of tec event to repeat.
#' @param world Worldfile basename to modify and repeat
#' @param redefine Redefine file to copy and rename. If NULL no files copied.
#' @param overwrite Should existing files be overwritten
#' @author Will Burke
#'
#' @export

tec_repeat <- function(start, end, interval = NULL, unit, event_name, world = NULL, redefine = NULL, overwrite = FALSE) {

  # ----- Do dates right
  if (is.character(start) & length(start) == 1 & is.character(end) & length(end) == 1) {
    start = unlist(unname(strsplit(start, "\\s+")))
    end = unlist(unname(strsplit(end, "\\s+")))
  }
  if (length(start) == 4 & length(end) == 4) {
    start_date = as.POSIXlt(paste0(start[1],"/",start[2],"/",start[3]))
    end_date = as.POSIXlt(paste0(end[1],"/",end[2],"/",end[3]))
  }

  # ----- Build df of tec events -----
  if (!is.null(interval) & !is.na(interval)) {
    #events = seq(start_date,end_date,by=paste(interval,unit))
    events = as.POSIXlt(seq.POSIXt(start_date,end_date,by=paste(interval,unit)))
  } else {
    events = start_date
  }

  df_out = data.frame(year = events$year + 1900, month = events$mon + 1, day = events$mday, hour = 1, name = event_name, stringsAsFactors = FALSE)

  # if (random=="single"){ # repeat on the same random date annually
  #   start_date2 = start_date
  #   start_date2$year = start_date2$year+1
  #   rand_date = sample(seq(start_date,start_date2 , by="day"),1)
  #   if(lubridate::month(rand_date)==2&lubridate::day(rand_date)==29){rand_date=rand_date-lubridate::days(1)} # for leap years
  #   events = seq(rand_date,end_date,by=paste(repeat_interval,"year"))
  #   df_out = data.frame(year = lubridate::year(events),month = lubridate::month(events),day = lubridate::day(events),hour=1,name=tec_type,stringsAsFactors = FALSE)
  # }

  # if first thinning event is on start date, put it at hour 10 so it doesn't interfere with other events
  if(all(df_out[1,1:4] == as.numeric(start))) {
    df_out[1,4] = 10
  }



  # read in template
  # tx = readLines(paste(world,"template",sep="")) # read in template world
  #
  # # replace dummy value in template with multiplier
  # tx2 = gsub(pattern = "-1111",replacement = mult,x = tx)
  # for(fname in filenames){
  #   writeLines(tx2, con=fname)
  # }

  if (file.exists(redefine) & !is.null(world)) {
    # build vector of filenames
    filenames = paste(world,".Y",df_out[,1],"M",df_out[,2],"D",df_out[,3],"H",df_out[,4],sep = "")
    # copy redefine and rename w world name
    file.copy(redefine, filenames, overwrite = overwrite)
  }

  return(df_out)
}
