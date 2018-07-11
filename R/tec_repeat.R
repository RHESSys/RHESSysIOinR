#' tec_repeat
#'
#' Repeats tec events at chosen intervals. Template should use -1111 for replacement value, and use worldfile name followed by "template"
#'
#' @param start Vector containing start date. Format is "c(year,month,day,hour)".
#' @param end Vector containing end date. Format is "c(year,month,day,hour)".
#' @param repeat_interval Interval in years to repeat chosen tec event.
#' @param tec_type Type of tec event to scheduled.
#' @param world Worldfile.
#' @param random Set as "single" to use a single random date for all events, "all" to randomize all dates.
#' @param thin_type Type of thinning event.
#' @param mult Multiplier to use for thinning.
#' @param write TRUE/FALSE write deta doc of output to file
#' @author Will Burke
#'
#' @export

tec_repeat <- function(start,end,repeat_interval,world,mult=NULL,thin_type=NULL,random=NULL,write=FALSE) {

  if(repeat_interval==0 & mult==0){
    return(data.frame())
  } else{
    start_date = as.POSIXlt(paste(start[1],"/",start[2],"/",start[3],sep=""))
    end_date = as.POSIXlt(paste(end[1],"/",end[2],"/",end[3],sep=""))

    if(!is.null(mult)){tec_type = "redefine_world_multiplier"}

    # ----- Build df of thinning tec events -----
    if (is.null(random)){ # repeat on start date
      events = seq(start_date,end_date,by=paste(repeat_interval,"year"))
      df_out = data.frame(year = lubridate::year(events),month = lubridate::month(events),day = lubridate::day(events),hour=1,name=tec_type,stringsAsFactors = FALSE)
    } else if (random=="single"){ # repeat on the same random date annually
      start_date2 = start_date
      start_date2$year = start_date2$year+1
      rand_date = sample(seq(start_date,start_date2 , by="day"),1)
      if(lubridate::month(rand_date)==2&lubridate::day(rand_date)==29){rand_date=rand_date-lubridate::days(1)} # for leap years
      events = seq(rand_date,end_date,by=paste(repeat_interval,"year"))
      df_out = data.frame(year = lubridate::year(events),month = lubridate::month(events),day = lubridate::day(events),hour=1,name=tec_type,stringsAsFactors = FALSE)
    } else if (random=="all"){ # repeat randomly every year
      error("this doesnt work yet")
    }

    # if first thinning event is on start date, put it at hour 10 so it doesn't interfere with other events
    if(all(df_out[1,1:4] == start)) {df_out[1,4]=10}

    # build vector of filenames
    filenames = paste(world,".Y",df_out[,1],"M",df_out[,2],"D",df_out[,3],"H",df_out[,4],sep = "")

    # read in template
    tx = readLines(paste(world,"template",sep="")) # read in template world

    # replace dummy value in template with multiplier
    tx2 = gsub(pattern = "-1111",replacement = mult,x = tx)
    for(fname in filenames){
      writeLines(tx2, con=fname)
    }

    # optional - output a csv of the events
    df_print = cbind(df_out,mult)
    if (write==TRUE){
      write.csv(df_print,file = paste(world,"_",repeat_interval,"rep_",mult,"thinmult",sep=""))
    }

    return(df_out)
  }
}
