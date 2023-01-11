#' Sample RHESSys Climate Data
#'
#' This function is used to generate new (artifical) RHESSys climate inputs from
#' existing climate data the function returns a new climate data frame in R and
#' writes .tmax, .tmin, .rain files for RHESSys met input
#'
#' @param prefix String giving the prefix to be used for output file names (e.g
#'   "../out/seqname")
#' @param clim Original climate data to be sampled from, this must have the
#'   following columns; year, month, day, date, wy, rain, tmax, and tmin. Rain
#'   must be in mm, tmax and tmin in C. There must be a value for every day in
#'   each water year and it must be in sequential order. Columns can be in any
#'   order, a file created by read_RHESSys_met will work.
#' @param samplewyrs is the vector of water years to be sampled from the
#'   original climate, in the order you want them to occur. A water year can be
#'   used more than once. samplewyrs must be included in the call to the
#'   function.
#' @param reps Creates a sequence where your samplewyrs will be repeated. If
#'   samplewyrs=c(2004,2000) and rep=4, the sequence you will get is made from
#'   2004,2000,2004,2000,2004,2000,2004,2000. Reps is optional and default to 1.
#' @param startwyr is the water year to be used for the first day of the newly
#'   generated sequence thus, if startwyr=1998, the new sequence will start on
#'   10/1/1998. startwyr is optional. If startwyr is not listed the program will
#'   use the first water year of the original climate sequence (clim)
#'
#' @export

write_sample_clim = function(prefix, clim, samplewyrs, reps=1, startwyr=0) {

  totalyrs = length(samplewyrs)

  newclim = as.data.frame(matrix(nrow=totalyrs*reps*366, ncol=9))

  firstdate = ifelse(startwyr==0, clim$wy[1], startwyr)
  firstdate = c(firstdate,10,1, firstdate-1)
  names(firstdate) = c("wy","month","day","year")
  firstdate = as.data.frame(t(firstdate))
  firstdate$date = as.Date(paste(firstdate$year, firstdate$month, firstdate$day, sep = "-"))

  colnames(newclim) = c("date","rain","tmax","tmin","oldwy","wy","month","day","year")
  newclim[,] = 0
  startr=1
  nyr = clim$wy[1]

  for (j in 1:reps) {
    for (i in 1:totalyrs)  {

      syr = samplewyrs[i]
      tmpo = subset(clim,clim$wy==syr)
      lno = nrow(tmpo)

      syr.leap=ifelse((round(syr/4) - (syr/4)) == 0, ifelse((round(syr/100)-(syr/100))==0,
                                                            ifelse((round(syr/400)-(syr/400))==0,1,0),1),0)

      nyr.leap=ifelse((round(nyr/4) - (nyr/4)) == 0, ifelse((round(nyr/100)-(nyr/100))==0,
                                                            ifelse((round(nyr/400)-(nyr/400))==0,1,0),1),0)

      if ((syr.leap == 1) && (nyr.leap==0))
        tmpo = tmpo[1:365,]
      if ((syr.leap == 0) && (nyr.leap==1)) {
        tmpo = as.data.frame(rbind(tmpo, tmpo[365,]))
        tmpo$rain[366] = 0.0
      }

      lno = nrow(tmpo)
      endr=startr+lno-1
      newclim[startr:endr,c("rain","tmax","tmin","oldwy")] = tmpo[,c("rain","tmax","tmin","wy")]
      newclim[startr:endr,"wy"] = rep(nyr, times=lno)
      startr=endr+1
      nyr = nyr+1
    }
  }

  newclim = newclim[1:endr,]
  newclim$date = seq(from=firstdate$date, length=length(newclim$date), by=1)
  newclim$year = as.integer(as.character(chron::years(newclim$date)))
  newclim$month = as.numeric(substr(as.character(newclim$date), 6,7))
  newclim$day = as.numeric(chron::days(newclim$date))
  header = sprintf("%d %d %d %d", firstdate$year[1], firstdate$month[1], firstdate$day[1], 1)
  nme = sprintf("%s.rain",prefix)
  write(header, file=nme)
  write.table(newclim$rain/1000.0, file=nme, row.names=F, col.names=F, append=T, quote=F)
  nme = sprintf("%s.tmax",prefix)
  write(header, file=nme)
  write.table(newclim$tmax, file=nme, row.names=F, col.names=F, append=T, quote=F)
  nme = sprintf("%s.tmin",prefix)
  write(header, file=nme)
  write.table(newclim$tmin, file=nme, row.names=F, col.names=F, append=T, quote=F)

  print(paste("New climate sequence is ", length(unique(newclim$wy))," wateryears (WY ", min(newclim$wy), " to ", max(newclim$wy), ")",sep=""))
  return(newclim)
}
