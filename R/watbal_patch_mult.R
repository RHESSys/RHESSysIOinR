#' watbal_patch_mult.R
#'
#' Water balance for multiple patches daily RHESSys output. 
#' 
#' Before you run the water balance script below, you must have added a "rain" column to your patch daily output
#' data that contains the patch-specific precipitation. Be careful to add the right precip values if your worldfile
#' uses multiple base stations, isohyets, or a precip lapse rate.
#'
#' This function will add a number of fields to your patch output file, the last of which will be called "watbal".
#' This is your water balance error. You should see a minor numerical error here even if your water balances (on the order of 10^-6).
#' If your watbal values are negative then water inputs are less than water outputs, and vice versa.
#'
#' If you have multiple patches with only a single stratum per patch, you need to run a different script to loop
#' through patches individually, call the water balance function, then write the output. For example, if your
#' patch output is called "pd", your canopy output is called "cd", and you want the water balance output written
#' to atable called "out", then you would type: "out=pwatbalmult(pd,cd)". This creates a new output table, called
#' "out" in this example, that contains date fields and a single column for each patch with the water balance error
#' for that patch (called "P345","P578","P900", etc. where the field label number matches the patch ID). You might
#' see a minor numerical error here even if your water balances (on the order of 10^-6). If your watbal values are
#' negative then water inputs are less than wateroutputs and vice versa. Note that this may take a long time to run
#' if you have many patches; better to run on a single patch or a single hillslope than a full basin.
#'
#' @param pd Patch daily rhessys output, read into R with a funciton like `readin_rhessys_output()`
#' @param cd Canopy daily rhessys output, read into R with a funciton like `readin_rhessys_output()`
#'
#' @export

watbal_patch_mult = function(pd, cd) {
  pids=unique(pd$patchID)
  tmp=subset(pd,pd$patchID==pids[1])
  wbp=tmp[c("day","month","year")]
  wbp=mkdate(wbp)
  n=ncol(wbp)+1
  for (i in pids) {
    tmpp=subset(pd,pd$patchID==i)
    tmpc=subset(cd,cd$patchID==i)
    tmpp=watbal_patch(tmpp,tmpc)
    wbp[,n]=tmpp$watbal
    names(wbp)[c(n)]=paste("P",i,sep="")
    n=n+1
  }
  return(wbp)
}