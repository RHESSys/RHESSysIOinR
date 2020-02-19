#' watbal_patch.R
#'
#' Water balance for basin daily RHESSys output

# PATCH DAILY WATER BALANCE FUNCTIONS

# NOTES:
# (1) Before you run the water balance script below, you must have
#     added a "rain" column to your patch daily output data that contains
#     the patch-specific precipitation. Be careful to add the right precip
#     values if your worldfile uses multiple base stations, isohyets,
#     or a precip lapse rate.
# (2) These scripts will not work if you have multiple stratums.

# TO RUN:
# (1) In order to run this routine, you need a patch daily output file with
#     a rain field added and a canopy daily output file.
# (2) Copy and paste into R the water balance and multipatch run
#     functions below (i.e., all the text below "##### PASTE BELOW #####")
# (3) If you only have a single patch and a single canopy, you can run
#     the watbal function as-is.  For example, if your patch output is called
#     "rp" and your canopy output is called "rc" then type:
#     rp=pwatbal(rp,rc)
#     This will add a number of fields to your patch output file, the last
#     of which will be called "watbal".  This is your water balance error.
#     You should see a minor numerical error here even if your water balances
#     (on the order of 10^-6).  If your watbal values are negative then
#     water inputs are less than water outputs, and vice versa.
# (4) If you have multiple patches with only a single stratum per patch,
#     you need to run a different script to loop through patches
#     individually, call the water balance function, then write the output.
#     For example, if your patch output is called "rp", your canopy output
#     is called "rc", and you want the water balance output written to a
#     table called "out", then you would type:
#     out=pwatbalmult(rp,rc)
#     This creates a new output table, called "out" in this example, that
#     contains date fields and a single column for each patch with the water
#     balance error for that patch (called "P345","P578","P900", etc. where
#     the field label number matches the patch ID). You might see a minor
#     numerical error here even if your water balances (on the order of 10^-6).
#     If your watbal values are negative then water inputs are less than water
#     outputs and vice versa.
#     *** Note that this may take a long time to run if you have many patches;
#     better to run on a single patch or a single hillslope than a full basin.


##### PASTE BELOW #####
##### WAT BAL FUNCTION #####
pwatbal = function(qp, qc) {
  qp$watbal.tmp = with(qp, rain + Qin - Qout - trans_sat - trans_unsat - evap - evap_surface - soil_evap)
  qp$sd = with(qp, sat_def - rz_storage - unsat_stor)
  tmp = diff(qp$sd)
  tmp = c(0, tmp)
  qp$sddiff = tmp
  tmp = diff(qp$snow)
  tmp = c(0, tmp)
  qp$snodiff = tmp
  tmp = diff(qp$detention_store)
  tmp = c(0, tmp)
  qp$detdiff = tmp
  tmp = diff(qp$litter.rain_stor)
  tmp = c(0, tmp)
  qp$litdiff = tmp
  tmp = diff(qc$rain_stored + qc$snow_stored)
  tmp = c(0, tmp)
  qp$candiff = tmp
  qp$watbal = with(qp, watbal.tmp + sddiff - snodiff - detdiff - litdiff)

  return(qp)
}
##### MULTIPATCH RUN FUNCTION #####
# pwatbalmult = function(rp, rc) {
#   pids = unique(rp$patchID)
#   tmp = subset(rp, rp$patchID == pids[1])
#   wbp = tmp[c("day", "month", "year")]
#   wbp = mkdate(wbp)
#   n = ncol(wbp) + 1
#   for (i in pids) {
#     tmpp = subset(rp, rp$patchID == i)
#     tmpc = subset(rc, rc$patchID == i)
#     tmpp = pwatbal(tmpp, tmpc)
#     wbp[, n] = tmpp$watbal
#     names(wbp)[c(n)] = paste("P", i, sep = "")
#     n = n + 1
#   }
#   wbp
# }
