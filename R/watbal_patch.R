#' watbal_patch.R
#'
#' Water balance for a single patch daily RHESSys output.
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
#' patch output is called "rp", your canopy output is called "rc", and you want the water balance output written
#' to atable called "out", then you would type: "out=pwatbalmult(rp,rc)". This creates a new output table, called
#' "out" in this example, that contains date fields and a single column for each patch with the water balance error
#' for that patch (called "P345","P578","P900", etc. where the field label number matches the patch ID). You might
#' see a minor numerical error here even if your water balances (on the order of 10^-6). If your watbal values are
#' negative then water inputs are less than wateroutputs and vice versa. Note that this may take a long time to run
#' if you have many patches; better to run on a single patch or a single hillslope than a full basin.
#'
#' See `watbal_patch_mult` for multiple patches.
#'
#' @param pd Patch daily rhessys output, read into R with a funciton like `readin_rhessys_output()`
#' @param cd Canopy daily rhessys output, read into R with a funciton like `readin_rhessys_output()`
#'
#' @importFrom magrittr %>%
#'
#' @export

watbal_patch = function(pd, cd) {

  qouta = ifelse(pd$streamflow > 0, pd$streamflow, pd$Qout)
  pd$watbal.tmp = with(pd, pcp + Qin - qouta - trans_sat - trans_unsat - evap - evap_surface - soil_evap)
  pd$sd = with(pd, sat_def - rz_storage - unsat_stor)

  cd$weighted_snow_stored = cd$snow_stored * cd$covfrac
  cd$weighted_rain_stored = cd$rain_stored * cd$covfrac
  tmp = cd %>% dplyr::group_by(zoneID, hillID, basinID, patchID, day, month, year) %>%
    dplyr::summarize(can_snow_stored = sum(weighted_snow_stored), can_rain_stored = sum(weighted_rain_stored) )

  pd = dplyr::left_join(pd, tmp[,c("basinID","hillID","zoneID","patchID","day","month","year","can_snow_stored","can_rain_stored")])

  pd$can_water_stored = pd$can_rain_stored + pd$can_snow_stored

  tmp = diff(pd$sd)
  tmp = c(0, tmp)
  pd$sddiff = tmp
  tmp = diff(pd$snow)
  tmp = c(0, tmp)
  pd$snodiff = tmp
  tmp = diff(pd$detention_store)
  tmp = c(0, tmp)
  pd$detdiff = tmp
  tmp = diff(pd$litter.rain_stor)
  tmp = c(0, tmp)
  pd$litdiff = tmp
  tmp = diff(pd$can_water_stored)
  tmp = c(0, tmp)
  pd$candiff = tmp
  pd$watbal = with(pd, watbal.tmp + sddiff - snodiff - detdiff - litdiff - candiff)
  pd$watbal[1] = 0.0

  return(pd)
}
