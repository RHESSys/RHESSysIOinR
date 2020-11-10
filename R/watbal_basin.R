#' watbal_basin.R
#'
#' Water balance for basin daily RHESSys output. To run, if your basin output is called "bd" type "bd=watbal(bd)".
#' This will add a number of fields to your basin output file, the last of which will be called "watbal".
#' This is your water balance error. You should see a minor numerical error here even if your water balances
#' (on the order of 10^-6).  If your watbal values are negative then water inputs are less than water outputs, and vice versa.
#' @param bd The basin daily outputs from rhessys, most easily retrieved via `readin_rhessys_output()`
#' @export

watbal_basin = function(bd) {

  # some error checks here
  req_cols = c("precip", "streamflow", "trans", "evap", "sat_def", "rz_storage", "unsat_stor",
               "snowpack", "detention_store", "litter_store", "canopy_store", "gw.storage")
  if (!is.data.frame(bd) || any(!req_cols %in% colnames(bd))) {
    cat("Input is either not a data frame or is missing the correct columns")
    return(NA)
  }

  # main fluxes
  bd$watbal_flux = with(bd, precip - streamflow - trans - evap)

  # changes in stores
  bd$sd = with(bd, sat_def - rz_storage - unsat_stor)
  bd$sddiff = c(0, diff(bd$sd))
  bd$snodiff = c(0, diff(bd$snowpack))
  bd$detdiff = c(0, diff(bd$detention_store))
  bd$litdiff = c(0, diff(bd$litter_store))
  bd$candiff = c(0, diff(bd$canopy_store))
  bd$gwdiff = c(0, diff(bd$gw.storage))

  # fluxes minus stores
  bd$watbal = with(bd, watbal_flux + sddiff - snodiff - detdiff - litdiff - candiff - gwdiff)
  bd$watbal[1] = 0.0
  # max(bd$watbal)
  # summary(bd$watbal)
  # hist(bd$watbal)

  return(bd)

}
