#' IOin_hdr
#'
#' Creates a header file based on specified parameter definition files and climate basestations.
#' @param basin Path to basin parameter definition file.
#' @param hillslope Path to hillslope parameter definition file.(s)
#' @param zone Path to zone parameter definition file(s).
#' @param soil Path to soil parameter definition file(s).
#' @param landuse Path to landuse parameter definition file(s).
#' @param stratum Path to stratum parameter definition file(s).
#' @param fire Path to fire parameter definition file.
#' @param fire_grid_prefix Path and basename/prefix name of the fire grid files used for the RHESSys WMFire model.
#' @param spinup Path to spinup parameter definition file(s).
#' @param basestations Path to basin climate basestation file(s).
#'
#' @author Will Burke
#'
#' @export

# LIST NAMING - MIRRORS INPUT ARS
# shorter, and still clear since the whole object is the header info, don't need to specify def
# ORDERING - keep in current order, same as args, should use names to reference though


IOin_hdr = function(basin,
                    hillslope,
                    zone,
                    soil,
                    landuse,
                    stratum,
                    fire = NULL,
                    fire_grid_prefix = NULL,
                    spinup = NULL,
                    basestations) {

  input_hdr_list <- list()

  # TODO check for each file, warn if missing - should work for everything expect clim which might be generated
  # remember each input can be a character vector of length 1+

  input_hdr_list$basin_def <- basin
  input_hdr_list$hillslope_def <- hillslope
  input_hdr_list$zone_def <- zone
  input_hdr_list$soil_def <- soil
  input_hdr_list$landuse_def <- landuse
  input_hdr_list$stratum_def <- stratum
  input_hdr_list$fire_def <- fire
  input_hdr_list$fire_grid_prefix <- fire_grid_prefix
  input_hdr_list$spinup_def <- spinup
  input_hdr_list$base_stations <- basestations

  return(input_hdr_list)

}
