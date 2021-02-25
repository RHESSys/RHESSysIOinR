#' IOin_std_pars
#'
#' Input standard soil parameters for RHESSys, see https://github.com/RHESSys/RHESSys/wiki/RHESSys-command-line-options
#' @param m Decay of hydraulic conductivity with depth.
#' @param k Hydraulic conductivity at the surface.
#' @param soil_dep Soil depth.
#' @param m_v Multiplier to scale the vertical decay of hydraulic conductivity with depth.
#' @param k_v Multiplier to scale the vertical hydraulic conductivity at the surface.
#' @param pa Multiplier to scale the pore size index, set in soil def file.
#' @param po Multiplier to scale the psi air entry, set in soil def file.
#' @param gw1 Multiplier on the sat_to_gw_coeff parameter set in the soil definition file
#' (representing the amount of water moving from the saturated store to the groundwater store).
#' @param gw2 Multiplier on the gw_loss_coeff parameter in the hillslope default file
#' (representing the amount of water moving from the groundwater store to the stream).
#' @param vgseng1 Multiples specific leaf areas.
#' @param vgseng2 Multiplies the ratio of shaded to sunlit leaf area.
#' @param vgseng3 Multiplier used only with the Dickenson algorithm of carbon allocation
#' (set with the epc.allocation_flag variable in the vegetation definition file). It changes
#' the allocation of net photosynthate sensitivity based on the current LAI. If not using the
#' Dickenson strategy of carbon allocation (i.e. using Waring or default Constant strategies),
#' set third value to 1.0. (i.e. -vgsen 1.0 2.0 1.0)
#' @param n The number of parameter sets to generate.
#' @param pct_range The percent range of variation from input values over which sampling (if any), will happen.
#'
#' @author Will Burke
#'
#' @export
#'

#  FORMAT -  this is pretty constrained, so a named vector works well here

# STANDARD PARS USAGE
# With the exception of gw and gw2, all of the command line parameters can be instead set via def file parameters.
# Because of this, the assumption moving forward will be that this function will not be used by default, and so the
# default values will all be set to 1, and if left as 1 (but still passed the run_rhessys_single/multi), will be ignored, since
# they would have no effect. Only non 1 parameters will be passed to the command line.

IOin_std_pars = function(m = 1,
                         k = 1,
                         soil_dep = 1,
                         m_v = 1,
                         k_v = 1,
                         pa = 1,
                         po = 1,
                         gw1 = 1,
                         gw2 = 1,
                         vgseng1 = 1,
                         vgseng2 = 1,
                         vgseng3 = 1,
                         n = 1,
                         pct_range = 0.25) {


  # TODO check here for real world ranges of the parameters, warn if out of bounds
  #
  std_pars = list(
    m = m,
    k = k,
    soil_dep = soil_dep,
    m_v = m_v,
    k_v = k_v,
    pa = pa,
    po = po,
    gw1 = gw1,
    gw2 = gw2,
    vgseng1 = vgseng1,
    vgseng2 = vgseng2,
    vgseng3 = vgseng3
  )

  if (n > 1) {
    std_pars = lapply(std_pars, function(x) stats::runif(n = n, min = x - (pct_range * x), max = x + (pct_range * x)))
  }

  return(std_pars)

}
