#' IOin_std_pars
#'
#' Input standard soil parameters for RHESSys, see https://github.com/RHESSys/RHESSys/wiki/RHESSys-command-line-options
#' @param m Decay of hydraulic conductivity with depth
#' @param k Hydraulic conductivity at the surface
#' @param m_v Multiplier to scale the vertical decay of hydraulic conductivity with depth
#' @param k_v Multiplier to scale the vertical hydraulic conductivity at the surface
#' @param pa Multiplier to scale the pore size index, set in soil def file.
#' @param po Multiplier to scale the psi air entry, set in soil def file.
#' @param gw1 Multiplier on the sat_to_gw_coeff parameter set in the soil definition file
#' (representing the amount of water moving from the saturated store to the groundwater store).
#' @param gw2 Multiplier on the gw_loss_coeff parameter in the hillslope default file
#' (representing the amount of water moving from the groundwater store to the stream).
#'
#' @author Will Burke
#'
#' @export
#'

#  FORMAT -  this is pretty constrained, so a named vector works well here


IOin_std_pars = function(m,
                         k,
                         m_v,
                         k_v,
                         pa,
                         po,
                         gw1,
                         gw2,
                         n = 1,
                         pct_range = 0.25) {


  # TODO check here for real world ranges of the parameters, warn if out of bounds
  #
  std_pars = list(m = m, k = k, m_v = m_v, k_v = k_v, pa = pa, po = po, gw1 = gw1, gw2 = gw2)

  if (n > 1) {
    std_pars = lapply(std_pars, function(x) runif(n = n, min = x - (pct_range * x), max = x + (pct_range * x)))
  }

  return(std_pars)

}
