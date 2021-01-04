#' IOin_def_pars_sobol
#'
#' Geneartes multiple def file changes based on sobol sensativity.
#' @param ... Any number of lists, each containing 3 elements in format: list("<def file>", "<parameter name>", <value>)
#' @param nboot The number of bootstraps to run for sobol2007
#' @param rm_dup TRUE/FALSE should duplicate def file + variable entries be automatically removed? A warning will occur regardless.

#' @author Will Burke
#'
#' @export


IOin_def_pars_sobol = function(..., nboot = 100, rm_dup) {

  pars = list(...)

  # if ... is already a list of lists, ie you're inputting the output of this function, unlist to keep format correct
  if (length(pars) == 1 && all(lapply(pars[[1]], length) == 3)) {
    pars = pars[[1]]
  }
  # some checks here, should get done regardless but mostly important for multiple param sets
  if (any(lapply(pars, length) != 3)) {
    stop("Each input list must have 4 elements - Def_file, Variable, Prior_dist")
  }
  # name some things to be helpful
  name_pars = function(x) {
    names(x) = c("Def_file", "Variable","Prior_dist")
    return(x)
  }
  pars = lapply(pars, name_pars)

  # check for duplicate def_file + variable entries, if rm_dup is T, keep only the first
  file_var = paste0(sapply(pars, "[[",1), "--", sapply(pars, "[[",2))
  if (length(pars[duplicated(file_var)]) > 0) {
    warning("There are duplicate def file + variable entries, these should be corrected before running RHESSys.")
    if (rm_dup) {
      pars[duplicated(file_var)] = NULL
      cat("Duplicate def file + variable entries have been removed.\n")
    }
  }

  par_dists = sapply(pars, "[[", 3)
  #colnames(par_dists) = sapply(pars, "[[", 2)

  sobol_out = sensitivity::sobol2007(model = NULL, X1 = par_dists, X2 = par_dists, nboot = nboot)

  pars_out = mapply(function(x, y) {x[[3]] = y; return(x)}, x = pars, y = as.data.frame(sobol_out$X), SIMPLIFY = F)

  return(pars_out)

}
