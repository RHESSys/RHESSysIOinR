#' IOin_def_pars_sobol
#'
#' Geneartes multiple def file changes based on sobol sensativity.
#' @param ... Any number of lists, each containing 3 elements in format: list("<def file>", "<parameter name>", <value>)
#' @param nboot The number of bootstraps to run for sobol2007
#' @param rm_dup TRUE/FALSE should duplicate def file + variable entries be automatically removed? A warning will occur regardless.

#' @author Will Burke
#'
#' @export


IOin_def_pars_sobol = function(..., nboot = 100, rm_dup, sobolfunction=sensitivity::sobol2007, return_sobol=FALSE) {

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

  # sobol requires two different evenly numbered samples so we will split in two

  npars = nrow(par_dists)
  colnames(par_dists) = sapply(pars, "[[", 2)
  if ((npars %% 2) == 1)
      par_dists = par_dists[1:(npars-1),]
  npars = nrow(par_dists)
  x1 = par_dists[1:(npars/2),]
  x2 = par_dists[((npars/2)+1):npars,]


  sobol_out = sobolfunction(model = NULL, X1 = x1, X2 = x2, nboot = nboot)

  pars_out = mapply(function(x, y) {x[[3]] = y; return(x)}, x = pars, y = as.data.frame(sobol_out$X), SIMPLIFY = F)

  if (return_sobol)
  return(list(pars_out=pars_out, sobol_out=sobol_out))
    else
  return(pars_out)

}
