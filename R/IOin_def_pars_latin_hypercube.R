#' IOin_def_pars_latin_hypercube
#'
#' Generates multiple def file changes based on a sample of parameter values across the full range of each parameter.
#' @param ... Any number of lists, each containing 3 elements in format: list("<def file>", "<parameter name>", <c(n, min value, max value)>). The last element in the list is a vector containing the total number of parameter sets, minimum value of parameter sampling range, and maximum value of parameter sampling range. n must be equal across all lists.
#' @param rm_dup TRUE/FALSE should duplicate def file + variable entries be automatically removed? A warning will occur regardless.
#'
#' @author Ryan Bart
#'
#' @export


IOin_def_pars_latin_hypercube = function(..., rm_dup=TRUE) {

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
    names(x) = c("Def_file", "Variable","Value")
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

  # Inputs for Latin Hypercube
  k <- length(pars)
  min_par <- lapply(seq_along(pars), function(x,y) y[[x]][[3]][2], y=pars)
  max_par <- lapply(seq_along(pars), function(x,y) y[[x]][[3]][3], y=pars)
  n <- pars[[1]][[3]][1]

  # Create parameter values via latin hypercube
  grid <- lhs::randomLHS(n, k) # Generate generic hypercube
  lhc_out <- mapply(function(w, x, y, z) qunif(w[,x], min=y, max=z), x=seq_len(k), y=min_par, z=max_par, MoreArgs = list(w=grid), SIMPLIFY = FALSE)

  pars_out = mapply(function(x, y) {x[[3]] = y; return(x)}, x = pars, y = lhc_out, SIMPLIFY = F)

  return(pars_out)

}

