#' IOin_def_pars_simple
#'
#' The definition file parameters to modify. This input function  generates an input parameter object, either for a single simulation,
#' or using simple random sampling over a set range (based on percent difference from input values). This later functionality can be put into
#' a seperate funciton later if desired.
#' @param ... Any number of lists, each containing 3 elements in format: list("<def file>", "<parameter name>", <value>)
#' @param n The number of parameter sets to generate.
#' @param pct_range The percent range of variation from input values over which sampling (if any), will happen.
#' @param rm_dup TRUE/FALSE should duplicate def file + variable entries be automatically removed? A warning will occur regardless.
#'
#' @author Will Burke
#'
#' @export

# pars = list(list("defs/veg_p301_conifer.def", "epc.allocation_flag","dickenson"),
# list("defs/veg_p301_conifer.def", "epc.alloc_frootc_leafc", 1),
# list("defs/veg_p301_conifer.def", "epc.alloc_stemc_leafc", 0.6),
# list("defs/veg_p301_conifer.def", "epc.netpabs_shade", 0.2))

# --- IMPORTANT DATA ASSUMPTIONS ---
# We can discuss revising this, but this code (and potentially other I(WB) write) will assume that parameters
# for a single run will use the following data format (as the ultimate structure that gets passed to run_rhessys_core)
# <list, length = num of unique def file params to change>
#   <list, length = 3, elements in order: def file path, variable name, value to be set to>
#   < repeat above format for each unique def file variable>
#
# For a set of def file parameters, regardless of how they are generated, I propose using the same format except instead of a single value
# there would be a vector of values for each def file variable. Ex:
# <list, length = num of unique def file params to change>
#   <list, length = 3, elements in order: def file path, variable name, VECTOR of values to be set to>
#   < repeat above format for each unique def file variable>
#
# Def file variables not being varied, or which don't make sense to be
# (e.g. text fields like epc.allocation_flag) would need to be replicated so that each def variable list has a vector of target values
# that are the same length. These can then be iterated through or lapply'd across, potentially in parallel.
#
# How these data structures are achieved can vary by IOin function/type of parameter variation, since different methods will require
# different inputs (see the simplest option I could come up with below)

IOin_def_pars_simple = function(..., n = 1, pct_range = 0.25, rm_dup = F) {

  options(stringsAsFactors = F)

  pars = list(...)

  # if ... is already a list of lists, ie you're inputting the output of this function, unlist to keep foramt correct
  if (length(pars) == 1 && all(lapply(pars[[1]], length) == 3)) {
    pars = pars[[1]]
  }

  # some checks here, should get done regardless but mostly important for multiple param sets
  if (any(lapply(pars, length) != 3)) {
    stop("Each input list must have 3 elements - 1) file path to def file 2) def file variable 3) value")
  }

  # name some things to be helpful
  name_pars = function(x) {
    names(x) = c("Def_file", "Variable", "Value")
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

  if (n > 1) {
    # only vary the variables that are numbers
    values = unlist(lapply(pars, "[[", 3))
    values = suppressWarnings(as.numeric(values))

    #if (any(is.na(values))) {
      #cat() # idk guess doesn't matter
    #}
  
    parset_from_pctrange = function(x) {
      if (x >= 0) {
        stats::runif(n = n, min = x - (pct_range * x), max = x + (pct_range * x))
      } else if (x < 0){
        stats::runif(n = n, max = x - (pct_range * x), min = x + (pct_range * x))
      }
    } 

    value_sets = lapply(values[!is.na(values)], parset_from_pctrange)

    pars[!is.na(values)] = mapply(function(x, y) {x[[3]] = y; return(x)}, x = pars[!is.na(values)], y = value_sets, SIMPLIFY = F)

    if (any(is.na(values))) {
      pars[is.na(values)] = lapply(pars[is.na(values)], function(x) {x[[3]] = rep.int(x[[3]], n); return(x)})
    }

  }

  return(pars)

}
