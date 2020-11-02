#' IOin_output_vars
#'
#' @param ... Any number of two element character vectors or dataframes, each specifying the output file using "pd", "pdg", "cd", "cdg"
#' style format in the first item/column and the variable of interest in the second item/column.
#'
#' @author Will Burke
#'
#' @export

# setting it up so you can either input a single df, seperate dfs that get combined, or individual vectors that get combined

IOin_output_vars = function(...) {

  pars = list(...)

  if (length(pars) > 1 && is.data.frame(pars[[1]])) {
    pars = do.call(rbind, pars)
  } else if (length(pars) > 1) {
    pars = as.data.frame(do.call(rbind, pars))
  } else {
    pars = as.data.frame(pars)
  }

  return(pars)

}
