#' IOin_def_pars_simple
#'
#' The definition file parameters to modify. This input function only sets parameters to a single static value.
#' @param ... Any number of lists, each containing 3 elements in format: list("<def file>", "<parameter name>", <value>)
#'
#' @author Will Burke
#'
#' @export

IOin_def_pars_simple = function(...) {

  pars = list(...)

  return(pars)

}
