#' Code for evaluating model output
#'
#' Description...
#'
#' @param sim
#' @param obs \code{obs} must have an equal number of values as \code{sim}.
#'
#' @export
evaluation <- function(sim, obs, evaluation_criteria = c("one_obj_func", "composite", "pareto"), ...){

  parameter_type <- match.arg(evaluation_criteria)

  obj_func <- gof(sim, obs, ...)
  print(obj_func)


  # Method - selection evalutation criteria
  select_evaluation_criteria(evaluation_criteria, ...)



  # Export top parameter sets

  return(obj_func_best)
}


