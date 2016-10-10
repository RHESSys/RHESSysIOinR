#' Select approach to evaluate objective function output
#'
#' Description...
#'
#'
#' @export
select_parameter_sets <- function(obj_function_all, obj_function_selection, method, num_ps=1, ...){

  if (method == "single_objective_function"){
    o <- order(obj_func)
    top_ps <- obj_func[o<=num_ps]

  }


  if (method == composite){

  }

  if (method == pareto){

  }

  return(top_ps)
}
