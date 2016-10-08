#' Code for evaluating model output
#'
#' Description...
#'
#'
#' Needs library hydroGOF
#'
#' @export
evaluation <- function(sim, obs, ...){

  # Test if observed data is single column
  if (length(obs) == 1 & length(sim) > 1){
    # Make obs same width
  }

  obj_func <- gof(sim, obs, ...)
  print(obj_func)




  # Method - selection criteria


  # Single objective function


  # Select composite


  # Select pareto


  # Export top parameter sets


  return(obj_func_best)
}


