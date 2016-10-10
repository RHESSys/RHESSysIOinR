#' Code for evaluating model output
#'
#' Description...
#'
#' Future iterations will account for multiple parameters and/or multiple criteria.
#' Also, future iterations should will need to combine outputs from separate simulations (from parallelization)
#' into a single evaluated simulation.
#'
#' @param sim_variables
#' @param obs_variables \code{obs} must have an equal number of values as \code{sim}.
#'
#' @export
evaluation <- function(sim_variables, obs_variables, obj_function_selection, input_folder, output_folder, ...,
                       evaluation_type = c("single_objective_function", "composite", "pareto")){

  parameter_type <- match.arg(evaluation_criteria)

  sim = lapply(as.data.frame(read.csv(paste(input_folder,"/allsim/",sim_variables, sep=""))))
  obs = lapply(as.data.frame(read.csv(paste(input_folder,"/",obs_variables,sep=""))))

  if (length(sim) > 1 & length(obs) == 0){
    # repeat obs variable so that it equals number of simulation variables
  }

  # Current implentation cannot handle multiple variables
  obj_function_all <- gof(sim, obs, ...)
  print(obj_function_all)

  # Selection evalutation criteria
  top_ps <- select_parameter_sets(obj_function_all, obj_function_selection = obj_function_selection,
                                  method = evaluation_type, ...)

  # Export top parameter sets
  write.csv(top_ps, paste(output_folder, "/", output_filename, "_top_ps.csv", sep=""))

  return(top_ps)
}


