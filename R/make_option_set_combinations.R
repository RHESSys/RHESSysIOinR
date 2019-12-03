#' Generates a dataframe of parameter set combinations
#'
#' Converts a list of parameter inputs (ranges, values, etc.) to a data frame
#' containing the specified number of unique parameter set combinations.
#'
#' @param input_list List of parameters values to be processed. Name of
#'   parameter should be provided as name of each list component.
#' @param parameter_method Parameter designating the method to use.
#'
#' Details of all_combinations method...
#'
#' Details of monte-carlo method...
#'
#' Details of latin hypercube method...
#'
#' Details of specific values method... This option is analogous to importing
#' a dataframe of parameter sets, but specifies values by parameter in a list.
#'
#' @export
make_option_set_combinations <- function(input_list,
                                         parameter_method){

  # ---------------------------------------------------------------------
  if (parameter_method == "all_combinations"){

    # Create parameter data frame
    out <- expand.grid(input_list)
  }

  # ---------------------------------------------------------------------
  if (parameter_method == "monte_carlo"){

    # Data checks
    k <- length(input_list)
    stopifnot(sapply(seq_len(k), function(x,y) length(y[[x]])==3, y=input_list)) # Check that there are 3 values for each parameter (presumably a min, max and n)
    stopifnot(sapply(seq_len(k), function(x,y) y[[x]][3]==y[[1]][3], y=input_list)) # Check that total n is equal for all parameters.

    # Inputs
    min_par <- sapply(seq_len(k), function(x,y) y[[x]][1], y=input_list)
    max_par <- sapply(seq_len(k), function(x,y) y[[x]][2], y=input_list)
    n <- input_list[[1]][3]

    # Create parameter data frame
    out <- mapply(function(x, y, z) runif(min=x, max=y, n=z), x=min_par, y=max_par, MoreArgs = list(z=n))
    out <- matrix(out,nrow=n)
    out <- as.data.frame(out)
    colnames(out) <- names(input_list)
  }

  # ---------------------------------------------------------------------
  if (parameter_method == "lhc"){   # latin hypercube

    # Data checks
    k <- length(input_list)
    stopifnot(sapply(seq_len(k), function(x,y) length(y[[x]])==3, y=input_list)) # Check that there are 3 values for each parameter (presumably a min, max and n)
    stopifnot(sapply(seq_len(k), function(x,y) y[[x]][3]==y[[1]][3], y=input_list)) # Check that total n is equal for all parameters.

    # Inputs for Latin Hypercube
    min_par <- sapply(seq_len(k), function(x,y) y[[x]][1], y=input_list)
    max_par <- sapply(seq_len(k), function(x,y) y[[x]][2], y=input_list)
    n <- input_list[[1]][3]

    # Transform a Latin hypercube
    grid <- lhs::randomLHS(n, k) # Generate generic hypercube
    out <- mapply(function(w, x, y, z) qunif(w[,x], min=y, max=z), x=seq_len(k), y=min_par, z=max_par, MoreArgs = list(w=grid))
    out <- matrix(out,nrow=n)
    out <- as.data.frame(out)
    colnames(out) <- names(input_list)

    # See KScorrect package, qlunif, for potentially implementing uniform
    # log-normal distribution. Would need new input_def_file parameter
    # specifying normal or log normal, then have ifelse statement on line 64
  }

  # ---------------------------------------------------------------------
  if (parameter_method == "exact_values"){

    # Data checks
    k <- length(input_list)
    stopifnot(sapply(seq_len(k), function(x,y) length(y[[x]])==length(y[[1]]), y=input_list)) # Check that total n is equal for all parameters.

    # Create parameter data frame
    out <- as.data.frame(do.call(cbind, input_list))
  }

  # ---------------------------------------------------------------------

 return(out)
}

