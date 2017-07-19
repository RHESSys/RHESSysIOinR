#' Create all-options table
#'
#'
#' Combines vectors of parameter inputs into data frame of parameter sets that
#' \code{run_rhessys()} cycles through.
#'
#' Each input parameter vector should be of equal length. Extra parameters beyond the
#' standard RHESSys calibrated parameters (e.g. def parameters) will need to be
#' accompanied by extra filenames for awk command. See \code{run_rhessys()} inputs.
#'
#' @export
make_all_option_table <- function(parameter_method,
                                  input_rhessys,
                                  option_sets_def_par,
                                  option_sets_standard_par,
                                  option_sets_dated_seq,
                                  ...){

  # ---------------------------------------------------------------------
  # Add def files

  # Create hybrid parameter names for def parameters (avoids potential duplicate names if multiple canopies are used)
  col_names_def <- mapply(function(x,y) paste(x, ":", names(y), sep=""),
                          x=names(option_sets_def_par),
                          y=option_sets_def_par,
                          SIMPLIFY = FALSE)

  # Attach hybrid parameter names to option sets
  option_sets_def_par_full_name <- mapply(function(x,y) setNames(x,y),
                                          x=option_sets_def_par,
                                          y=col_names_def,
                                          SIMPLIFY = FALSE)

  if (parameter_method == "all_combinations"){

    # Generate a single dataframe for def files
    tmp1 <- option_sets_def_par_full_name %>%   # Isolate the group_id for each def file
      lapply(.,function(x) dplyr::select(x,ends_with("group_id")))
    tmp2 <- expand.grid(purrr::flatten(tmp1))   # Create all combinations of def parameters
    tmp3 <- mapply(function(x,y,z) dplyr::full_join(dplyr::select(x,z),y,by=z), y=option_sets_def_par_full_name, z=names(purrr::flatten(tmp1)), MoreArgs=list(x=tmp2), SIMPLIFY = FALSE) # Rejoin the variables to the appropriate group_id
    all_option_def <- do.call(dplyr::bind_cols, tmp3)
    all_option_def <- bind_cols(all_option_def, data.frame(def_id=seq_along(all_option_def[[1]]))) # Add unique par identifier

  }

  if (parameter_method == "monte_carlo" || parameter_method == "lhc" || parameter_method == "specific_values"){

    # Generate a single dataframe for def files
    all_option_def <- do.call(bind_cols,option_sets_def_par_full_name)
    all_option_def <- bind_cols(all_option_def, data.frame(def_id=seq_along(all_option_def[[1]]))) # Add unique par identifier

  }

  # ---------------------------------------------------------------------
  # Add standard parameters

  if (parameter_method == "all_combinations"){

    # Generate a single dataframe for def files
    tmp1 <- option_sets_standard_par %>%
      dplyr::select(ends_with("stan_id"))
    tmp2 <- all_option_def %>%
      dplyr::select(ends_with("def_id"))
    tmp3 <- expand.grid(c(tmp1,tmp2))   # Create all combinations of def and standard parameters
    tmp4 <- mapply(function(x,y,z) dplyr::full_join(dplyr::select(x,z),y,by=z), y=list(option_sets_standard_par, all_option_def) , z=names(tmp3), MoreArgs=list(x=tmp3), SIMPLIFY = FALSE)  # Rejoin the variables to the appropriate group_id
    all_option_par <- do.call(dplyr::bind_cols, tmp4)
    all_option_par <- dplyr::bind_cols(all_option_par, data.frame(par_id=seq_along(all_option_par[[1]]))) # Add unique par identifier
  }

  if (parameter_method == "monte_carlo" || parameter_method == "lhc" || parameter_method == "specific_values"){

    # Generate a single dataframe
    all_option_par <- dplyr::bind_cols(all_option_def, option_sets_standard_par)
    all_option_par <- dplyr::bind_cols(all_option_par, data.frame(par_id=seq_along(all_option_par[[1]]))) # Add unique par identifier
  }

  # ---------------------------------------------------------------------
  # Add dated sequences

  # **** Pending ****

  # ---------------------------------------------------------------------
  # Add tec files

  # **** Pending ****

  # ---------------------------------------------------------------------
  # Add rhessys inputs

  # Create expanded grid for rhessys_input
  tmp1 <- all_option_par %>%
    dplyr::select(ends_with("par_id"))
  tmp2 <- expand.grid(c(input_rhessys, tmp1))
  option_sets_all <- dplyr::full_join(tmp2,all_option_par,by="par_id")
  option_sets_all <- dplyr::bind_cols(option_sets_all, data.frame(all_id=seq_along(option_sets_all[[1]]))) # Add unique all-option identifier

  # ---------------------------------------------------------------------
  return(option_sets_all)
}

