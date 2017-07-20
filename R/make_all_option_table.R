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
                                  input_hdr_list,
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
  # Make a table from which hdr files will be derived (These are the group_id columns)

  # After dated sequences are added, change input file from all_option_par to the one produced from dated sequences
  # Actually, since stan parameters should not be included in hdr file, should reorder dated seq after def, then do this process.

  def_file_types <- purrr::discard(names(input_hdr_list), names(input_hdr_list)=="base_stations")
  base_file_types <- purrr::keep(names(input_hdr_list), names(input_hdr_list)=="base_stations")
  # Still need to separate base station file


  input_hdr <- purrr::flatten(input_hdr_list)
  for (aa in seq_along(input_hdr)){
    input_hdr[aa]
    if (input_hdr[aa] %in% names(option_sets_def_par)){    # Did a def file from the hdr input have parameter changes?
      #print("nothing to see here")
    } else {
      # Attach column to master_par_change_df for unchanging def files
      all_option_par <- cbind(all_option_par,  "placeholder_name" = rep(0, length(all_option_par[,1])))
      names(all_option_par)[names(all_option_par) == "placeholder_name"] <- paste(input_hdr[aa], ":group_id", sep="")
    }
  }

  # Make hdr table
  option_sets_hdr <- dplyr::select(all_option_par,ends_with("group_id"))

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
  return(list(option_sets_all = option_sets_all,
              option_sets_hdr = option_sets_hdr))
}

