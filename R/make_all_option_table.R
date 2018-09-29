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
                                  option_sets_dated_seq){

  # ---------------------------------------------------------------------
  # Add def files

  all_option_def = NULL # overwriten if there are def pars
  if(!is.null(option_sets_def_par)){ # only run if there are def file pars to include

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
        lapply(.,function(x) dplyr::select(x,dplyr::ends_with("group_id")))
      tmp2 <- expand.grid(purrr::flatten(tmp1))   # Create all combinations of def parameters
      tmp3 <- mapply(function(x,y,z) dplyr::full_join(dplyr::select(x,z),y,by=z), y=option_sets_def_par_full_name, z=names(purrr::flatten(tmp1)), MoreArgs=list(x=tmp2), SIMPLIFY = FALSE) # Rejoin the variables to the appropriate group_id
      all_option_def <- do.call(dplyr::bind_cols, tmp3)
      all_option_def <- dplyr::bind_cols(all_option_def, data.frame(def_id=seq_along(all_option_def[[1]]))) # Add unique par identifier
    }

    if (parameter_method == "monte_carlo" || parameter_method == "lhc" || parameter_method == "specific_values"){

      # Generate a single dataframe for def files
      all_option_def <- do.call(dplyr::bind_cols,option_sets_def_par_full_name)
      all_option_def <- dplyr::bind_cols(all_option_def, data.frame(def_id=seq_along(all_option_def[[1]]))) # Add unique par identifier
    }

    # Add columns for unchanging def files (These columns are needed when making hdr files)
    input_hdr_list_def <- purrr::flatten(purrr::discard(input_hdr_list, names(input_hdr_list)=="base_stations"))
    for (aa in seq_along(input_hdr_list_def)){
      if (input_hdr_list_def[aa] %in% names(option_sets_def_par)){    # Did a def file from the hdr input have parameter changes?
        # Do nothing!
      } else {
        # Attach column to all_option_def for unchanging def files
        # reaplceed length of rep, was: length(all_option_def[,1])
        all_option_def <- cbind(all_option_def,  "placeholder_name" = rep(0, length(option_sets_standard_par[,1])))
        colnames(all_option_def)[colnames(all_option_def) == "placeholder_name"] <- paste(input_hdr_list_def[aa], ":group_id", sep="")
      }
    }
  }

  # ---------------------------------------------------------------------
  # Add standard parameters

  if (parameter_method == "all_combinations"){

    # Generate a single dataframe for def files
    tmp1 <- option_sets_standard_par %>%
      dplyr::select(dplyr::ends_with("stan_id"))
    tmp2 <- all_option_def %>%
      dplyr::select(dplyr::ends_with("def_id"))
    tmp3 <- expand.grid(c(tmp1,tmp2))   # Create all combinations of def and standard parameters
    tmp4 <- mapply(function(x,y,z) dplyr::full_join(dplyr::select(x,z),y,by=z), y=list(option_sets_standard_par, all_option_def) , z=names(tmp3), MoreArgs=list(x=tmp3), SIMPLIFY = FALSE)  # Rejoin the variables to the appropriate group_id
    all_option_par <- do.call(dplyr::bind_cols, tmp4)
    all_option_par <- dplyr::bind_cols(all_option_par, data.frame(par_id=seq_along(all_option_par[[1]]))) # Add unique par identifier
  }

  if (parameter_method == "monte_carlo" || parameter_method == "lhc" || parameter_method == "specific_values"){

    # Generate a single dataframe
    all_option_par <- dplyr::bind_cols(option_sets_standard_par,all_option_def)
    all_option_par <- dplyr::bind_cols(all_option_par, data.frame(par_id=seq_along(all_option_par[[1]]))) # Add unique par identifier
  }

  # ---------------------------------------------------------------------
  # Add dated sequences

  tmp1 <- all_option_par %>%
    dplyr::select(dplyr::ends_with("par_id"))
  tmp2 <- expand.grid(c(option_sets_dated_seq, tmp1))
  all_option_dated_seq <- dplyr::full_join(tmp2,all_option_par,by="par_id")
  all_option_dated_seq <- dplyr::bind_cols(all_option_dated_seq, data.frame(par_dated_id=seq_along(all_option_dated_seq[[1]]))) # Add unique all-option identifier


  # ---------------------------------------------------------------------
  # Add tec files options

  # **** Pending ****
  all_option_tec <- all_option_dated_seq

  # ---------------------------------------------------------------------
  # Add rhessys inputs

  # Create expanded grid for rhessys_input
  tmp1 <- all_option_tec %>%
    dplyr::select(dplyr::ends_with("par_dated_id"))
  tmp2 <- expand.grid(c(input_rhessys, tmp1))
  option_sets_all <- dplyr::full_join(tmp2,all_option_tec,by="par_dated_id")
  option_sets_all <- dplyr::bind_cols(option_sets_all, data.frame(all_id=seq_along(option_sets_all[[1]]))) # Add unique all-option identifier

  # ---------------------------------------------------------------------
  # Add hdr_id to option_sets_all

  group_ids <- dplyr::select(option_sets_all,dplyr::ends_with("group_id"),dated_id)  # Select group id's
  dots <- lapply(names(group_ids), as.symbol)
  hdr_id <- dplyr::group_indices_(group_ids, .dots=dots)
  option_sets_all <- dplyr::bind_cols(option_sets_all, data.frame(hdr_id=hdr_id)) # Add unique hdr identifier

  # ---------------------------------------------------------------------

  return(option_sets_all)
}

