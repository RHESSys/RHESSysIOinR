#' Processes parameters imported using a data frame.
#'
#' This function is as alternative to developing parameter sets using
#' monte-carlo, via latin hypercube, etc. Generally, this function will import
#' selected parameter sets that were exported from previous RHESSys simulations
#'
#' @param input_preexisting_table path and file of table to be inputted. Columns
#'   in table represent parameters, rows indicate different parameter sets.
#'   Standard parameters include ... Parameters to be changed in def files must
#'   include as a name the path and file of def file, a ":", and the name of the
#'   parameter.
#'
#'
#' @export
process_input_preexisting_table <- function(input_preexisting_table){


  # ---------------------------------------------------------------------
  # Read in parameter file
  par_table <- read_csv(file.path(input_preexisting_table), col_names = TRUE)

  # ---------------------------------------------------------------------
  # Process standard RHESSys parameters

  stan_par <- c("m", "k", "m_v", "k_v", "pa", "po", "gw1", "gw2")
  option_sets_standard_par <- dplyr::select(par_table, stan_par)

  # Attach group ID to option_sets_standard_par
  tmp <- seq_along(option_sets_standard_par[[1]])
  option_sets_standard_par <- bind_cols(option_sets_standard_par, stan_id = tmp)

  # ---------------------------------------------------------------------
  # Process def files

  # Isolate def parmaters
  def_file_par <- par_table %>%
    dplyr::select(-one_of(stan_par)) %>%
    dplyr::select(-all_id)

  # Split def file names and rename def_file_par
  def_file_par_name_split <- strsplit(names(def_file_par), ":")
  def_par_name <- sapply(seq_along(def_file_par_name_split), function(x,y) y[[x]][2], y=def_file_par_name_split)
  names(def_file_par) <- def_par_name

  # Find unique def files
  def_files_name <- sapply(seq_along(def_file_par_name_split), function(x,y) y[[x]][1], y=def_file_par_name_split)
  def_files <- unique(def_files_name)

  option_sets_def_par <- list()
  for (aa in seq_along(def_files)){
    option_sets_def_par[[aa]] <- def_file_par[def_files_name==def_files[aa]]

    # Potential to collapse data frame (and the number of def files produced) if
    # it has redundant parameter sets

    # Attach group ID to option_sets_def_par
    tmp <- seq_along(option_sets_def_par[[aa]][[1]])
    option_sets_def_par[[aa]] <- bind_cols(option_sets_def_par[[aa]], group_id = tmp)
  }

  names(option_sets_def_par) <- def_files

  # ---------------------------------------------------------------------

  return(list(option_sets_def_par=option_sets_def_par,
              option_sets_standard_par=option_sets_standard_par))
}

