#' Produces ...
#'
#'
#'
#'
#'
#'
#' @export
process_input_preexisting_table <- function(input_preexisting_table,
                                            parameter_method){


  # ---------------------------------------------------------------------
  # Read in parameter file
  par_table <- read.table(file.path(input_preexisting_table), header = TRUE, stringsAsFactors = FALSE)

  # ---------------------------------------------------------------------
  # Process standard RHESSys parameters

  stan_par <- c("m", "k", "m_v", "k_v", "pa", "po", "gw1", "gw2")
  option_sets_standard_par <- dplyr::select(par_table, stan_par)

  # Attach group ID to option_sets_standard_par
  tmp <- seq_along(option_sets_standard_par[[1]])
  option_sets_standard_par <- bind_cols(option_sets_standard_par, stan_id = tmp)

  # ---------------------------------------------------------------------
  # Process def files




  # ---------------------------------------------------------------------


  return(list(option_sets_def_par=option_sets_def_par,
              option_sets_standard_par=option_sets_standard_par))
}

