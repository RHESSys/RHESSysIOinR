#' Produces ...
#'
#'
#'
#'
#'
#'
#' @export
process_input_preexisting_table <- function(parameter_change_df_by_def, parameter_method){


  # ---------------------------------------------------------------------
  # Read in parameter file
  par_table <- read.table(par_input_table_path, header = TRUE, stringsAsFactors = FALSE)

  # ---------------------------------------------------------------------
  # Separate parameters from def files

  # Def file parameters, core parameters, other variables (eg worldfile)

  # Maybe add unique identifiers to each type of parameter (eg )




}

