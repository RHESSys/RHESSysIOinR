#' Produces data-frames for all parameter combinations and for each def file
#'
#'
#'
#'
#'
#' Loose ends
#' Non-unique parameters
#' Needs to work properly when various components are NULL
#' Base station code
#'
#' Output: parameter_change_df_by_def
#'
#' @export
process_input_parameters <- function(parameter_change_df_by_def, parameter_method){

  # Output of this function
  # An all-options table for inputting into to model
  # Individual def-file tables for making def files.

  # ---------------------------------------------------------------------

  # Process input


  if (par_input_table = NULL){

    # ---------------------------------------------------------------------
    # Process def files - separate def_par_input_list by def file

    parameter_change_df_by_def <- list()

    if (is.null(def_par_input_list[1]) == F){
      def_file_input_path <- unlist(lapply(def_par_input_list, function(x) x[1]))
      def_file_input_unique <- unique(def_file_input_path)
      def_file_input_par <- unlist(lapply(def_par_input_list, function(x) x[2]))

      # This one
      happy <- sapply(def_file_input_unique,function(y) lapply(def_par_input_list, function(x) x[1] == y))

      def_par_input_list[happy[,1] == TRUE]
      def_par_input_list[happy[,2] == TRUE]

      # happy2 is three nested loops
      happy2 <- lapply(seq_along(def_file_input_unique), function(x,y) def_par_input_list[y[,x] == TRUE], happy)

      # Subset by each def file
      for (bb in seq_along(def_file_input_unique)){
        happy3 <- happy2[[bb]]
        names(happy3) <- lapply(happy3, function(x) x[[2]])
        def_par_change_values <- sapply(happy3, function(x) x[[3]], simplify = FALSE, USE.NAMES = TRUE) # Isolate parameter values
        print(def_par_change_values)
        print("----")

        parameter_change_df_by_def[[bb]] <- compute_par_combinations(par_input_list=def_par_change_values, parameter_method=parameter_method)

      } else {
        parameter_change_df_by_def <- NULL
      }

      names(parameter_change_df_by_def) <- def_file_input_unique
      # names(parameter_change_df_by_def) <- paste(def_file_input_unique,
    }

    # ---------------------------------------------------------------------
    # Process core RHESSys parameters
    core_par <- compute_par_combinations(par_input_list=core_par_input_list, parameter_method=parameter_method)

    # ---------------------------------------------------------------------
    # Process dated sequence files

    if (is.null(dated_seq_input_list[1]) == F){
      dated_seq_input_list
    }

  } else {
    # Code for importing parameter sets


    process_par_input_table()

  }





  make_all_input_table()



}
