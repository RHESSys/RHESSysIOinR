#' Produces data-frames for all parameter combinations and for each def file
#'
#'
#'
#' Loose ends
#' Non-unique parameters
#' Needs to work properly when various components are NULL
#' Base station code
#'
#' Output: option_sets_def, option_sets_standard_par, option_sets_all
#'
#' @export
generate_option_sets <- function(parameter_method){

  # Output of this function
  # An all-options table for inputting into to model
  # Individual def-file tables for making def files.

  # ---------------------------------------------------------------------
  # Process parameters

  if (input_preexisting_table = NULL){

    # ---------------------------------------------------------------------
    # Process def files - separate def_par_input_list by def file

    option_sets_def <- list()
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

        option_sets_def[[bb]] <- compute_par_combinations(par_input_list=def_par_change_values, parameter_method=parameter_method)

      } else {
        option_sets_def <- NULL
      }

      names(option_sets_def) <- def_file_input_unique
      # names(option_sets_def) <- paste(def_file_input_unique,
    }

    # ---------------------------------------------------------------------
    # Process standard RHESSys parameters
    option_sets_standard_par <- make_option_set_combinations(input_list=input_standard_par_list, parameter_method=parameter_method)


  } else {
    # Code for importing parameter sets


    process_input_preexisting_table()

  }


  # ---------------------------------------------------------------------
  # Process dated sequence file(s)

  if (is.null(dated_seq_input_list[1]) == F){
    # Implement dated_seq_input_list code here
  }

  # ---------------------------------------------------------------------
  # Process tec file(s)

  # Currently not implemented. This code would permit for the use of different
  # tec files

  # ---------------------------------------------------------------------
  # Generate all-option table

  option_sets_all <- make_all_option_table(rhessys_version = rhessys_version, tec_file = tec_file,
                                       world_file = world_file, world_hdr_file = world_hdr_file,
                                       flow_file = flow_file, start_date = start_date,
                                       end_date = end_date, output_folder = output_folder,
                                       output_filename = output_filename, command_options = command_options,
                                       m = m, k = k, m_v = m_v, k_v = k_v,
                                       pa = pa, po = po, gw1 = gw1, gw2 = gw2,
                                       parameter_change_list = parameter_change_list,
                                       dated_seq_data = dated_seq_data,
                                       method = parameter_type)
  parameter_sets_l <- length(parameter_sets[,1])
  total_variables <- length(parameter_sets)

  # ---------------------------------------------------------------------
  # Export option sets (this should move to generate_input_files.r)

  parameter_output <- option_sets_all

  if (is.null(dated_seq_data) == FALSE){
    parameter_output$dated_seq_data <- sapply(parameter_sets$dated_seq_data, function(x) dated_seq_data[[x]][[5]])
  }
  write.csv(parameter_output, paste(output_folder, output_filename, "_parameter_sets.csv", sep=""), row.names = FALSE)



  #return("")
}
