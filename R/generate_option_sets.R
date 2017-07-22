#' Produces data-frames for all parameter combinations and for each def file
#'
#'
#'
#' Loose ends
#' Needs to work properly when various components are NULL
#' Base station code
#'
#'
#' @export
generate_option_sets <- function(parameter_method,
                                 input_hdr_list,
                                 input_rhessys,
                                 input_preexisting_table,
                                 input_def_list,
                                 input_standard_par_list,
                                 input_dated_seq_file){

  # ---------------------------------------------------------------------
  # Process parameters

  if (is.null(input_preexisting_table) == TRUE){

    # ---------------------------------------------------------------------
    # Process def files
    # Output is a list of data-frames showing parameter combinations for each def file

    option_sets_def_par <- list()

    if (is.null(input_def_list[1]) == FALSE){
      input_def_file <- unlist(lapply(input_def_list, function(x) x[1]))
      input_def_file_unique <- unique(input_def_file)

      input_def_list_by_unique_file <- input_def_file_unique %>%
        # Generates matrix indicating which components of input_def_list are assigned to each def file
        sapply(function(y) lapply(input_def_list, function(x) x[1] == y)) %>%
        # Generates three nested lists
        lapply(seq_along(input_def_file_unique), function(x,y) input_def_list[y[,x] == TRUE], .)

      # Subset by each def file
      for (bb in seq_along(input_def_file_unique)){
        names(input_def_list_by_unique_file[[bb]]) <- lapply(input_def_list_by_unique_file[[bb]], function(x) x[[2]])
        input_def_change_par <- sapply(input_def_list_by_unique_file[[bb]], function(x) x[[3]], simplify = FALSE, USE.NAMES = TRUE) # Isolate parameter values

        option_sets_def_par[[bb]] <- make_option_set_combinations(input_list=input_def_change_par, parameter_method=parameter_method)

        # Attach group ID to option_sets_def_par
        tmp <- seq_along(option_sets_def_par[[bb]][[1]])
        option_sets_def_par[[bb]] <- bind_cols(option_sets_def_par[[bb]], group_id = tmp)
      }

      names(option_sets_def_par) <- input_def_file_unique

      lapply(input_def_file_unique, function(x,y) names(y[[x]]), option_sets_def_par)

    } else {
      option_sets_def_par <- NULL
    }

    # ---------------------------------------------------------------------
    # Process standard RHESSys parameters

    if (is.null(input_standard_par_list[1]) == FALSE){
      option_sets_standard_par <- make_option_set_combinations(input_list=input_standard_par_list, parameter_method=parameter_method)

      # Attach group ID to option_sets_standard_par
      tmp <- seq_along(option_sets_standard_par[[1]])
      option_sets_standard_par <- bind_cols(option_sets_standard_par, stan_id = tmp)

      } else {
      option_sets_standard_par <- NULL
    }

  } else {
    # ---------------------------------------------------------------------
    # Code for importing parameter sets

    process_input_preexisting_table()

  }

  # ---------------------------------------------------------------------
  # Process dated sequence file(s)

  if (is.null(input_dated_seq_list[1]) == F){
    # ****Currently not implemented****
    # Implement input_dated_seq_list code here
    option_sets_dated_seq <- NULL
  } else {
    option_sets_dated_seq <- NULL
  }

  # ---------------------------------------------------------------------
  # Process tec file(s)

  # ****Currently not implemented****
  # Code should permit the use of multiple tec files

  # ---------------------------------------------------------------------
  # Generate all-option table

  option_sets_all <- make_all_option_table(parameter_method,
                                           input_rhessys,
                                           input_hdr_list,
                                           option_sets_def_par,
                                           option_sets_standard_par,
                                           option_sets_dated_seq)

  # ---------------------------------------------------------------------
  # Make table used for generating hdr files

  group_ids <- dplyr::select(option_sets_all,ends_with("group_id"), hdr_id) # Select group id's
  option_sets_hdr <- dplyr::distinct(group_ids)                             # Reduce group_ids into distict sets
  names(option_sets_hdr) <- do.call(rbind, strsplit(names(option_sets_hdr), ":"))[,1] # Split variable names from group_id

  # ---------------------------------------------------------------------
  # Generate input table for rhessys_command

  # Generate hdr path
  tmp_path <- file.path(dirname(as.character(option_sets_all$world_file)), option_sets_all$world_hdr_prefix)
  option_sets_all$world_hdr_file <- file.path(tmp_path, paste(option_sets_all$world_hdr_prefix,"_", hdr_id,".hdr",sep=""))

  # Generate output file
  option_sets_all$output_file <- file.path(option_sets_all$output_folder,option_sets_all$output_filename)

  # Generate input_parameters
  option_sets_all$input_parameters <- sprintf("-s %f %f -sv %f %f -svalt %f %f -gw %f %f",
                                              option_sets_all$m,
                                              option_sets_all$k,
                                              option_sets_all$m_v,
                                              option_sets_all$k_v,
                                              option_sets_all$pa,
                                              option_sets_all$po,
                                              option_sets_all$gw1,
                                              option_sets_all$gw2)
  # ***Input_parameters needs to be redone generically so that any combination
  # of standard parameters can be included in model***


  # Produce dataframe for inputting into rhessys
  option_sets_rhessys <- dplyr::select(option_sets_all,
                                       rhessys_version,
                                       world_file,
                                       world_hdr_file,
                                       tec_file,
                                       flow_file,
                                       start_date,
                                       end_date,
                                       output_file,
                                       input_parameters,
                                       command_options)


  # ---------------------------------------------------------------------
  # Export option sets (this should move to generate_input_files.r?)

  # Output types
  # Everything (check)
  # Inputs for rhessys_command
  # Parameters for transferring to simulation


  # parameter_output <- option_sets_all
  #
  # if (is.null(dated_seq_data) == FALSE){
  #   parameter_output$dated_seq_data <- sapply(parameter_sets$dated_seq_data, function(x) dated_seq_data[[x]][[5]])
  # }
  # write.csv(parameter_output, paste(output_folder, output_filename, "_parameter_sets.csv", sep=""), row.names = FALSE)



  return(list(option_sets_def_par = option_sets_def_par,
              option_sets_standard_par = option_sets_standard_par,
              option_sets_dated_seq = option_sets_dated_seq,
              option_sets_all = option_sets_all,
              option_sets_hdr = option_sets_hdr,
              option_sets_rhessys = option_sets_rhessys))
}
