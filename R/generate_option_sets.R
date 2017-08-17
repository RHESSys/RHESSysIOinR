#' Produces data-frames for all parameter combinations and for each def file
#'
#'
#'
#'
#' @export
generate_option_sets <- function(parameter_method,
                                 input_hdr_list,
                                 input_rhessys,
                                 input_preexisting_table,
                                 input_def_list,
                                 input_standard_par_list,
                                 input_dated_seq_list){

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

    tmp <- process_input_preexisting_table(input_preexisting_table=input_preexisting_table)

    option_sets_def_par <- tmp$option_sets_def_par
    option_sets_standard_par <- tmp$option_sets_standard_par
  }

  # ---------------------------------------------------------------------
  # Process dated sequence file(s)

  if (is.null(input_dated_seq_list[1]) == F){
    # Attach group ID to option_sets_dated_seq
    option_sets_dated_seq <- data.frame(dated_id = seq_along(input_dated_seq_list))

  } else {
    option_sets_dated_seq <- data.frame(dated_id = 0)
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

  group_ids <- dplyr::select(option_sets_all,ends_with("group_id"), dated_id, hdr_id) # Select group id's
  option_sets_hdr <- dplyr::distinct(group_ids)                             # Reduce group_ids into distict sets
  names(option_sets_hdr) <- do.call(rbind, strsplit(names(option_sets_hdr), ":"))[,1] # Split variable names from group_id

  # ---------------------------------------------------------------------
  # Generate table for rhessys_command

  # Generate hdr path
  tmp_path <- file.path(dirname(as.character(option_sets_all$world_file)), option_sets_all$world_hdr_prefix)
  option_sets_all$world_hdr_file <- file.path(tmp_path, paste(option_sets_all$world_hdr_prefix,"_", option_sets_all$hdr_id,".hdr",sep=""))

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
  # Make table (option_sets_par) for exporting and use in subsequent simulations

  # Isolate variable names from option_sets_def_par
  tmp1 <- mapply(function(x,y) paste(x, ":", names(y), sep=""),
                          x=names(option_sets_def_par),
                          y=option_sets_def_par,
                          SIMPLIFY = FALSE)
  tmp2 <- do.call(c, flatten(tmp1))
  remove <- grep("group_id", tmp2)
  names_def_par <- tmp2[-remove]

  # Isolate variable names from option_sets_standard_par
  names_standard_par <- option_sets_standard_par %>%
    names() %>%
    `[`(. != "stan_id")

  option_sets_par <- dplyr::select(option_sets_all,
                                   names_def_par,
                                   names_standard_par,
                                   all_id)

  # ---------------------------------------------------------------------

  return(list(option_sets_def_par = option_sets_def_par,
              option_sets_standard_par = option_sets_standard_par,
              option_sets_par = option_sets_par,
              option_sets_dated_seq = option_sets_dated_seq,
              option_sets_all = option_sets_all,
              option_sets_hdr = option_sets_hdr,
              option_sets_rhessys = option_sets_rhessys))
}

