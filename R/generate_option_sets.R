#' Produces data-frames with differnt combinations of parameters and inputs
#'
#' \code{generate_option_sets} is used to produce dataframes with different
#' combinations of parameters and inputs which are used in other parts of
#' RHESSysIOinR or are outputted as csv's.
#'
#' @return \code{option_sets_def_par} is a dataframe containing changed def file
#'   parameters.
#'
#'   \code{option_sets_standard_par} is a dataframe containing standard
#'   parameters.
#'
#'   \code{option_sets_par} is a dataframe containing both changed def file and
#'   standard parameter inputs. This dataframe is exported as
#'   \code{output_name_parameter_sets.csv} and can then be reimported (following
#'   optional filtering) under input_preexisting_table.
#'
#'   \code{option_sets_dated_seq} is a dataframe containing inputs for dated
#'   sequences.
#'
#'   \code{option_sets_all} is a dataframe containing all inputs for a given
#'   run.
#'
#'   \code{option_sets_hdr} is a dataframe containing inputs for hdr (header)
#'   files.
#'
#'   \code{option_sets_rhessys} is a dataframe containing inputs for
#'   rhessys_command.
#'
#' @section Details:
#'
#'   Each output dataframe has a unique identifier (e.g. all_id, hdr_id,
#'   group_id for def files). These identifiers, and in particular all_id, are
#'   used identify the same runs across outputs. Unique identifiers increase
#'   sequentially starting at 1 when they are applicable to a batch of runs.
#'   When a component of the model is not being used (for example, no dated
#'   sequence is needed for a run), the code will often generate a 'dummy'
#'   dataframe with a value of 0 for the unique identifier. A value of 0 is a
#'   placeholder and the code knows to ignore values of 0.
#'   \code{option_sets_all} will also generate a column for each unique
#'   identifier and populate it with 0. The only component of the code in this
#'   function that acts kinda different is the generation of the def file unique
#'   id's. No dummy dataframe is generated when def file parameters are not
#'   being altered. Instead, the output dataframe, \code{option_sets_def_par},
#'   is NULLed. Unique def file identifiers are still produced in
#'   \code{option_sets_all} and this file can be used to select unique def file
#'   group_ids's, as done in section 'Make table used for generating hdr files'.
#'   However, for consistency with the rest of code, it may be benefical for
#'   code to generate a dummy dataframe in the future .
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

  if (is.null(input_preexisting_table)) {

    # ---------------------------------------------------------------------
    # Process def files
    # Output is a list of data-frames showing parameter combinations for each def file

    option_sets_def_par <- list()

    if (!is.null(input_def_list[1])) {
      input_def_file <- unlist(lapply(input_def_list, function(x) x[1]))
      input_def_file_unique <- unique(input_def_file)

      # Generates matrix indicating which components of input_def_list are assigned to each def file
      # Generates three nested lists
      #input_def_list_by_unique_file <- input_def_file_unique %>% sapply(function(y) lapply(input_def_list, function(x) x[1] == y)) %>% lapply(seq_along(input_def_file_unique), function(x,y) input_def_list[y[,x] == TRUE], .)
      input_def_tmp <- sapply(input_def_file_unique, function(y) lapply(input_def_list, function(x) x[1] == y))
      input_def_list_by_unique_file = lapply(seq_along(input_def_file_unique), function(x,y) input_def_list[y[,x] == TRUE], input_def_tmp)

      # Subset by each def file
      for (bb in seq_along(input_def_file_unique)) {
        names(input_def_list_by_unique_file[[bb]]) <- lapply(input_def_list_by_unique_file[[bb]], function(x) x[[2]])
        input_def_change_par <- sapply(input_def_list_by_unique_file[[bb]], function(x) x[[3]], simplify = FALSE, USE.NAMES = TRUE) # Isolate parameter values

        option_sets_def_par[[bb]] <- make_option_set_combinations(input_list = input_def_change_par, parameter_method = parameter_method)

        # Attach group ID to option_sets_def_par
        tmp <- seq_along(option_sets_def_par[[bb]][[1]])
        option_sets_def_par[[bb]] <- dplyr::bind_cols(option_sets_def_par[[bb]], data.frame(group_id = tmp))
      }

      names(option_sets_def_par) <- input_def_file_unique

      lapply(input_def_file_unique, function(x,y) names(y[[x]]), option_sets_def_par) # IdK what this is doing... - Will

    } else {
      option_sets_def_par <- NULL
      # This section needs to change to a dummy data.frame with 0's. Could
      # possibly move code from make_all_option_table that makes the unchanging
      # def files to here. I would then have to generate an new def file whether
      # parameters change or not
    }

    # ---------------------------------------------------------------------
    # Process standard RHESSys parameters

    if (!is.null(input_standard_par_list[1])) {
      # Scenario where there are standard parameters to process
      option_sets_standard_par <- make_option_set_combinations(input_list = input_standard_par_list, parameter_method = parameter_method)

      # Attach group ID to option_sets_standard_par
      tmp <- seq_along(option_sets_standard_par[[1]])
      option_sets_standard_par <- dplyr::bind_cols(option_sets_standard_par, data.frame(stan_id = tmp))

    } else {
      # Scenario where there are no standard parameters to process
      if (!is.null(option_sets_def_par)) {
        # If there are def parameters processed, then a option_sets_standard_par dataframe consisting of only the stan_id is made of the same length. The values of 0 represent dummy values.
        option_sets_standard_par <- data.frame(stan_id = rep(0, nrow(option_sets_def_par[[1]])))
      } else {
        # If there are no def parameters processed, then a option_sets_standard_par dataframe consisting of only the stan_id of length 1 is made. The value of 0 representa a dummy value.
        option_sets_standard_par <- data.frame(stan_id = 0)
      }
    }

  } else {
    # ---------------------------------------------------------------------
    # Code for importing parameter sets

    tmp <- process_input_preexisting_table(input_preexisting_table = input_preexisting_table)

    option_sets_def_par <- tmp$option_sets_def_par
    option_sets_standard_par <- tmp$option_sets_standard_par
  }

  # ---------------------------------------------------------------------
  # Process dated sequence file(s)

  if (!is.null(input_dated_seq_list[1])) {
    # Attach group ID to option_sets_dated_seq
    option_sets_dated_seq <- data.frame(dated_id = seq_along(input_dated_seq_list))

  } else {
    option_sets_dated_seq <- data.frame(dated_id = 0)
  }

  # ---------------------------------------------------------------------
  # Process tecfile(s)

  # ***** Not currently implemented *****
  # This is a placeholder for code that would allow more than one tecfile to be
  # used over a set of runs. Currently, only a single tecfile can be used for
  # all runs. However, code could be developed to associate different tecfiles
  # for different runs. This could be useful if one needed to systematically
  # change the date of redefine worlds across runs, for example.


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

  group_ids <- dplyr::select(option_sets_all, dplyr::ends_with("group_id"), dated_id, hdr_id) # Select group id's
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
  if (!is.null(input_standard_par_list)) {
    option_sets_all$input_parameters <- sprintf("-s %f %f -sv %f %f -svalt %f %f -gw %f %f",
                                                option_sets_all$m,
                                                option_sets_all$k,
                                                option_sets_all$m_v,
                                                option_sets_all$k_v,
                                                option_sets_all$pa,
                                                option_sets_all$po,
                                                option_sets_all$gw1,
                                                option_sets_all$gw2)
  } else {
    option_sets_all$input_parameters = NA
  }

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

  if (is.null(input_standard_par_list)) {
    option_sets_rhessys$input_parameters = NULL
  }

  # ---------------------------------------------------------------------
  # Make table (option_sets_par) for exporting and use in subsequent simulations

  # Isolate variable names from option_sets_def_par. If option_sets_def_par doesn't exist, make names_def_par NULL.
  if (is.null(option_sets_def_par) == FALSE){
    tmp1 <- mapply(function(x,y) paste(x, ":", names(y), sep=""),
                   x=names(option_sets_def_par),
                   y=option_sets_def_par,
                   SIMPLIFY = FALSE)
    tmp2 <- do.call(c, purrr::flatten(tmp1))
    remove <- grep("group_id", tmp2)
    names_def_par <- tmp2[-remove]
  } else {
    names_def_par=NULL
  }

  # Isolate variable names from option_sets_standard_par.  If option_sets_standard_par doesn't exist, make names_def_par NULL.
  if (ncol(option_sets_standard_par) > 1){
    # If standard parameters are processed, then option_sets_standard_par will be greater than 1 column.
    #names_standard_par <- option_sets_standard_par magrittr::%>% names() magrittr::%>%  `[`(. != "stan_id")
    names_standard_par <-  names(option_sets_standard_par[names(option_sets_standard_par) != "stan_id"])
  } else {
    names_standard_par=NULL
  }

  # Make a table of parameters along with all_id identifier.
  option_sets_par <- dplyr::select(option_sets_all,names_def_par,names_standard_par, all_id)

  # ---------------------------------------------------------------------

  return(list(option_sets_def_par = option_sets_def_par,
              option_sets_standard_par = option_sets_standard_par,
              option_sets_par = option_sets_par,
              option_sets_dated_seq = option_sets_dated_seq,
              option_sets_all = option_sets_all,
              option_sets_hdr = option_sets_hdr,
              option_sets_rhessys = option_sets_rhessys))
}

