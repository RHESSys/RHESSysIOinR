#' Produces a hdr file in RHESSys
#'
#' The hdr file in RHESSys determines which definition and climate files are to be used for a given run.
#'
#' This function produces hdr file and associated files that are called by hdr file; def files and clim.base files.
#'
#'
#'
#' @export
process_hdr_def_file <- function(hdr_input_list, def_par_input_list, dated_seq_input_list, parameter_method){

  # Produce par_change_master_df according to appropriate method
  # ---------------------------------------------------------------------
  if (parameter_method == "all_combinations"){

    # Process combinations
    if (is.null(dated_seq_input_list[1]) == F){
      dated_seq_change_values = list(dated_seq_input_list = seq_along(dated_seq_input_list))
    } else {
      dated_seq_change_values = NULL
    }

    if (is.null(def_par_input_list[1]) == F){
      names(def_par_input_list) <- lapply(def_par_input_list, function(x) x[[3]]) # Assigning parameter names to list
      def_par_change_values <- sapply(def_par_input_list, function(x) x[[1]], simplify = FALSE, USE.NAMES = TRUE) # Isolate parameter values
    } else {
      def_par_change_values = NULL
    }

    master_par_change_df <- expand.grid(c(dated_seq_change_values, def_par_change_values))

    # Make unique column names

   }

  # ---------------------------------------------------------------------
  if (parameter_method == "monte_carlo"){}



  # ---------------------------------------------------------------------
  if (parameter_method == "lhc"){   # latin hypercube

  }

  # ---------------------------------------------------------------------
  if (parameter_method == "specific_values"){

  }


  # ---------------------------------------------------------------------
  if (parameter_method == "data_frame"){

  }

  # Output needs to be table of various def parameter combinations


  # ---------------------------------------------------------------------
  #

  def_file_types <- purrr::discard(names(hdr_input_list), names(hdr_input_list)=="base_stations")
  base_file_types <- purrr::keep(names(hdr_input_list), names(hdr_input_list)=="base_stations")

  # Cycle through each type of def file
  for (aa in seq_along(def_file_types)){

    # Process each def file input type
    def_file_input_path <- unlist(lapply(def_par_input_list, function(x) x[2]))
    def_file_input_unique <- unique(unlist(lapply(def_par_input_list, function(x) x[2])))

    hdr_input_selected <- hdr_input_list[[def_file_types[aa]]]

    # Step through def filea with more than one type (if present)
    for (bb in seq_along(hdr_input_selected)){

      # Check to see if def file type requires changes in parameter values.
      if (hdr_input_selected[bb] %in% def_file_input_unique){

        # Select columns (parameters) that are associated with the selected def file
        def_par_input_names <- unlist(lapply(def_par_input_list, function(x) x[3]))
        def_par_selected <- def_par_input_names[def_file_input_path==hdr_input_selected[bb]]
        dots <- lapply(def_par_selected, as.symbol)

        # Establish IDs to identify def files with different parameter sets
        par_change_by_def_file <- dplyr::select_(master_par_change_df, .dots=dots)
        par_change_by_def_file$group_id <- group_indices_(par_change_by_def_file, .dots=dots)

        # Attach group ID to master_par_change_df
        master_par_change_df = cbind(master_par_change_df,  "placeholder_name" = par_change_by_def_file$group_id)
        names(master_par_change_df)[names(master_par_change_df) == "placeholder_name"] <- hdr_input_selected[bb]


        # -------------
        # Generate def files

        # Determine the number of unique parameter sets (group #'s) for each def file
        par_change_by_def_file_condensed <- par_change_by_def_file %>%
          group_by(group_id) %>%
          dplyr::filter(row_number()==1)

        # Step through each unique parameter set and make def file
        for (cc in seq_along(par_change_by_def_file_condensed$group_id)){

           change_def_file(def_file = hdr_input_selected[bb],
                           par_sets = dplyr::select(ungroup(par_change_by_def_file_condensed)[cc,], -group_id),
                           file_name_ext = as.character(dplyr::select(ungroup(par_change_by_def_file_condensed)[cc,], group_id)))
        }

      } else {

        # Attach column to master_par_change_df for unchanging def files
        master_par_change_df = cbind(master_par_change_df,  "placeholder_name" = rep(0, length(master_par_change_df[,1])))
        names(master_par_change_df)[names(master_par_change_df) == "placeholder_name"] <- hdr_input_selected[bb]
      }

    } # End for bb loop

  } # End for aa loop




  # Cycle through each base station file
  for (xx in seq_along(base_file_types)){
    print("nothing yet")
  }

  # create hdr file here
  # Use master_par_change_df and create correct output for each variable

  names(hdr_input_list)

  for (yy in seq_along(names(hdr_input_list))){
    names(hdr_input_list)

    dplyr::select(master_par_change_df, matches(hdr_input_list[[yy]]))

  }
}



# ----












  # # ------
  #
  #   # Function for assembling inital line for each hdr input
  # def_file_df <- function(def, default_file = "default_file"){
  #   output <- lapply(def, function(x) c(x, default_file)) %>%
  #   do.call(rbind, .)
  #   colnames(output) <- c("c1", "c2")
  #   return(output)
  # }
  #
  # def_file_desc <- c("basin_default_file","hillslope_default_file","zone_default_file",
  #                   "soil_default_file","stratum_default_file",
  #                   "fire_default_file","base_stations_file")
  #
  # # Assemble hdr file
  # hdr_df_tmp <- data.frame(c1 = length(hdr_list$basin_def), c2 = "num_basin_default_files", stringsAsFactors=FALSE) %>%
  #   rbind(., def_file_df(hdr_list$basin_def, "basin_default_file")) %>%
  #
  #   rbind(., c(length(hdr_list$hill_def), "num_hillslope_default_files")) %>%
  #   rbind(., def_file_df(hdr_list$hill_def, "hillslope_default_file")) %>%
  #
  #   rbind(., c(length(hdr_list$zone_def), "num_zone_default_files")) %>%
  #   rbind(., def_file_df(hdr_list$zone_def, "zone_default_file")) %>%
  #
  #   rbind(., c(length(hdr_list$soil_def), "num_soil_default_files")) %>%
  #   rbind(., def_file_df(hdr_list$soil_def, "soil_default_file")) %>%
  #
  #   rbind(., c(length(hdr_list$landuse_def), "num_landuse_default_files")) %>%
  #   rbind(., def_file_df(hdr_list$landuse_def, "landuse_default_file")) %>%
  #
  #   rbind(., c(length(hdr_list$stratum_def), "num_stratum_default_files")) %>%
  #   rbind(., def_file_df(hdr_list$stratum_def, "stratum_default_file")) %>%
  #
  #   # Add fire option here
  #   rbind(., c(length(hdr_list$fire_def), "num_fire_default_files")) %>%
  #   rbind(., def_file_df(hdr_list$fire_def, "fire_default_file")) %>%
  #
  #   rbind(., c(length(hdr_list$base_stations), "num_base_stations")) %>%
  #   rbind(., def_file_df(hdr_list$base_stations, "base_stations_file"))
  # #print(hdr_df)
  #
  # write.table(hdr_df, file = "happy", col.names = FALSE, row.names=FALSE, quote = FALSE)
  #








