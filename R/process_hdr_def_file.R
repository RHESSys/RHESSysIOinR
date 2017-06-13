#' Produces a hdr file in RHESSys
#'
#' The hdr file in RHESSys determines which definition and climate files are to be used for a given run.
#'
#' This function produces hdr file and associated files that are called by hdr file; def files and clim.base files.
#'
#'
#'
#' @export
create_hdr_file <- function(hdr_input_list, core_par_input_list, def_par_input_list, dated_seq_input_list){

  # Produce par_change_master_df according to appropriate method
  # ---------------------------------------------------------------------
  if (method == "all_combinations"){

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
  if (method == "monte_carlo"){}



  # ---------------------------------------------------------------------
  if (method == "lhc"){   # latin hypercube

  }

  # ---------------------------------------------------------------------
  if (method == "specific_values"){

  }


  # ---------------------------------------------------------------------
  if (method == "data_frame"){

  }

  # Output needs to be table of various def parameter combinations


  # ---------------------------------------------------------------------
  #


  def_file_types <- c("basin_def", "hillslope_def", "zone_def",
                     "soil_def", "landuse_def", "stratum_def",
                     "fire_def")

  base_file_types <- c("base_stations_file")

  # Cycle through each type of def file types
  for (aa in seq_along(def_file_types)){
    hdr_input_selected <- hdr_input_list[[def_file_types[aa]]]

    # Check if reference to def file type exists in hdr_input_list
    if (is.null(hdr_input_selected) == TRUE){
      print(paste("These is no", def_file_types[aa], "hdr file", sep=""))
    } else {

      # Process each def file input type
      def_file_input_path <- unlist(lapply(def_par_input_list, function(x) x[2]))
      def_file_input_unique <- unique(unlist(lapply(def_par_input_list, function(x) x[2])))

      # Step through multiple def file types (if present)
      for (bb in hdr_input_selected){

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
          hdr_input_selected[bb] <- par_change_by_def_file$group_id
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


            make_def_files(par_change_by_def_file_condensed)

          }
        }

        # print out lines of hdr file

      } # End for bb loop

    } # end else

  } # End for aa loop

  # Cycle through each base station file
  for (xx in seq_along(base_file_types)){

  }


# ----








    #read_table(def_file)
    #write.table(def_file)



  # This will then be divided up by def file by collasping data and changed individually?

  # ------

    # Function for assembling inital line for each hdr input
  def_file_df <- function(def, default_file = "default_file"){
    output <- lapply(def, function(x) c(x, default_file)) %>%
    do.call(rbind, .)
    colnames(output) <- c("c1", "c2")
    return(output)
  }

  def_file_desc <- c("basin_default_file","hillslope_default_file","zone_default_file",
                    "soil_default_file","stratum_default_file",
                    "fire_default_file","base_stations_file")

  # Assemble hdr file
  hdr_df_tmp <- data.frame(c1 = length(hdr_list$basin_def), c2 = "num_basin_default_files", stringsAsFactors=FALSE) %>%
    rbind(., def_file_df(hdr_list$basin_def, "basin_default_file")) %>%

    rbind(., c(length(hdr_list$hill_def), "num_hillslope_default_files")) %>%
    rbind(., def_file_df(hdr_list$hill_def, "hillslope_default_file")) %>%

    rbind(., c(length(hdr_list$zone_def), "num_zone_default_files")) %>%
    rbind(., def_file_df(hdr_list$zone_def, "zone_default_file")) %>%

    rbind(., c(length(hdr_list$soil_def), "num_soil_default_files")) %>%
    rbind(., def_file_df(hdr_list$soil_def, "soil_default_file")) %>%

    rbind(., c(length(hdr_list$landuse_def), "num_landuse_default_files")) %>%
    rbind(., def_file_df(hdr_list$landuse_def, "landuse_default_file")) %>%

    rbind(., c(length(hdr_list$stratum_def), "num_stratum_default_files")) %>%
    rbind(., def_file_df(hdr_list$stratum_def, "stratum_default_file")) %>%

    # Add fire option here
    rbind(., c(length(hdr_list$fire_def), "num_fire_default_files")) %>%
    rbind(., def_file_df(hdr_list$fire_def, "fire_default_file")) %>%

    rbind(., c(length(hdr_list$base_stations), "num_base_stations")) %>%
    rbind(., def_file_df(hdr_list$base_stations, "base_stations_file"))
  #print(hdr_df)

  write.table(hdr_df, file = "happy", col.names = FALSE, row.names=FALSE, quote = FALSE)

}







