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

    # Check if reference to def file type exists in hdr_input_list
    if (is.null(hdr_input_list[[def_file_types[aa]]]) == TRUE){
      print(paste("These is no", def_file_types[aa], "hdr file", sep=""))
    } else {

      # Process each def file input type
      def_file_input_path <- unlist(lapply(def_par_input_list, function(x) x[2]))
      def_file_input_unique <- unique(unlist(lapply(def_par_input_list, function(x) x[2])))

      # Step through multiple def file types (if present)
      for (bb in hdr_input_list[[def_file_types[aa]]]){


        # Check to see if def file type requires changes in parameter values.
        if (hdr_input_list[[def_file_types[aa]]][bb] %in% def_file_input_unique){

          # For column titles
          def_file_input_path <- unlist(lapply(def_par_input_list, function(x) x[2]))

          # Select columns that are to be ...
          def_file_input_par_types <- unlist(lapply(def_par_input_list, function(x) x[3]))
          par_file_selected <- def_file_input_par_types[def_file_input_path==hdr_input_list[[def_file_types[aa]]][bb]]


          print("if yes, process def files here.")

        }

        # print out lines of hdr file

        # Attach def file names to master_par_change_df

      }




      # ----
      # Process the division and consolidation
      # How many unique def files are their for each def type?
      def_file_input_unique <- unique(unlist(lapply(def_par_input_list, function(x) x[2])))

      def_file_input_types <- unlist(lapply(def_par_list, function(x) x[2]))  # !!!This has same name as earlier variable!!!
      par_types <- unlist(lapply(def_par_list, function(x) x[3]))

      par_file_selected <- par_types[def_file_input_types==def_unique[aa]]


      dots <- lapply(par_file_selected, as.symbol)
      par_change_df_by_def_file <- dplyr::select_(par_change_df, .dots=dots)

      par_change_df_by_def_file$group_id <- par_change_df_by_def_file %>%
        group_indices_(.dots=dots)
      happy <- par_change_df_by_def_file %>%
        group_by(group_id) %>%
        dplyr::filter(row_number()==1)

      print(happy)
      # ----

      # Column bind par_change_df_by_def_file$group_id to par_change_df

      # Call make def_file file
      for (bb in happy$group_id){
        process_def_parameters()

      }

    } # end else





  }






# ----








    #read_table(def_file)
    #write.table(def_file)

  }

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







