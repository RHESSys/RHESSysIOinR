#' Produces a hdr file in RHESSys
#'
#' The hdr file in RHESSys determines which definition and climate files are to be used for a given run.
#'
#'
#'
#'
#'
#' @export
create_hdr_file <- function(hdr_list, core_par_list, def_par_list, dated_seq_data){

  # ---------------------------------------------------------------------
  if (method == "all_combinations"){

    if (is.null(dated_seq_data[1]) == F){
      dated_seq_seq = list(dated_seq_data = seq_along(dated_seq_data))
    } else {
      dated_seq_seq = NULL
    }

    if (is.null(def_par_list[1]) == F){
      names(def_par_list) <- lapply(def_par_list, function(x) x[[3]]) # What is this actually doing? Assigning names
      def_par_change_values <- sapply(def_par_list, function(x) x[[1]], simplify = FALSE, USE.NAMES = TRUE)
    } else {
      def_par_change_values = NULL
    }

    par_change_df <- expand.grid(c(dated_seq_seq, def_par_change_values))

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

  for (aa in def_file){


    def_unique <- unique(lapply(def_par_list, function(x) x[[2]]))


    # dplyr::filter(def_file)
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







