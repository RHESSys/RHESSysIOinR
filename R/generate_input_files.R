#' Produces a hdr file in RHESSys
#'
#' The hdr file in RHESSys determines which definition and climate files are to be used for a given run.
#'
#' This function produces hdr file and associated files that are called by hdr file; def files and clim.base files.
#'
#' Loose ends
#' Non-unique parameters
#' Needs to work properly when various components are NULL
#' Base station code
#'
#' @export
generate_input_files <- function(hdr_input_list, def_par_input_list, dated_seq_input_list, parameter_method, world_hdr_prefix, world_file){

  # Process each hdr input list
  def_file_types <- purrr::discard(names(hdr_input_list), names(hdr_input_list)=="base_stations")
  base_file_types <- purrr::keep(names(hdr_input_list), names(hdr_input_list)=="base_stations")

  # Process each def file input list
  def_file_input_path <- unlist(lapply(def_par_input_list, function(x) x[1]))
  def_file_input_unique <- unique(def_file_input_path)
  def_file_input_par <- unlist(lapply(def_par_input_list, function(x) x[2]))

  # Cycle through each type of def file
  for (aa in seq_along(def_file_types)){

    hdr_input_selected <- hdr_input_list[[def_file_types[aa]]]

    # Step through def files with more than one type (if present)
    for (bb in seq_along(hdr_input_selected)){

      # Check to see if def file type requires changes in parameter values.
      if (hdr_input_selected[bb] %in% def_file_input_unique){

        # Select columns (parameters) that are associated with the selected def file
        def_par_selected <- def_file_input_par[def_file_input_path==hdr_input_selected[bb]]
        dots <- lapply(def_par_selected, as.symbol)

        # Establish IDs to identify def files with different parameter sets
        par_change_by_def_file <- dplyr::select_(master_par_change_df, .dots=dots)
        par_change_by_def_file$group_id <- group_indices_(par_change_by_def_file, .dots=dots)

        # Attach group ID to master_par_change_df
        master_par_change_df <- cbind(master_par_change_df,  "placeholder_name" = par_change_by_def_file$group_id)
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
        # Unchanging def files do not get renamed

        # Attach column to master_par_change_df for unchanging def files
        master_par_change_df <- cbind(master_par_change_df,  "placeholder_name" = rep(0, length(master_par_change_df[,1])))
        names(master_par_change_df)[names(master_par_change_df) == "placeholder_name"] <- hdr_input_selected[bb]
      }

    } # End for bb loop

  } # End for aa loop




  # ---------------------------------------------------------------------
  # Cycle through each base station file

  # Process each clim.base file path

  for (gg in seq_along(base_file_types)){
    print("Placeholder added")
    master_par_change_df <- cbind(master_par_change_df,  "placeholder_name" = rep(0, length(master_par_change_df[,1])))
    names(master_par_change_df)[names(master_par_change_df) == "placeholder_name"] <- hdr_input_list$base_stations
    # Call change_clim_base_file.R
    # Produce make_dated_seq_file.R files
  }



  # ---------------------------------------------------------------------
  # Generate hdr file according to master_par_change_df

  #Create hdr output folder and path names
  world_path <- dirname(world_file)
  world_hdr_path <- file.path(world_path, world_hdr_prefix)
  if(dir.exists(world_hdr_path) == FALSE){dir.create(world_hdr_path)}

  # Select out the def files from master_par_change_df
  master_hdr_df <- master_par_change_df[unlist(hdr_input_list)]

  # Cycle through each parameter set
  for (yy in seq_along(master_hdr_df[,1])){

    # Generate components of hdr file
    if (is.null(hdr_input_list$basin_def)==FALSE){
      world_hdr_out <- make_hdr_file(master_table=master_hdr_df[yy,], path_initial=hdr_input_list$basin_def, num_files="num_basin_default_files", default_file="basin_default_file")
    }

    if (is.null(hdr_input_list$hillslope_def)==FALSE){
      hdr_hillslope_comp <- make_hdr_file(master_table=master_hdr_df[yy,], path_initial=hdr_input_list$hillslope_def, num_files="num_hillslope_default_files", default_file="hillslope_default_file")
      world_hdr_out <- rbind(world_hdr_out, hdr_hillslope_comp)
    }

    if (is.null(hdr_input_list$zone_def)==FALSE){
      hdr_zone_comp <- make_hdr_file(master_table=master_hdr_df[yy,], path_initial=hdr_input_list$zone_def, num_files="num_zone_default_files", default_file="zone_default_file")
      world_hdr_out <- rbind(world_hdr_out, hdr_zone_comp)
    }

    if (is.null(hdr_input_list$soil_def)==FALSE){
      hdr_soil_comp <- make_hdr_file(master_table=master_hdr_df[yy,], path_initial=hdr_input_list$soil_def, num_files="num_soil_default_files", default_file="soil_default_file")
      world_hdr_out <- rbind(world_hdr_out, hdr_soil_comp)
    }

    if (is.null(hdr_input_list$landuse_def)==FALSE){
      hdr_landuse_comp <- make_hdr_file(master_table=master_hdr_df[yy,], path_initial=hdr_input_list$landuse_def, num_files="num_landuse_default_files", default_file="landuse_default_file")
      world_hdr_out <- rbind(world_hdr_out, hdr_landuse_comp)
    }

    if (is.null(hdr_input_list$stratum_def)==FALSE){
      hdr_stratum_comp <- make_hdr_file(master_table=master_hdr_df[yy,], path_initial=hdr_input_list$stratum_def, num_files="num_stratum_default_files", default_file="stratum_default_file")
      world_hdr_out <- rbind(world_hdr_out, hdr_stratum_comp)
    }

    if (is.null(hdr_input_list$fire_def)==FALSE){
      hdr_fire_comp <- make_hdr_file(master_table=master_hdr_df[yy,], path_initial=hdr_input_list$fire_def, num_files="num_fire_default_files", default_file="fire_default_file")
      world_hdr_out <- rbind(world_hdr_out, hdr_fire_comp)
    }

    if (is.null(hdr_input_list$base_stations)==FALSE){
      hdr_base_stations_comp <- make_hdr_file(master_table=master_hdr_df[yy,], path_initial=hdr_input_list$base_stations, num_files="num_base_stations_files", default_file="base_stations_file")
      world_hdr_out <- rbind(world_hdr_out, hdr_base_stations_comp)
    }

    # Write hdr file
    world_hdr_name_out <- file.path(world_hdr_path, paste(world_hdr_prefix,"_",yy,".hdr",sep=""))
    write.table(world_hdr_out, file = world_hdr_name_out, col.names = FALSE, row.names=FALSE, quote = FALSE, sep="        ")
    print(world_hdr_name_out)
  }

  # Attach hdr # to master list
  hdr_file_names <- sapply(seq_along(master_hdr_df[,1]), function(x,y,z) file.path(y, paste(z,"_", x,".hdr",sep="")), y=world_hdr_path, z=world_hdr_prefix)
  master_par_change_df <- cbind(master_par_change_df, hdr_file_names)


  # ---------------------------------------------------------------------
  # Make tec_file





  return(master_par_change_df)
}


