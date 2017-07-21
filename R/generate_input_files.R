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
generate_input_files <- function(input_hdr_list, option_sets_def_par, option_sets_hdr, world_hdr_prefix, world_file){

  # ---------------------------------------------------------------------
  # Generate def files

  if (is.null(option_sets_def_par)==FALSE){
    # Step though each def file
    for (aa in seq_along(option_sets_def_par)){

      # Step through each unique parameter set and make def file
      for (bb in seq_along(option_sets_def_par[[aa]]$group_id)){

        change_def_file(def_file = names(option_sets_def_par)[aa],
                        par_sets = dplyr::select(option_sets_def_par[[aa]], -group_id)[bb,],
                        file_name_ext = as.character(option_sets_def_par[[aa]]$group_id[bb]))
      }
      print(paste("New def files written for file", names(option_sets_def_par)[aa]))
    }
  }


  # ---------------------------------------------------------------------
  # Cycle through each base station file

  # Process each clim.base file path

  # base_file_types <- purrr::keep(names(input_hdr_list), names(input_hdr_list)=="base_stations")
  #
  # for (gg in seq_along(base_file_types)){
  #   print("Placeholder added")
  #   master_par_change_df <- cbind(master_par_change_df,  "placeholder_name" = rep(0, length(master_par_change_df[,1])))
  #   names(master_par_change_df)[names(master_par_change_df) == "placeholder_name"] <- input_hdr_list$base_stations
    # Call change_clim_base_file.R
    # Produce make_dated_seq_file.R files
  #}


  # ---------------------------------------------------------------------
  # Generate hdr files

  # Create hdr output folder and path names
  world_path <- dirname(world_file)
  world_hdr_path <- file.path(world_path, world_hdr_prefix)
  if(dir.exists(world_hdr_path) == FALSE){dir.create(world_hdr_path)}

  # Cycle through each parameter set
  for (yy in seq_along(option_sets_hdr[,1])){

    # Generate components of hdr file
    if (is.null(input_hdr_list$basin_def)==FALSE){
      world_hdr_out <- make_hdr_file(master_table=option_sets_hdr[yy,], path_initial=input_hdr_list$basin_def, num_files="num_basin_default_files", default_file="basin_default_file")
    }

    if (is.null(input_hdr_list$hillslope_def)==FALSE){
      hdr_hillslope_comp <- make_hdr_file(master_table=option_sets_hdr[yy,], path_initial=input_hdr_list$hillslope_def, num_files="num_hillslope_default_files", default_file="hillslope_default_file")
      world_hdr_out <- rbind(world_hdr_out, hdr_hillslope_comp)
    }

    if (is.null(input_hdr_list$zone_def)==FALSE){
      hdr_zone_comp <- make_hdr_file(master_table=option_sets_hdr[yy,], path_initial=input_hdr_list$zone_def, num_files="num_zone_default_files", default_file="zone_default_file")
      world_hdr_out <- rbind(world_hdr_out, hdr_zone_comp)
    }

    if (is.null(input_hdr_list$soil_def)==FALSE){
      hdr_soil_comp <- make_hdr_file(master_table=option_sets_hdr[yy,], path_initial=input_hdr_list$soil_def, num_files="num_soil_default_files", default_file="soil_default_file")
      world_hdr_out <- rbind(world_hdr_out, hdr_soil_comp)
    }

    if (is.null(input_hdr_list$landuse_def)==FALSE){
      hdr_landuse_comp <- make_hdr_file(master_table=option_sets_hdr[yy,], path_initial=input_hdr_list$landuse_def, num_files="num_landuse_default_files", default_file="landuse_default_file")
      world_hdr_out <- rbind(world_hdr_out, hdr_landuse_comp)
    }

    if (is.null(input_hdr_list$stratum_def)==FALSE){
      hdr_stratum_comp <- make_hdr_file(master_table=option_sets_hdr[yy,], path_initial=input_hdr_list$stratum_def, num_files="num_stratum_default_files", default_file="stratum_default_file")
      world_hdr_out <- rbind(world_hdr_out, hdr_stratum_comp)
    }

    if (is.null(input_hdr_list$fire_def)==FALSE){
      hdr_fire_comp <- make_hdr_file(master_table=option_sets_hdr[yy,], path_initial=input_hdr_list$fire_def, num_files="num_fire_default_files", default_file="fire_default_file")
      world_hdr_out <- rbind(world_hdr_out, hdr_fire_comp)
    }

    if (is.null(input_hdr_list$base_stations)==FALSE){
      hdr_base_stations_comp <- make_hdr_file(master_table=option_sets_hdr[yy,], path_initial=input_hdr_list$base_stations, num_files="num_base_stations_files", default_file="base_stations_file")
      world_hdr_out <- rbind(world_hdr_out, hdr_base_stations_comp)
    }

    # Write hdr file
    world_hdr_name_out <- file.path(world_hdr_path, paste(world_hdr_prefix,"_",yy,".hdr",sep=""))
    write.table(world_hdr_out, file = world_hdr_name_out, col.names = FALSE, row.names=FALSE, quote = FALSE, sep="        ")
    print(paste("New hdr file written :", world_hdr_name_out))
  }

  # Attach hdr # to master list
#  hdr_file_names <- sapply(seq_along(master_hdr_df[,1]), function(x,y,z) file.path(y, paste(z,"_", x,".hdr",sep="")), y=world_hdr_path, z=world_hdr_prefix)
#  master_par_change_df <- cbind(master_par_change_df, hdr_file_names)


  # ---------------------------------------------------------------------
  # Make tec_file





  return(master_par_change_df)
}


