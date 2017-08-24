#' Produces a hdr file in RHESSys
#'
#' The hdr file in RHESSys determines which definition and climate files are to be used for a given run.
#'
#' This function produces hdr file and associated files that are called by hdr file; def files and clim.base files.
#'
#'
#' @export
generate_input_files <- function(input_rhessys,
                                 input_hdr_list,
                                 input_clim_base_list,
                                 input_tec_data,
                                 option_sets_all,
                                 option_sets_def_par,
                                 option_sets_par,
                                 option_sets_hdr,
                                 option_sets_dated_seq,
                                 world_hdr_prefix,
                                 world_file){


  # ---------------------------------------------------------------------
  # Export parameter file and all-options file

  write.csv(option_sets_par, file.path(input_rhessys$output_folder, paste(input_rhessys$output_filename, "_parameter_sets.csv", sep="")), row.names = FALSE, quote=FALSE)
  write.csv(option_sets_all, file.path(input_rhessys$output_folder, paste(input_rhessys$output_filename, "_all_options.csv", sep="")), row.names = FALSE, quote=FALSE)

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
  # Cycle through each base station/dated sequence file

  # Step though each dated seq file (Note that clim files without dated sequence
  # will have a option_sets_dated_seq$dated_id of length one and equal to 0)
  for (aa in seq_along(option_sets_dated_seq$dated_id)){

    # Step through each climate base station
    for (bb in seq_along(input_clim_base_list)){

      make_clim_base_file(input_clim_base = input_clim_base_list[[bb]],
                          clim_base_path = input_hdr_list$base_stations[bb],
                          input_dated_seq = input_dated_seq_list[[aa]],
                          clim_dated_ext = option_sets_dated_seq$dated_id[aa])
    }
  }


  # ---------------------------------------------------------------------
  # Generate hdr files

  option_sets_hdr <- arrange(option_sets_hdr, hdr_id) # Arrange hdr_id in sequential order

  # Create hdr output folder
  world_path <- dirname(input_rhessys$world_file)[1]
  world_hdr_path <- file.path(world_path, input_rhessys$world_hdr_prefix)
  if(dir.exists(world_hdr_path) == FALSE){dir.create(world_hdr_path)}

  # Cycle through each parameter set
  for (yy in option_sets_hdr$hdr_id){

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
    world_hdr_name_out <- file.path(world_hdr_path, paste(input_rhessys$world_hdr_prefix,"_",yy,".hdr",sep=""))
    write.table(world_hdr_out, file = world_hdr_name_out, col.names = FALSE, row.names=FALSE, quote = FALSE, sep="        ")
    print(paste("New hdr file written :", world_hdr_name_out))
  }


  # ---------------------------------------------------------------------
  # Write tec file

  if (is.null(input_tec_data) == FALSE){
    make_tec_file(tec_file = input_rhessys$tec_file, tec_data = input_tec_data)
    print(paste("Tec file has been written"))
  }

  # ---------------------------------------------------------------------

  #return()
}


