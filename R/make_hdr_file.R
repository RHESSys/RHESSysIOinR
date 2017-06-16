



make_hdr_file <- function(){

  world_hdr_out <- data.frame(c1 = length(hdr_input_list$basin_def), c2 = "num_basin_default_files", stringsAsFactors=FALSE)

  path_def_basin = vector()
  for (zz in seq_along(hdr_input_list$basin_def)){

    # Def file paths and names
    def_path <- dirname(hdr_input_list$basin_def)
    name_no_ext <- tools::file_path_sans_ext(basename(hdr_input_list$basin_def))
    ext <- tools::file_ext(hdr_input_list$basin_def)
    def_path_new <- file.path(def_path, name_no_ext)

    # Determine the type of def file used
    if (master_hdr_df[yy,hdr_input_list$basin_def][zz] == 0){
      # Reference original def file
      path_def_basin[zz] <- hdr_input_list$basin_def
    } else {
      # Reference new def file
      path_def_basin[zz] <- file.path(def_path, paste(name_no_ext,"_",file_name_ext,".txt",sep=""))
    }
  }

  world_hdr_out <- rbind(world_hdr_out, def_file_df(path_def_basin, "basin_default_file"))

 return(world_hdr_out)

}
