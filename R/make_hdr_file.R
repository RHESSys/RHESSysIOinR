#' Produces components of a hdr file
#'
#' This function generates the lines in a hdr file associated with a single def file.
#'
#' @param master_table Data frame of a single row with references to def file number
#' @param path_initial Path to the non-differiated def file
#' @param num_files
#' @param default_file
#'
#' @export
make_hdr_file <- function(master_table, path_initial, num_files, default_file){

  # Function for assembling paths for each hdr input
  def_file_df <- function(def, default_file = "default_file"){
    output <- lapply(def, function(x) c(x, default_file)) %>%
      do.call(rbind, .)
    colnames(output) <- c("c1", "c2")
    return(output)
  }

  hdr_out <- data.frame(c1 = length(path_initial), c2 = num_files, stringsAsFactors=FALSE)

  path_def_basin <- vector()
  for (zz in seq_along(path_initial)){

    # Def file paths and names
    def_path <- dirname(path_initial)[zz]
    name_no_ext <- tools::file_path_sans_ext(basename(path_initial))[zz]
    ext <- tools::file_ext(path_initial)[zz]
    def_path_new <- file.path(def_path, name_no_ext)

    file_name_ext <- master_table[path_initial][zz]

    # Determine the type of def file used
    if (file_name_ext == 0){
      # Reference original def file
      path_def_basin[zz] <- path_initial
    } else {
      # Reference new def file
      path_def_basin[zz] <- file.path(def_path_new, paste(name_no_ext,"_",file_name_ext,".txt",sep=""))
    }
  }

  hdr_out <- rbind(hdr_out, def_file_df(path_def_basin, default_file))

 return(hdr_out)
}
