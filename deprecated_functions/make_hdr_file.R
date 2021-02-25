#' Produces components of a hdr file
#'
#' This function generates the lines in a hdr file associated with a single def
#' file.
#'
#' @param master_table Data frame of a single row with references to def file
#'   number. A value of zero for the def file number indicates that the original
#'   def file is to be used. Higher numbers are added as a extension to the file
#'   name.
#' @param path_initial Path to the non-differiated def file
#' @param num_files ???
#' @param default_file ???
#'
#' @export
make_hdr_file <- function(master_table,
                          path_initial,
                          num_files,
                          default_file){

  # ---------------------------------------------------------------------
  # Function for assembling paths for each hdr input
  def_file_df <- function(def, default_file = "default_file"){
    #output <- lapply(def, function(x) c(x, default_file)) magrittr::%>% do.call(rbind, .)
    output0 <- lapply(def, function(x) c(x, default_file))
    output = do.call(rbind, output0)
    colnames(output) <- c("c1", "c2")
    return(output)
  }

  # ---------------------------------------------------------------------

  hdr_out <- data.frame(c1 = length(path_initial), c2 = num_files, stringsAsFactors=FALSE)

  path_full <- rep(NA,length(path_initial))
  for (zz in seq_along(path_initial)){

    # Def file paths and names
    path_short <- dirname(path_initial)[zz]
    name_no_ext <- tools::file_path_sans_ext(basename(path_initial))[zz]
    ext <- tools::file_ext(path_initial)[zz]
    path_new <- file.path(path_short, name_no_ext)

    file_name_ext <- ifelse(ext =="def", master_table[path_initial][zz], master_table["dated_id"])

    # Determine the type of def file used
    if (file_name_ext == 0){
      # Reference original def file
      path_full[zz] <- path_initial[zz]
    } else {
      # Reference new def file
      path_full[zz] <- file.path(path_new, paste(name_no_ext,"_",file_name_ext,".",ext,sep=""))
    }
  }

  hdr_out <- rbind(hdr_out, def_file_df(path_full, default_file))

 return(hdr_out)
}
