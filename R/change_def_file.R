#' Replaces parameters in a def file
#'
#' This function imports a def file, replaces the values of selected parameters,
#' and exports a def file.
#'
#' @param def_file Path and name of def file
#' @param par_sets Data frame with parameter names as colnames and a single row
#'   of parameter values
#' @param file_name_ext Optional extension to add to file name
#'
#'
#' @export
change_def_file <- function(def_file, par_sets, file_name_ext = NULL){

  # ---------------------------------------------------------------------
  # Read in def file
  def_table <- read.table(def_file, header = FALSE, stringsAsFactors = FALSE)

  # ---------------------------------------------------------------------
  # Replace parameters

  sub_pos <- sapply(colnames(par_sets), function(x,y) which(x == y[,2]), def_table)

  for (dd in seq_along(sub_pos)){
    def_table[sub_pos[dd],1] = par_sets[1,dd]
  }

  # ---------------------------------------------------------------------
  # Output def file

  path <- dirname(def_file)
  name_no_ext <- tools::file_path_sans_ext(basename(def_file))
  ext <- tools::file_ext(def_file)

  # Create new directory
  path_new <- file.path(path, name_no_ext)
  if(dir.exists(path_new) == FALSE){dir.create(path_new)}

  # Write new file
  file_name_out <- file.path(path_new, paste(name_no_ext,"_",file_name_ext,".txt",sep=""))
  write.table(def_table, file = file_name_out, row.names = FALSE, col.names = FALSE, quote=FALSE, sep="       ")
}

