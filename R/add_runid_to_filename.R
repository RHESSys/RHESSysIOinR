#' Create a subfolder and add a runID to a filename
#'
#' When conducting multiple runs, this function 1) creates the subfolder for
#' modified def and tec files, and 2) generates filenames with unique runID for
#' the def and tec files within each subfolder.
#'
#' @param filename Path and name of file with extension
#' @param runID The unique ID to be appended to the filename.
#'
#' @export

add_runID_to_filename <- function(filename, runID = NULL){
  # Parse filename
  path <- dirname(filename)
  name_no_ext <- tools::file_path_sans_ext(basename(filename))
  ext <- tools::file_ext(filename)

  # Create new subdirectory if necessary
  path_new <- file.path(path, name_no_ext)
  if (dir.exists(path_new) == FALSE) {dir.create(path_new)}

  # Get new filename
  if (!is.null(runID)){
    filename_out <- file.path(path_new, paste0(name_no_ext, "_", runID, ".", ext))
  } else {
    filename_out <- file.path(path_new, paste0(name_no_ext, ".", ext))
  }

  return(filename_out)
}


