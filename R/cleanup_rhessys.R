#' cleanup_rhessys
#'
#' Function to remove rhessys output, options to move it to a specific folder or one w unique ID,
#' only delete/move output based on a pattern
#' @param dir Directory to look for output files within. To be safe function will not look recursively in subfolders.
#' @param output_name The base name of the rhessys output, used to select only one set of rhessy outputs
#' (still getting all of the varyious spatial + temporal levels for that output)
#' @param copy TRUE/FALSE should the output be copied or just deleted
#' @param destination Specify a destination for output to be copied to, if left NULL a folder will be
#' generated with a unique name with format rh_out_datetime
#' @author William Burke
#' @export

cleanup_rhessys = function(dir = NULL, output_name = NULL, copy = TRUE, destination = NULL) {

  # find all eligable rhessys outputs + RHESSysIO outputs
  if (is.null(dir)) {
    dir = getwd()
  }

  rh_pattern = "\\.(hourly|daily|monthly|yearly|params)$"
  # subset just those that match pattern
  if (!is.null(output_name)) {
    rh_pattern = paste0(output_name, ".*",rh_pattern)
  }
  # if there's issues with the suffixes, copy the explicit ones from select_output_variables_R.R
  rh_files = list.files(path = dir, pattern = rh_pattern,full.names = F)

  if (copy) {
    # copy the files
    if (!is.null(destination)) {
      # check destination folder is valid/make it if it's not
      # add note to console if you made a folder
      if (!dir.exists(destination)) {
        dir.create(path = file.path(dir, destination))
        file.rename(from = file.path(dir, rh_files), to = file.path(dir,destination,rh_files))
      }
    } else {
      # make a folder with a unique name -  rh_out_date+time
      # cat to console
      dirname = paste0("rh_out_", gsub( ":", ".", sub( " ", "_", Sys.time())))
      dir.create(path = file.path(dir, dirname))
      cat("Created directory '",file.path(dir, dirname),"'\n", sep = "")
      shh = file.rename(from = file.path(dir, rh_files), to = file.path(dir,dirname,rh_files))
      cat("Moved RHESSys output files to new directory\n")
    }
  } else {
    # delete the files
    shh = file.remove(file.path(dir, rh_files))
    cat("Deleted RHESSys output files from", dir,"\n")
  }

}
