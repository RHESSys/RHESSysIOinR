#' compile_rhessys
#'
#' Compiles rhessys, with options to delete objects and move resulting RHESSys executable
#' @param location The file patch to where the rhessys makefile or folder is
#' @param delete_objs TRUE/FALSE to delete objects (before and after)
#' @param destination Optional input of where to move resulting RHESSys executable
#' @param make_args Arguments passed to the end of the make commandline call (examples: "wmfire='T'", "clean", or "clobber", )
#' @param ignore.stdout Passed through to system()
#' @author Will Burke
#'
#' @export

compile_rhessys = function(location,
                           delete_objs = TRUE,
                           destination = NULL,
                           make_args = NULL,
                           ignore.stdout = FALSE) {

  # find the makefile - i think foolproof but who knows
  if (endsWith(location, "makefile") && file.exists(location)) {
    makefile = location
  } else if (endsWith(file.path(location), "rhessys") && file.exists(file.path(location, "makefile")) ) {
    makefile = file.path(location, "makefile")
  } else {
    makeopts = list.files(location, pattern = "makefile$", recursive = TRUE)
    makeopts = grep(file.path("rhessys","makefile"), makeopts, value = TRUE)
    makeopts = file.path(location, makeopts)
    if (length(makeopts) == 0) {
      stop(noquote(paste0("Could not find any valid makefiles at path: ",location)))
    } else if (length(makeopts) > 1) {
      stop(noquote(paste0("Found more then 1 valid makefiles: ", paste(makeopts, collapse = ", "))))
    } else if (length(makeopts) == 1) {
      makefile = makeopts
    }
  }

  # delete objects if they exist
  if (delete_objs) {
    objs = file.path(dirname(makefile), "objects")
    if (!file.exists(objs)) {
      cat("Couldn't find the objects folder - may not exist yet\n")
    } else {
      out = file.remove(file.path(objs, list.files(objs)))
      if (length(out) > 0 && out) {
        cat("RHESSys objects removed before compilation\n")
      }
    }
  }

  if (!is.null(make_args)) {
    make_args = paste0(" ", trimws(make_args))
  }

  if (.Platform$OS.type == "windows") {
    dr = substr(path.expand(makefile), 0, 1)
    makefile = paste0("/mnt/", sub(dr, tolower(dr), sub(":", "", path.expand(makefile))))
  }

  make_cmd = paste0("make -C ", dirname(makefile), " -f ", basename(makefile), make_args)

  if (.Platform$OS.type == "windows") {
    make_cmd2 = noquote(paste("bash -c \"", make_cmd, "\"", sep = ""))
  } else {
    make_cmd2 = make_cmd
  }

  cat("Running makefile... \n\n")
  sysout = system(command = make_cmd2, ignore.stdout = ignore.stdout)
  cat("\nCommand line echo:", make_cmd2,"\n")

  if (sysout == 2) {
    warning("Make probably failed, returned: ", sysout)
  }

  # move rhessys
  if (!is.null(destination)) {
    rhessys_write = file.path(dirname(makefile), paste0("rhessys", substr(make_info[1], 11, nchar(make_info[1]))))
    file.rename(rhessys_write, to = file.path(destination, basename(rhessys_write)))
    cat("Moved '",basename(rhessys_write), "' from '", dirname(makefile), "' to '", destination,"'\n", sep = "")
  }

  # delete new objects
  if (delete_objs) {
    out = file.remove(file.path(objs, list.files(objs)))
    if (length(out) > 0 && out) { cat("RHESSys objects removed after compilation\n") }
  }

  cat("\n -------------------- End compile_rhessys.R -------------------- \n")

}
