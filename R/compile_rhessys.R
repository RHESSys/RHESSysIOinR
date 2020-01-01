#' compile_rhessys
#'
#' Compiles rhessys, with options to delete objects and move resulting RHESSys executable
#' @param location The file patch to where the rhessys makefile or folder is
#' @param delete_obj TRUE/FALSE to delete objects (before and after)
#' @param destination Optional input of where to move resulting RHESSys executable
#' @param CFLAGS Overwrite CFLAGS in the makefile
#' @author Will Burke
#'
#' @export

compile_rhessys = function(location, delete_objs = TRUE, destination = NULL, CFLAGS = NULL) {

  # find the makefile
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

  print(paste0("Compiling RHESSys using makefile: ", makefile), quote = FALSE)
  read_make = readLines(makefile)
  make_info = c(read_make[startsWith(read_make, "VERSION")],
                read_make[startsWith(read_make, "CFLAGS")])
  print(paste(make_info, collapse = " | "), quote = FALSE)

  # delete objects
  if (delete_objs) {
    objs = file.path(dirname(makefile), "objects")
    if (!file.exists(objs)) { stop("Couldn't find the objects folder")}
    out = file.remove(file.path(objs, list.files(objs)))
    if (length(out) > 0 && out) {
      print("Objects removed", quote = FALSE)
    }
  }

  # CFLAGS - write a new makefile and point to that one
  if (!is.null(CFLAGS)) {
    new_makefile = read_make
    new_makefile[startsWith(read_make, "CFLAGS")] = paste0(new_makefile[startsWith(read_make, "CFLAGS")], " ", trimws(CFLAGS))
    makefile = file.path(dirname(makefile), "makefile_tmp")
    writeLines(new_makefile, makefile)
  }

  make_cmd = shQuote(paste0("make -C ", dirname(makefile), " -f ", basename(makefile)), type = "sh")

  if (.Platform$OS.type == "windows") {
    make_cmd2 = paste0("bash -c ", make_cmd)
  } else {
    make_cmd2 = make_cmd
  }

  sysout = system(make_cmd2)
  print(paste("Command line echo:", make_cmd2), quote = FALSE)

  if (sysout != 0) {
    warning("Make probably failed, returned: ", sysout)
  }

  if (!is.null(CFLAGS)) {
    out_rm = file.remove(makefile)
  }


  # move rhessys
  if (!is.null(destination)) {
    rhessys_write = file.path(dirname(makefile), paste0("rhessys", substr(make_info[1], 11, nchar(make_info[1]))))
    file.rename(rhessys_write, to = file.path(destination, basename(rhessys_write)))
    print(paste0("Moved ",basename(rhessys_write), " from ", dirname(makefile), " to ", destination), quote = FALSE)
  }


}
