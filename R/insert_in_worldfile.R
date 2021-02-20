#' Insert in Worldfile
#'
#' Inserts new state variables and values into a worldfile at specified location(s).
#' @param world_in Path to input worldfile. Cannot create from scratch (use RHESSysPreprocessing)
#' @param world_out Path to output worldfile. If set to FALSE, no file will be written.
#' @param insert Character vector of length = 2, containing state variable and value to be inserted into a worldfile.
#' @param insert_loc Location, in the form of an existing state variable name, for a new state variable(s) to be inserted following.
#' @param return_data TRUE/FALSE should an R data object be returned.
#' @param overwrite TRUE/FALSE should output worldfile overwrite existing file.
#'
#' @author Will Burke
#' @export

insert_in_worldfile = function(world_in,
                               world_out,
                               insert,
                               insert_loc,
                               return_data = FALSE,
                               overwrite = FALSE) {

  # ---------- Check Arguments ----------
  if (!file.exists(world_in)) { stop(noquote(paste0("No file found at: ", world_in))) }
  if (file.exists(world_out) & overwrite) {
    cat("File:", world_out, "will be overwritten.\n")
  } else if (file.exists(world_out) & !overwrite) {
    stop(noquote(paste0("Existing file found at: ", world_in, " & overwrite == FALSE")))
  }
  if (!is.vector(insert) | length(insert) != 2) {
    stop("Insert is not a length == 2 character vector")
  }

  # ---------- Read World ----------
  # this is the slow version - if needed switch to faster not in function method
  read_world = readLines(world_in, warn = FALSE, encoding = "UTF-8")
  read_world = read_world[nchar(trimws(read_world)) > 0]
  world =  strsplit(trimws(read_world), "\\s+")
  world = data.frame(matrix(unlist(world), nrow = length(world), byrow = T), stringsAsFactors = FALSE)
  names(world) = c("values","vars")

  # Check insert_loc is valid
  insert_loc_i = which(world$vars == insert_loc)
  if (length(insert_loc_i) == 0) {
    stop(noquote(paste0("Could not find state variable: ", insert_loc, " in worldfile: ", world_in)))
  }

  # text string to insert - follows existing formatting
  insert_txt = gsub(insert_loc,insert[1], read_world[insert_loc_i[1]])
  insert_txt = gsub(world$values[world$vars == insert_loc][1], insert[2], insert_txt)
  write_world = read_world

  # short loop through replacement indexes
  for (i in seq_along(insert_loc_i)) {
    write_world = c(write_world[1:(insert_loc_i[i] + i - 1)], insert_txt, write_world[(insert_loc_i[i] + i):length(write_world)])
  }

  # Report changes
  cat("Inserted line: ", trimws(insert_txt), "\nat ", length(insert_loc_i), " instances following state variable: ", insert_loc,"\n")

  # Write new worldfile
  writeLines(text = write_world, world_out)
  cat("Wrote new worldfile to: ", world_out)

}
