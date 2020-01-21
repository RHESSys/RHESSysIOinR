#' read_world
#'
#' Reads a worldfile
#'
#' @param worldfile Source worldfile
#' @author Will Burke
#'
#' @export

read_world = function(worldfile) {

  # ---------- Parse Worldfile ----------
  # parsing the values as characters to retain the exact value/precision
  read_world = readLines(worldfile, warn = FALSE, encoding = "UTF-8")
  read_world = read_world[nchar(trimws(read_world)) > 0]
  world =  strsplit(trimws(read_world), "\\s+")
  world = data.frame(matrix(unlist(world), nrow = length(world), byrow = T), stringsAsFactors = FALSE)
  names(world) = c("values","vars")

  # ---------- Find Levels----------
  index_all = which(world$vars == "world_ID" | world$vars == "basin_ID" | world$vars == "hillslope_ID" |
                      world$vars == "zone_ID" | world$vars == "patch_ID" | world$vars == "canopy_strata_ID")
  index_names = gsub("_ID", "", x = world$vars[index_all])
  index_max = c(index_all[2:length(index_all)] - 1, length(world$vars))
  world$level = unname(unlist(mapply(rep, index_names, (index_max - index_all) + 1 )))
  world$ID = unname(unlist(mapply(rep, world$values[index_all], (index_max - index_all) + 1 )))

  # get unique ID - useful for queries/parsing later
  world$unique_ID = unname(unlist(mapply(rep, c(1:length(index_names)), (index_max - index_all) + 1 )))

  return(world)
}
