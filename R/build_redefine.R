#' build_redefine
#'
#' Create a redefine worldfile
#'
#' @param worldfile Source worldfile
#' @param out_file Destination file to write
#' @param vars variables to edit
#' @param values values to insert for variables
#' @param std_thin Value to insert for standard thinning state vars (replacement value or multiplier depending on usage)
#' @param patchID Patch ID(s) to apply redefine to, can be used alone or with other subsets
#' @param strataID Strata ID(s) to apply redefine to, can be used alone or with other subsets
#' @param veg_parm_ID veg parm ID to apply changes to, can be used alone or with other subsets
#' @author Will Burke
#'
#' @export

build_redefine = function(worldfile, out_file, vars = NULL, values = NULL, std_thin = NULL, patchID = NULL, strataID = NULL, veg_parm_ID = NULL, append = FALSE) {

  # test
  # worldfile = worldfilesA[w]
  # out_file = sub(".world", "_under_0.1.world" ,redef_worldfilesA[w])
  # # std_thin = 0
  # # veg_parm_ID = 7
  # # patchID = 101
  # vars = NULL
  # values = NULL
  # strataID = NULL
  # append = FALSE
  #
  # std_thin = "1"
  # veg_parm_ID = "50"
  # patchID = "101"

  # ---------- Check Aguments ----------
  if (!file.exists(worldfile)) {stop(noquote(paste0("No file found at", worldfile)))}
  if (file.exists(out_file)) {print(paste0("File '",out_file,"' will be overwritten"), quote = FALSE)}

  # ---------- Parse Worldfile ----------
  # parsing the values as characters to retain the exact value/precision
  read_world = readLines(worldfile, warn = FALSE)
  read_world = read_world[nchar(read_world) > 0]
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
  world$unique = 1
  world$patchID = NA
  for (i in 2:length(world$unique)) {
    if (world$level[i] != world$level[i - 1] | world$ID[i] != world$ID[i - 1]) {
      world$unique[i] = world$unique[i - 1] + 1
      if (world$level[i] == "patch") {
        world$patchID[i] = world$ID[i]
      } else if (world$level[i] == "canopy_strata" & world$level[i - 1] == "patch") {
        world$patchID[i] = world$patchID[i - 1]
      }
    } else {
      world$unique[i] = world$unique[i - 1]
      world$patchID[i] = world$patchID[i - 1]
    }
  }

  # thinning vars
  thin_vars =  c(
    "cs_pool",
    "cs_leafc",
    "cs_dead_leafc",
    "cs_live_stemc",
    "cs_dead_stemc",
    "cs_live_crootc",
    "cs_dead_crootc",
    "cs_frootc",
    "cs.pool",
    "cs.leafc",
    "cs.dead_leafc",
    "cs.live_stemc",
    "cs.dead_stemc",
    "cs.live_crootc",
    "cs.dead_crootc",
    "cs.frootc",
    "cs.cpool"
  )

  other_thin_vars = c("cover_fraction", "gap_fraction")

  # thinning options- rhessys-side
  #
  # redefine_world_thin_remain -
  # For cover fraction and gap fraction, the value in the redefine file will become the value in the new worldfile
  # For the C-pool values, the values in the redefine file should be the fraction of those pools you want "thinned",
  # or removed from the live biomass pools. The corresponding N-pool values will be automatically removed and both the
  # C and N removed fractions will be distributed to the appropriate litter and CWD pools
  #
  # redefine_world_thin_harvest -
  # Works the same as above, but the aboveground C and N fractions are "harvested" (e.g., removed from the live biomass
  # pools but not added into the litter and CWD pools) while the belowground C and N fractions are removed and added to
  # the litter and CWD pools
  #
  # redefine_world_thin_snags -
  # Biomass C and N pools are reduced based on the fractions specified (as in the thin_remain option above) except livestem
  # C and N pools are transferred to deadwood C and N instead of CWD.


  # ugh clean this up to not mix numeric indices and TF vectors
  redef_index = NULL
  if (!is.null(std_thin)) {

    redef_strata = rep.int(TRUE, length(world$vars))
    redef_veg_strata = rep.int(TRUE, length(world$vars))
    redef_patch = rep.int(TRUE, length(world$vars))

    if (!is.null(patchID)) {
      redef_patch = world$patchID %in% patchID
    }
    if (!is.null(strataID)) {
      # functionality to support using just 1 or 2
      # if (all(nchar(strataID) == 1) & all(nchar(unique(world$ID[world$level == "canopy_strata"])) > 1)) {
      #   redef_strata = strata_IDs[substr(strata_IDs, nchar(strata_IDs), nchar(strata_IDs)) == as.character(strataID)]
      # }
      redef_strata = world$level == "canopy_strata" & world$ID %in% strataID
    }
    if (!is.null(veg_parm_ID)) {
      redef_veg_strata = world$unique %in% world$unique[world$vars == "veg_parm_ID" & world$values %in% veg_parm_ID]
    }

    redef_index = which(redef_patch & redef_strata & redef_veg_strata & world$vars %in% thin_vars)
    if (length(redef_index) == 0) {redef_index = NULL}
    redef_values_old = world$values[redef_index]

    redef_values = as.character(rep.int(std_thin, length(redef_values_old)))

    if (!is.null(redef_index)) {
      read_world[redef_index] = unname(mapply(sub,paste0(redef_values_old,"[[:blank:]]"),paste0(redef_values,"\t"),read_world[redef_index]))
    }
  }

  # ---------- Find and Replace Vars ----------
  replace_index = NULL
  if (!is.null(vars) & !is.null(values)) {
    if (length(vars) > 1 & length(values) == 1) {
      values = rep.int(values, length(vars))
    }

    for (i in 1:length(vars)) {

      replace_index = which(world$vars == vars[i])
      if (length(replace_index) == 0) {stop(noquote("var to replace can't be found in worldfile")) }

      # if unique values for every instance of var to be replaces were given, do nothing, otherwise repeat to get enough replacement values
      current_value = world$values[replace_index]
      if (length(values[i]) != length(replace_index)) {
        new_value = rep(values[i], length(replace_index)/length(values[i]))
      } else {
        new_value = values[i]
      }

      if (!is.null(replace_index)) {
        read_world[replace_index] = unname(mapply(sub,paste0(current_value,"[[:blank:]]"),paste0(new_value,"\t"),read_world[replace_index]))
      }
    }
  }

  # ---------- Replace all other values w -9999 ----------
  keep_vars = c("world_ID", "basin_ID", "hillslope_ID", "zone_ID", "patch_ID", "canopy_strata_ID",
                "num_basins", "num_hillslopes", "num_zones", "num_patches", "num_stratum")
  keep_index = c(unique(redef_index, replace_index),
                 which(world$vars %in% keep_vars))
  no_change_vars = c(1:length(read_world))[-keep_index]
  no_change_old = world$values[no_change_vars]

  # this is in case you only want to add vars
  if (!append) {
    read_world[no_change_vars] = unname(mapply(sub,paste0(no_change_old,"[[:blank:]]"),paste0("-9999","\t"),read_world[no_change_vars]))
  }

  # ---------- Write file ----------
  writeLines(text = read_world,out_file)

  print(noquote(paste("Successfully wrote redefine worldfile to",out_file)))

}
