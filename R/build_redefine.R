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

build_redefine = function(worldfile, out_file, vars = NULL, values = NULL, std_thin = NULL, patchID = NULL, strataID = NULL, veg_parm_ID = NULL) {


  # ---------- Check Arguments ----------
  if (!file.exists(worldfile)) { stop(noquote(paste0("No file found at: ", worldfile))) }
  if (file.exists(out_file)) {cat("File:",out_file,"will be overwritten.\n")}

  # Either need vars + values or std_thin
  if ((is.null(vars) | is.null(values)) & is.null(std_thin)) {
    stop(cat("Input is required for both `vars` and `values`, or `std_thin`"))
  }
  # if using vars + values - values must either be length of vars or 1
  if ((!is.null(vars) & !is.null(values)) && length(vars) != length(values) && length(values) != 1) {
    stop(cat("Input length mismatch:", length(vars), "input `vars` and", length(values),
             "input `values`. `length(values) == length(vars)` or `length(values) == 1`.\n"))
  }

  # read and parse
  world = read_world(worldfile)

  # get patch ID col that includes stratum
  world$patch_ID = world$ID
  world$patch_ID[world$level == "canopy_strata"] = NA
  world$patch_ID = zoo::na.locf(world$patch_ID)
  world$patch_ID[world$level %in% c("world", "basin", "hillslope", "zone")] = NA

  # read to be used for final line by line replacement
  read_world = readLines(worldfile, warn = FALSE, encoding = "UTF-8")
  read_world = read_world[nchar(trimws(read_world)) > 0]

  # thinning vars
  thin_vars =  c(
    "cs.cpool",
    "cs.leafc",
    "cs.dead_leafc",
    "cs.live_stemc",
    "cs.dead_stemc",
    "cs.live_crootc",
    "cs.dead_crootc",
    "cs.frootc"
  )

  # could add, but wont be read anyways:
  # "ns.npool"
  # "ns.leafn"
  # "ns.dead_leafn"
  # "ns.live_stemn"
  # "ns.dead_stemn"
  # "ns.live_crootn"
  # "ns.dead_crootn"
  # "ns.frootn"

  other_thin_vars = c("cover_fraction", "gap_fraction", "cs.stem_density")

  # ---------- Thinning redefine ----------
  redef_index = NULL
  if (!is.null(std_thin)) {

    redef_strata = rep.int(TRUE, length(world$vars))
    redef_veg_strata = rep.int(TRUE, length(world$vars))
    redef_patch = rep.int(TRUE, length(world$vars))

    if (!is.null(patchID)) {
      # this only works if changing patch vars
      redef_patch = world$patch_ID %in% as.character(patchID)

    }
    if (!is.null(strataID)) {
      # functionality to support using just 1 or 2
      # if (all(nchar(strataID) == 1) & all(nchar(unique(world$ID[world$level == "canopy_strata"])) > 1)) {
      #   redef_strata = strata_IDs[substr(strata_IDs, nchar(strata_IDs), nchar(strata_IDs)) == as.character(strataID)]
      # }
      redef_strata = world$level == "canopy_strata" & world$ID %in% as.character(strataID)
    }
    if (!is.null(veg_parm_ID)) {
      redef_veg_strata = world$unique %in% world$unique[world$vars == "veg_parm_ID" & world$values %in% as.character(veg_parm_ID)]
    }

    redef_index = which(redef_patch & redef_strata & redef_veg_strata & (world$vars %in% thin_vars))
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
      if (length(replace_index) == 0) {stop(noquote("var to replace can't be found in worldfile.\n")) }

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


  if ( (is.null(redef_index) || all(!redef_index)) & (is.null(replace_index) || all(!replace_index)) ) {
    cat("No vars matched criteria, all set to -9999.\n")
  }

  # ---------- Replace all other values w -9999 ----------
  keep_vars = c(
    "world_ID",
    "basin_ID",
    "hillslope_ID",
    "zone_ID",
    "patch_ID",
    "canopy_strata_ID",
    "num_basins",
    "num_hillslopes",
    "num_zones",
    "num_patches",
    "num_canopy_strata",
    "num_stratum",
    "basin_n_basestations",
    "basin_basestation_ID",
    "hillslope_n_basestations",
    "hillslope_basestation_ID",
    "zone_n_basestations",
    "zone_basestation_ID",
    "patch_n_basestations",
    "patch_basestation_ID",
    "canopy_strata_n_basestations",
    "canopy_strata_basestation_ID"
  )
  keep_index = c(unique(redef_index, replace_index), which(world$vars %in% keep_vars))
  no_change_vars = c(1:length(read_world))[-keep_index]
  no_change_value = world$values[no_change_vars]

  read_world[no_change_vars] = unname(mapply(sub,paste0(no_change_value,"[[:blank:]]"),paste0("-9999","\t"),read_world[no_change_vars]))

  # ---------- Write file ----------
  writeLines(text = read_world,out_file)

  cat("Successfully wrote redefine worldfile to",out_file,"\n")

}
