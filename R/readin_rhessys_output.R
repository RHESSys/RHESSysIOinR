#' readin_rhessys_output
#'
#' Pulls output from a single RHESSys run into R, optionally calculates statistics. This should
#' be much faster than readin_rhessys_output since it uses data.table file read and aggregation methods
#' @param pre file prefix. Does not include spatial or temporal level of aggrigation
#' ie. "my_watershed" NOT "my_watershed_basin" or "my_watershed_basin.daily"
#' @param read_space Spatial levels of all_files to be read. Default is "dynamic" which will get all all_files
#' that were output (with more than 1 line) with the matching prefix. Can also input a character
#' vector with any combination of "b", "h", "z", "p", "c", "f". Even if all_files are specified,
#' they will be checked if they have data, and only read if they do.
#' @param read_time Timesteps of all_files to be read. Default is "dynamic". Can specify any combination of
#' "h", "d", "m", "y", for hourly, daily, monthly, and yearly respectively
#' @param read_grow Should grow files be read? Default is dynamic. Can also specify TRUE or FALSE
#' @param stat Stat to be computed. Only tested with "mean".
#' @param stat_time Not used yet - only produces "wy", "wyd" for now.
#' @param stat_space Not used yet. All stats aggregate to basin level. In the future will have options here
#' to retain finer spatial resolutions.
#' @param patch_family_agg Should additional dataframes of aggregated patch families be created?
# @param patch_areas If aggregating patches to patch family for the stratum output only, a data frame of
# patch IDs and their respective areas needs to be provided for the weighted average to be computed correctly
#'
#' @export

readin_rhessys_output = function(pre,
                                 read_space = "dynamic",
                                 read_time = "dynamic",
                                 read_grow = "dynamic",
                                 stat = FALSE,
                                 stat_time = c("wy", "wyd"),
                                 stat_space = "b",
                                 patch_family_agg = FALSE) {

  # ---------- Arg checking and packages ----------
  # lol maybe add or not idk

  # ---------- Parse read_X args ----------
  space = c("basin","hillslope","zone","patch","stratum","fire")
  space_in = c("b", "h", "z", "p", "c","f")
  time = c("hourly", "daily", "monthly", "yearly")
  time_in = c("h", "d", "m", "y")

  if (read_space != "dynamic") {
    space = space[space_in %in% read_space]
  }
  if (read_time != "dynamic") {
    time = time[time_in %in% read_time]
  }

  grow = c("grow_", "")
  if (is.logical(read_grow) && !read_grow) {
    grow = c("")
  }

  # all_inputs contains combinations of grow, spatial level, and time step
  all_inputs = rbind(expand.grid(grow,space[!space == "fire"], time, stringsAsFactors = FALSE),
                  expand.grid("",space[space == "fire"], time, stringsAsFactors = FALSE))
  suffixes = paste0(all_inputs[,1], all_inputs[,2], ".", all_inputs[,3])
  all_files = paste(pre,"_",suffixes,sep="")
  exist_data = file.exists(all_files) # exist_data is T/F for if file exists and has data in it
  exist_data[exist_data] = sapply(all_files[exist_data],function(x) length(readLines(x,n = 4,warn = FALSE))) > 2
  files = all_files[exist_data]
  inputs = all_inputs[exist_data,]

  # get short inputs
  abrev_inputs = sapply(inputs, substring, 1, 1, simplify = "matrix")
  if (!is.matrix(abrev_inputs)) {abrev_inputs = t(as.matrix(abrev_inputs))}
  abrev_inputs[abrev_inputs[,2] == "s",2] = "c"

  # ---------- Build list ----------
  list_names = paste(inputs[,2], inputs[,3], inputs[,1],sep = "_")
  list_names = substr(list_names,0,nchar(list_names) - 1) # get rid of underscore at end
  # actually use short names for lists to make it consistent w old version
  short_names = paste(abrev_inputs[,2], abrev_inputs[,3], abrev_inputs[,1],sep = "")
  out_list = vector("list", length(inputs[,2]))
  names(out_list) = short_names

  # ----- Read in data w/ fread -----
  # in loop for now, can use lapply version below if its faster
  for (i in 1:length(list_names)) {
    out_list[[i]] = data.table::fread(file = files[i], stringsAsFactors = FALSE,showProgress = TRUE)
    #attr(out_list[[i]], "description") = list_names[i]
    data.table::setattr(out_list[[i]], "description", list_names[i])
    #attr(out_list[[i]], "source") = files[i]
    data.table::setattr(out_list[[i]], "source", files[i])
  }
  # out_list2 = lapply(files, data.table::fread, stringsAsFactors = FALSE,showProgress = TRUE)

  # ---------- Add dates ----------
  # not using mkdate or cal.wyd since they're slow since they replicate the entire data strucutre passed to them.
  # date info added to only daily output for now

  out_list = lapply(out_list, add_dates)

  # ---------- Extra metrics ----------
  if (any(inputs[,2] == "basin" & inputs[,3] == "daily" & inputs[,1] == "")) {
    bd = which(inputs[,2] == "basin" & inputs[,3] == "daily" & inputs[,1] == "")
    bdg = which(inputs[,2] == "basin" & inputs[,3] == "daily" & inputs[,1] == "grow_")
    out_list[[bd]]$et = out_list[[bd]]$trans + out_list[[bd]]$evap
    out_list[[bd]]$unfilled_cap = out_list[[bd]]$unsat_stor + out_list[[bd]]$rz_storage

    if (any(inputs[,2] == "basin" & inputs[,3] == "daily" & inputs[,1] == "grow_") &&
        nrow(out_list[[bd]]) == nrow(out_list[[bdg]])) {

      # they might be from different runs
      out_list[[bd]]$veg_awc = ifelse((out_list[[bd]]$rootdepth < out_list[[bd]]$sat_def_z),
                                      out_list[[bd]]$sat_def / out_list[[bd]]$sat_def_z *
                                        (out_list[[bd]]$sat_def_z - out_list[[bd]]$rootdepth) + out_list[[bd]]$rz_storage,
                                      out_list[[bd]]$rz_storage)
    } else {print("basin daily and basin daily grow have different number rows")}
  }

  if (any("stratum" == inputs[,2] & "grow_" == inputs[,1] & inputs[,3] == "daily")) {
    cdg = which(inputs[,2] == "stratum" & inputs[,3] == "daily" & inputs[,1] == "grow_")
    out_list[[cdg]]$woodc = out_list[[cdg]]$live_stemc + out_list[[cdg]]$dead_stemc +
      out_list[[cdg]]$live_crootc + out_list[[cdg]]$dead_crootc
    out_list[[cdg]]$plantc = out_list[[cdg]]$woodc + out_list[[cdg]]$frootc + out_list[[cdg]]$leafc
  }

  # ---------- Patch family aggregation ----------
  if (patch_family_agg == TRUE) {
    # patch + stratum aggregation
    if (any("patch" == inputs[,2])) {
      fam_agg_ind = which("patch" == inputs[,2])
      pfam_list_p = lapply(out_list[fam_agg_ind], patch_fam_agg)
      names(pfam_list_p) = paste0(names(out_list)[fam_agg_ind],"_fam_agg")
      out_list = c(out_list, pfam_list_p)

    }
    if (any("stratum" == inputs[,2])) {
      fam_agg_ind = which("stratum" == inputs[,2])
      if (!"patch" %in% inputs[,2]) {
        stop("Need patches to get area - or add a code fix line 183")
      }
      areas = out_list$pd[,c("patchID", "area")]
      pfam_list_s = lapply(out_list[fam_agg_ind], patch_fam_agg, areas = unique(areas))
      names(pfam_list_s) = paste0(names(out_list)[fam_agg_ind],"_fam_agg")
      out_list = c(out_list, pfam_list_s)

    }
  }

  # ---------- Stats ----------
  if (stat != FALSE & stat != "mean") {warning("this might not work")}

  if (stat != FALSE) {
    # make option table for stats - input table, time step to aggregate to, aggregation method
    stat_opts = expand.grid(short_names, stat_time, stat,stringsAsFactors = FALSE)
    stat_list = vector("list",length = length(stat_opts[,3]))
    names(stat_list) = paste(stat_opts[,1], stat_opts[,2], stat_opts[,3],sep = "_")

    # run stats based on option table
    for (i in 1:length(stat_list)) {
      stat_list[[i]] = out_list[[stat_opts[i,1]]][, lapply(.SD, match.fun(stat_opts[i,3])), by = c(stat_opts[i,2])]
    }

    # extra level-specific stats
    bdwy = stat_opts[,1] == "basin_daily" & stat_opts[,2] == "wy"
    if (any(bdwy)) {
      bd = inputs[,2] == "basin" & inputs[,3] == "daily" & inputs[,1] == ""
      stat_list[[which(bdwy)]]$pkswe = out_list[[which(bd)]][, max(snowpack), by = c("wy") ][,2]
    }
    cdgwy = stat_opts[,1] == "stratum_daily_grow" & stat_opts[,2] == "wy"
    if (any(cdgwy)) {
      cdg = "stratum" == inputs[,2] & "grow_" == inputs[,1] & inputs[,3] == "daily"
      stat_list[[which(cdgwy)]]$mincpool = out_list[[which(cdg)]][, min(cpool), by = c("wy") ][,2]
      stat_list[[which(cdgwy)]][,mincpoolp:= plantc * 100]
    }

    out_list = c(out_list, stat_list)
  }


  return(out_list)
}

