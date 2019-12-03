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
  require(data.table) # this is important since fread and the data.table [] operations are much faster than base R
  # this shouldn't be in a require() but is for now so the [] based functions work

  # ---------- Parse read_X args ----------
  # set some vars to parse time/space input args
  space = c("basin","hillslope","zone","patch","stratum","fire")
  space_in = c("b", "h", "z", "p", "c","f")
  time = c("hourly", "daily", "monthly", "yearly")
  time_in = c("h", "d", "m", "y")
  grow = c("grow_", "")

  if (read_space != "dynamic") {
    space = space[space_in %in% read_space]
  }
  if (read_time != "dynamic") {
    time = time[time_in %in% read_time]
  }
  if (is.logical(read_grow)) {
    if (!read_grow) {
      grow = c("")

    }
  }

  # all_inputs contains combinations of grow, spatial level, and time step
  all_inputs = rbind(expand.grid(grow,space[!space == "fire"], time, stringsAsFactors = FALSE),
                  expand.grid("",space[space == "fire"], time, stringsAsFactors = FALSE))
  suffixes = sprintf("%s%s.%s",all_inputs[,1], all_inputs[,2], all_inputs[,3])
  all_files = paste(pre,"_",suffixes,sep="")
  exist_data = file.exists(all_files) # exist_data is T/F for if file exists and has data in it
  exist_data[exist_data] = sapply(all_files[exist_data],function(x) length(readLines(x,n = 4,warn = FALSE))) > 2
  files = all_files[exist_data]
  inputs = all_inputs[exist_data,]
  short_inputs = sapply(inputs, substring, 1, 1, simplify = "matrix")
  if (!is.matrix(short_inputs)) {short_inputs = t(as.matrix(short_inputs))}
  short_inputs[short_inputs[,2] == "s",2] = "c"

  # ---------- Build list ----------
  list_names = paste(inputs[,2], inputs[,3], inputs[,1],sep = "_")
  list_names = substr(list_names,0,nchar(list_names) - 1) # get rid of underscore at end
  short_names = paste(short_inputs[,2], short_inputs[,3], short_inputs[,1],sep = "")
  out_list = vector("list", length(inputs[,2]))
  names(out_list) = short_names

  # ----- Read in data w/ fread -----
  # do in loop since each fread is parallelized, idk if there's a better/faster way to do this
  for (i in 1:length(list_names)) {
    out_list[[i]] = data.table::fread(file = files[i],sep = " ",header = TRUE, stringsAsFactors = FALSE,showProgress = TRUE)
    setattr(out_list[[i]], "description", list_names[i])
    setattr(out_list[[i]], "source", files[i])
  }

  # ---------- Add dates ----------
  # not using mkdate or cal.wyd since they're slow since they replicate the entire data strucutre passed to them.
  # date info added to only daily output for now
  # loop is same speed as lapply in testing, whole thing is still kinda slow tho
  for (i in which(inputs[,3] == "daily")) {
    # if IDate doesn't work use this
    out_list[[i]]$date = as.Date(paste(out_list[[i]]$year, out_list[[i]]$month, out_list[[i]]$day, sep = "-"),"%Y-%m-%d")
    #out_list[[i]][, date := as.IDate(paste(year, month, day, sep = "-"),"%Y-%m-%d")]
    out_list[[i]]$wy = ifelse(out_list[[i]]$month >= 10, out_list[[i]]$year + 1, out_list[[i]]$year)
    out_list[[i]]$yd = as.integer(format(as.Date(out_list[[i]]$date), format = "%j"))
    #day_ct = out_list[[i]][ , max(yd), by = year]
    day_ct = aggregate(out_list[[i]]$yd, max, by = list(out_list[[i]]$year))
    no_leap = subset(day_ct, day_ct$V1 == 365)
    out_list[[i]]$wyd = ifelse((out_list[[i]]$year %in% no_leap$year),
                               ifelse(out_list[[i]]$yd >= 274,
                                      out_list[[i]]$yd - 273, out_list[[i]]$yd + 92),
                               ifelse(out_list[[i]]$yd >= 275, out_list[[i]]$yd - 274,
                                      out_list[[i]]$yd + 92)
    )
  }

  # ---------- Extra metrics ----------
  if (any(inputs[,2] == "basin" & inputs[,3] == "daily" & inputs[,1] == "")) {
    bd = which(inputs[,2] == "basin" & inputs[,3] == "daily" & inputs[,1] == "")
    out_list[[bd]][,et:=trans + evap]
    out_list[[bd]][,unfilled_cap:=sat_def - unsat_stor - rz_storage]
    if (any(inputs[,2] == "basin" & inputs[,3] == "daily" & inputs[,1] == "grow_")) {
      bdg = which(inputs[,2] == "basin" & inputs[,3] == "daily" & inputs[,1] == "grow_")
      porosity = out_list[[bd]][,sat_def/sat_def_z]
      if (nrow(out_list[[bd]]) == nrow(out_list[[bdg]])) {
        out_list[[bd]]$veg_awc = ifelse((out_list[[bdg]]$root_depth < out_list[[bd]]$sat_def_z),
                                        porosity * (out_list[[bd]]$sat_def_z - out_list[[bdg]]$root_depth) + out_list[[bd]]$rz_storage,
                                        out_list[[bd]]$rz_storage)
      } else {print(paste(,,"and", ,"have a mismatch number of rows. veg_awc not computed."))}
    }
  }

  if (any("stratum" == inputs[,2] & "grow_" == inputs[,1] & inputs[,3] == "daily")) {
    cdg = which(inputs[,2] == "stratum" & inputs[,3] == "daily" & inputs[,1] == "grow_")
    out_list[[cdg]][,woodc:=live_stemc + dead_stemc + live_crootc + dead_crootc]
    out_list[[cdg]][,plantc:=woodc + frootc + leafc]
  }

  # ---------- Patch family aggregation ----------
  if (patch_family_agg == TRUE) {
    for (i in which(inputs[,2] == "patch")) {

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

