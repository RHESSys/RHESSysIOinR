#' check_params
#'
#' Function to check if parameters are valid and compare them to default parameters from a version of RHESSys
#' @param rh_file Path to the appropriate construct file in the RHESSys source code, e.g. construct_stratum_defaults.c
#' @param def_file Path to a appropriate parameter definition file to be compared to the RHESSys defaults
#' @author Will Burke
#' @export

check_params = function(rh_file, def_file) {
  # ------------------------------ Read in def file ------------------------------
  def_read = readLines(def_file, warn = FALSE)
  def_read = def_read[nchar(def_read) > 0]
  def_table_list =  strsplit(trimws(def_read), "\\s+")
  list_lens <- max(lengths(def_table_list))
  def_table <- as.data.frame(do.call(rbind, lapply(def_table_list, `length<-`, list_lens)), stringsAsFactors = FALSE)
  names(def_table)[1:2] = c("Value", "Name")
  def_table = def_table[,1:2]
  defaultparams = parse_rh_constr_func(rh_file)
  combparams = merge(defaultparams, def_table, by = "Name", all = T)
  return(combparams)
}

#' @export
parse_rh_constr_func = function(rh_file) {
  rawlines = trimws(readLines(rh_file))
  i=1
  while (i < length(rawlines)) {
    if (!grepl(";", rawlines[i])  & (!startsWith(rawlines[i], "/*")| !startsWith(rawlines[i], "//"))) {
      rawlines[i] =  paste0(rawlines[i]," ", rawlines[i + 1])
      rawlines = rawlines[-(i+1)]
    } else {
      i = i+1
    }
  }
  paramlines = rawlines[grepl("getStrParam|getIntParam|getFloatParam|getDoubleParam", rawlines)]
  paramtext = regmatches(paramlines, gregexpr("(getStrParam\\(|getIntParam\\(|getFloatParam\\(|getDoubleParam\\().*?\\)", paramlines))
  if (any(!endsWith(unlist(paramtext), ")"))) {
    cat("Something went wrong in c file parsing.")
  }
  paramsep = strsplit(gsub("\"","", gsub("\\)", "", gsub(".*\\(","", unlist(paramtext)))), ",")
  paramsep = lapply(paramsep, trimws)
  paramdf = as.data.frame(do.call(rbind, paramsep))[,3:6]
  names(paramdf) = c("Name", "Type", "DefaultValue", "UseDefault")
  paramdf = paramdf[,c("Name", "DefaultValue", "UseDefault")]

  # some custom checks for some stratum vars
 if ("epc.gxylem_max" %in% paramdf$Name) {
  if (paramdf$DefaultValue[paramdf$Name == "epc.gxylem_max"] == "default_object_list[i].epc.gl_smax") {
    paramdf$DefaultValue[paramdf$Name == "epc.gxylem_max"] = paramdf$DefaultValue[paramdf$Name == "epc.gl_smax"]
  }
 }
 if ("epc.gxylem_min_gs" %in% paramdf$Name) {
  if (paramdf$DefaultValue[paramdf$Name == "epc.gxylem_min_gs"] == "default_object_list[i].epc.gl_c*10") {
    paramdf$DefaultValue[paramdf$Name == "epc.gxylem_min_gs"] = as.numeric(paramdf$DefaultValue[paramdf$Name == "epc.gl_c"])*10
  }
}
if ("epc.gxylem_recovery_rate" %in% paramdf$Name) {
  if (paramdf$DefaultValue[paramdf$Name == "epc.gxylem_recovery_rate"] == "default_object_list[i].epc.gxylem_max*0.1") {
    paramdf$DefaultValue[paramdf$Name == "epc.gxylem_recovery_rate"] = as.numeric(paramdf$DefaultValue[paramdf$Name == "epc.gxylem_max"])*0.1
  }
}


  return(paramdf)
}

#' @export
compare_params = function(a, b, rh_construct_default_file) {
  options(scipen = 999)

  a_pars = check_params(rh_file = rh_construct_default_file,def_file = a)
  b_pars = check_params(rh_file = rh_construct_default_file,def_file = b)

  combined_pars = merge(a_pars[,c("Name","Value", "DefaultValue")],b_pars[,c("Name","Value")], by = "Name" )
  names(combined_pars)[names(combined_pars) == "Value.x"] = basename(a)
  names(combined_pars)[names(combined_pars) == "Value.y"] = basename(b)
  combined_pars = combined_pars[,c("Name",basename(a),basename(b),"DefaultValue" )]

  # if possible, set as numeric (and then back to chr)
  isnum = !is.na(suppressWarnings(as.numeric(combined_pars[,basename(a)])))
  combined_pars[isnum,basename(a)] = as.numeric(combined_pars[isnum,basename(a)])
  isnum = !is.na(suppressWarnings(as.numeric(combined_pars[,basename(b)])))
  combined_pars[isnum,basename(b)] = as.numeric(combined_pars[isnum,basename(b)])
  isnum = !is.na(suppressWarnings(as.numeric(combined_pars[,"DefaultValue"])))
  combined_pars[isnum,"DefaultValue"] = as.numeric(combined_pars[isnum,"DefaultValue"])

  # col for tracking if pars are diferent, accounting for defaults
  combined_pars$different_pars = TRUE
  both_default = is.na(combined_pars[,basename(a)]) & is.na(combined_pars[,basename(b)])
  combined_pars[both_default,"different_pars"] = FALSE
  # a NA/default, b is not, is default same as b
  combined_pars[is.na(combined_pars[,basename(a)]) & !is.na(combined_pars[,basename(b)]) &
                  combined_pars[,basename(b)] == combined_pars[,"DefaultValue"],"different_pars"] = FALSE
  # b NA/default, a is not, default compared to a
  combined_pars[is.na(combined_pars[,basename(b)]) & !is.na(combined_pars[,basename(a)]) &
                  combined_pars[,basename(a)] == combined_pars[,"DefaultValue"],"different_pars"] = FALSE
  # neither is NA, and are they the same?
  combined_pars[!is.na(combined_pars[,basename(a)]) & !is.na(combined_pars[,basename(b)]) &
                  combined_pars[,basename(a)] == combined_pars[,basename(b)], "different_pars"] = FALSE
  return(combined_pars)
}
