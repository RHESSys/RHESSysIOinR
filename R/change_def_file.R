#' Replaces parameters in a def file
#'
#' This function imports a def file, replaces the values of selected parameters, and exports a def file.
#' @param def_file Path and name of def file
#' @param par_sets Data frame with parameter names as colnames and a single row of parameter values
#' @param file_name_ext Optional extension to add to file name
#'
#' @export

change_def_file <- function(def_file, par_sets, file_name_ext = NULL){

  # ------------------------------ Read in def file ------------------------------
  # def_table = data.table::fread(def_file, header = FALSE, stringsAsFactors = FALSE)
  # def_table = read.table(def_file, header = FALSE, stringsAsFactors = FALSE)
  # both above have issues - read table cant handle comments, fread cant do different spacing

  # doing it manually
  def_read = readLines(def_file, warn = FALSE)
  def_read = def_read[nchar(def_read) > 0]
  def_table_list =  strsplit(trimws(def_read), "\\s+")
  list_lens <- max(lengths(def_table_list))
  def_table <- as.data.frame(do.call(rbind, lapply(def_table_list, `length<-`, list_lens)), stringsAsFactors = FALSE)
  names(def_table)[1:2] = c("pars", "names")

  # ------------------------------ Replace parameters ------------------------------
  par_sets_df = data.frame(pars = as.vector(t(par_sets[1,])), names = colnames(par_sets))

  if (any(duplicated(par_sets_df$names))) {
    cat("Duplicate def file changes found for", par_sets_df$names[duplicated(par_sets_df$names)], "in", def_file)
  }

  # in case on comments
  if (ncol(def_table) == 3) {
    par_sets_df$V3 = NA
  }

  # replace existing in def file
  in_def = par_sets_df$names %in% def_table$names
  if (any(in_def)) {
    def_table[match(par_sets_df$names, def_table$names, nomatch = 0),1] = par_sets_df$pars[in_def]
  }
  # add to def file
  if (any(!in_def)) {
    def_table = rbind(def_table, par_sets_df[!in_def,])
  }

  def_table <- format(def_table, scientific=FALSE);

  # ------------------------------ Output def file ------------------------------
  path <- dirname(def_file)
  name_no_ext <- tools::file_path_sans_ext(basename(def_file))
  ext <- tools::file_ext(def_file)

  # Create new directory
  path_new <- file.path(path, name_no_ext)
  if(dir.exists(path_new) == FALSE){dir.create(path_new)}

  # Write new file
  file_name_out <- file.path(path_new, paste(name_no_ext,"_",file_name_ext,".def",sep=""))
  # if there are comments, this should remove extra NAs
  def_table[def_table == "NA"] = " "
  write.table(def_table, file = file_name_out, row.names = FALSE, col.names = FALSE, quote=FALSE, sep="       ")
}

