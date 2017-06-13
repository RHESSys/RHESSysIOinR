#' Selects parameter (and variables) to be dynamically searched
#'
#' This function calls changes  awk file for substituting parameter values in RHESSys.
#' May also be used for substituting variables in worldfiles.
#'
#' @param par_value ??
#' @param awk_file ??
#' @param input_file ??
#' @param output_folder ??
#'
#'
#' @export
change_parameters <- function(par_set_groups, par_name, def_file, ){

  # Read in def files
  read.table()




  tmp <- sprintf("awk -f %s par=%f < %s > %s", awk_file, par_value, input_file, output_folder)
  system(tmp)


  a <- var_names %>%
    sapply(., function(., path) file.path(path, .), path=path) %>%
    lapply(., read.table, header = FALSE, skip = 2)


}





#gsub("e", "", group$group)


# Should I call each def file?



