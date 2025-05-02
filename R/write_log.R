#' write_log
#' 
#' write or append to a log file, by row

write_log = function (input_rhessys,
                      output_filter,
                      return_cmd,
                      run_ct,
                      log_loc = "~/rhessys_run_log.csv") {

# assumes output filters
if (!is.null(output_filter) & is.list(output_filter)) {
output_loc = unique(lapply(output_filter[1:length(output_filter)-1], function(X) X$output$path))[1]
} else {
  output_loc = NA
}

if(is.null(input_rhessys$world_hdr_path)) {
  headerpath = input_rhessys$world_hdr_prefix
} else {
  headerpath = file.path(input_rhessys$world_hdr_path, input_rhessys$world_hdr_prefix)
}

# trying to keep this as short as possible
log_tab = data.frame(
  Run_Count = run_ct,
  Date = Sys.Date(), 
  Time = format(Sys.time(), "%I:%M:%S %p"),
  Working_Dir = getwd(),
  Worldfile = input_rhessys$world_file,
  Flowtable = input_rhessys$flow_file,
  Header = headerpath,
  Tecfile = input_rhessys$tec_file,
  Start_Date = input_rhessys$start_date,
  End_Date = input_rhessys$end_date,
  Cmd_Opts = ifelse(is.null(input_rhessys$command_options), NA, input_rhessys$command_options)
)
if (!file.exists(log_loc)) {
  write.table(log_tab,
    file = log_loc,
    sep = ",",
    row.names = FALSE,
    col.names = TRUE,
    quote = FALSE)
  cat("Wrote first time log to: ",log_loc,"\n")
} else {
  write.table(log_tab,
    file = log_loc,
    sep = ",",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE,
    append = TRUE)
    cat("Appended log: ",log_loc,"\n")
}

return(NULL)

}
