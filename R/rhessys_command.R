#' Executes single RHESSys run on command line
#'
#' \code{rhessys_command} Assembles command line RHESSys call, and runs it. See the RHESSys wiki:
#' https://github.com/RHESSys/RHESSys/wiki/RHESSys-command-line-options
#' @param rhessys_version Path and file name of compiled version of RHESSys.
#' @param world_file Oath and file name of RHESSys world_file.
#' @param world_hdr_file Path and file name of RHESSys header file
#' @param tec_file Path and file name of RHESSys temporal event control (tec) file
#' @param flow_file Path and file name of RHESSys flow table file
#' @param start_date Start date character vector in format <year> <month> <day> <hour>, delimited by spaces. Ex. '1990 12 30 01'
#' @param end_date End date character vector, same format as start_date
#' @param output_file Path and base file name of RHESSys output
#' @param input_parameters Soil parameters passed to RHESSys command line.
#' @param command_options RHESSys command line options, ex. '-g' or '-p'.
#'
#' @export

rhessys_command <- function(rhessys_version,
                            world_file,
                            world_hdr_file,
                            tec_file,
                            flow_file,
                            start_date,
                            end_date,
                            output_file,
                            input_parameters,
                            command_options) {

  # handles NULLS better
  tmp = paste0(rhessys_version, " -w ", world_file, " -whdr ",world_hdr_file, " -t ", tec_file, " -r ", flow_file,
               " -st ", start_date, " -ed ", end_date, " -pre ", output_file, " ", input_parameters, " ", command_options)

  # check OS and run via system correctly - windows requires the linux subsystem
  if(.Platform$OS.type=="windows"){
    tmp = noquote(paste("bash -c \"",tmp,"\"",sep=""))
  }

  print(paste("Command line echo:",tmp),quote = FALSE)

  system(tmp)

}

