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
#' @param output_filter Path to a yaml formatted output filter.
#' @param command_options RHESSys command line options, ex. '-g' or '-p'.
#' @param prefix_command A shell command to be run previous to the RHESSys command line call.
#' @param return_cmd true/false passed from run_rhessys_single
# @param supress_console TRUE/FALSE if console output from system() should be supressed
#' This can be used to source a shell script, which itself can run multiple commands if needed.
#'
#' @export

rhessys_command <- function(rhessys_version,
                            world_file,
                            world_hdr_file,
                            tec_file,
                            flow_file,
                            start_date,
                            end_date,
                            output_file = NULL,
                            input_parameters,
                            output_filter = NULL,
                            par_option_ID = NULL,
                            command_options,
                            prefix_command = NULL,
                            return_cmd = FALSE) {

  tmp = paste0(
    rhessys_version,
    " -w ", world_file,
    " -whdr ", world_hdr_file,
    " -t ", tec_file,
    " -r ", flow_file,
    " -st ", start_date,
    " -ed ", end_date
  )

  if (!is.null(output_file)) {
    tmp = paste0(tmp, " -pre ", output_file)
  }
  if (!is.null(output_filter)) {
    tmp = paste0(tmp, " -of ", output_filter)
  }
  if (!is.null(par_option_ID)) {
    tmp = paste0(tmp, " -par ", par_option_ID)
  }

  if (!is.null(input_parameters)) {
    tmp = paste0(tmp, " ", input_parameters)
  }

  if (length(command_options) > 0) {
    tmp = paste0(tmp, " ", command_options)
  }

  # add prefix command optionally
  if (!is.null(prefix_command)) {
    tmp = paste0(prefix_command, "; ", tmp)
  }

  # check OS and run via system correctly - windows requires the linux subsystem
  # TODO add check for WSL installation
  if (.Platform$OS.type == "windows") {
    # tmp = noquote(paste("bash -c \"", tmp, "\"", sep = ""))
    tmp = noquote(paste("wsl ", tmp, sep = ""))
  }

  cat("Command line echo:", tmp, "\n")

  if (return_cmd) {
    return(tmp)
  } else {
    cat("\n----------------------------------------\n")
    cat("===== Beginning RHESSys Simulation =====\n")
    cat("----------------------------------------\n\n")

    system(tmp)

    return(NULL)
  }

}

