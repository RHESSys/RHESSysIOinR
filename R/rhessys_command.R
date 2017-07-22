#' Executes single RHESSys run on command line
#'
#' \code{rhessys_command} calls rhessys on the command line.
#'
#' Requires that supporting files be developed prior to running RHESSys. These
#' include ...
#'
#' @param rhessys_version path and file of compiled version of RHESSys.
#' @param world_file path and file of world_file. World_file is produced using
#'   grass-to-world
#' @param world_hdr_file ???
#' @param tec_file ???
#' @param flow_file ???
#' @param start_date ???
#' @param end_date ???
#' @param output_file ???
#' @param input_parameters ???
#' @param command_options ???
#'
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
                            command_options){

  tmp <- sprintf("%s -w %s -whdr %s -t %s -r %s -st %s -ed %s -pre %s %s %s",
                 rhessys_version,
                 world_file,
                 world_hdr_file,
                 tec_file,
                 flow_file,
                 start_date,
                 end_date,
                 output_file,
                 input_parameters,
                 command_options)

  system(tmp)
}

