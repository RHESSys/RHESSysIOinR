#' Executes single RHESSys run on command line
#'
#' \code{rhessys_command} calls rhessys on the command line.
#'
#' Requires that supporting files be developed prior to running RHESSys. These
#' include ...
#'
#' @param rhessys_version path and file of compiled version of RHESSys.
#' @param tec_file
#' @param world_file path and file of world_file. World_file is produced using
#'   grass-to-world
#' @param world_hdr_file ???
#' @param flow_file ???
#' @param start_date ???
#' @param end_date ???
#' @param output_folder ???
#' @param output_filename ???
#' @param command_options ???
#' @param m ???
#' @param k ???
#' @param m_v ???
#' @param k_v ???
#' @param pa ???
#' @param po ???
#' @param gw1 ???
#' @param gw2 ???


#' @export
rhessys_command <- function(rhessys_version, tec_file, world_file, world_hdr_file,
                           flow_file, start_date, end_date, output_folder,
                           output_filename, command_options,
                           m, k, m_v, k_v, pa, po, gw1, gw2){

  tmp = sprintf("%s -t %s -w %s -whdr %s -r %s -st %s -ed %s -pre %s -s %f %f -sv %f %f -svalt %f %f -gw %f %f %s",
                rhessys_version, tec_file, world_file, world_hdr_file, flow_file,
                start_date, end_date, paste(output_folder,"/",output_filename,sep=""),
                m, k, m_v, k_v, pa, po, gw1, gw2, command_options)

  system(tmp, ignore.stderr = T)
}

