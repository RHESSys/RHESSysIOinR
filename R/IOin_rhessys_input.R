#' IOin_rhessys_input
#'
#' Basic inputs to run RHESSys
#'
#' @param version Path to compiled RHESSys binary.
#' @param tec_file Path and name of input tec file.
#' @param world_file Path and name of input world file.
#' @param world_hdr_prefix Prefix for folder and temporary header files to be written.
#' @param flowtable Path and name of input tec file.
#' @param start Start date of simulation.
#' @param end End date of simulation.
#' @param output_folder Path to folder where simulation output will go.
#' @param output_prefix Prefix for output files.
#' @param commandline_options Commandline options to be passed to RHESSys, e.x. '-g' or '-p'
#'
#' @author Will Burke
#'
#' @export

IOin_rhessys_input = function(version,
                              tec_file,
                              world_file,
                              world_hdr_prefix,
                              flowtable,
                              start,
                              end,
                              output_folder,
                              output_prefix,
                              commandline_options) {
  rh_list = list()

  # TODO check that a binary exists at specified location - no error, just warning maybe
  # TODO this could be built on to automatically pull and compile rhessys
  rh_list$rhessys_version <- version

  # TODO since tec_file can be built via inputs, could check for it existing, but no error.
  # TODO Could potentially use this input tec file,  if it exists, as a basis for a new tec file, so only modificaitons would need to be added
  # in the IOin_tec_ function. That actually should probably stay/occur in that file.
  # There is potential for conflict here though so a check should happen in the run_rhessys_core or scenario iteration functions
  rh_list$tec_file <- tec_file

  # TODO check that it exists, warn if no
  rh_list$world_file <- world_file
  # this is unclear what the purpose is -  we could just delete this and do it all automatically using scenario IDs we generate
  rh_list$world_hdr_prefix <- world_hdr_prefix

  # TODO check it exsists, warn if no
  rh_list$flow_file <- flowtable

  # TODO start and end dates, should have automatic coversion to rhessys format from R Date at least
  rh_list$start_date <- start
  rh_list$end_date <- end

  # TODO check if exists, create if not
  rh_list$output_folder <- output_folder

  rh_list$output_filename <- output_prefix

  # TODO compare against a vector of known commandline options, warn if not in that list
  rh_list$command_options <- commandline_options

  return(rh_list)


}
