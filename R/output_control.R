#' output_control
#'
#' Passes output variables to appropriate output selection method.
#' @param output_method Output method (function) to use. "awk" will use awk, "r" will use new R based method,
#'  any other non NULL input will use the older R based output selection.
#' @param output_variables Datafrane with two named columns: "variables" containing variables of interest
#' found in the header of standard rhessys output files, "out_file" points to the files containing the associated variables,
#' this can be either the path to that file, can use the abbreviation for the different output types
#' (patch daily = pd, patch daily grow = pdg etc.) and the files in output_folder will be parsed appropriately,
#' or can use the fully written out space.time suffix, e.g. "patch.daily" or "grow_patch.daily"
#' @param output_folder Folder where rhessys output is located
#' @param output_filename Base file name of standard rhessys output
#' @param runID Integer ID of the current run, used internally
#' @inheritParams select_output_variables_R

# if output function has a new arg, add function to inheritsParams

output_control = function(output_method,
                          output_variables,
                          return_data,
                          output_folder,
                          output_filename,
                          runID = 1) {

  if (!is.null(output_variables[1]) & !is.null(output_method)) {

    # make allsim folder in output location - won't overwrite, warning for if exists is supressed
    dir.create(file.path(input_rhessys$output_folder, "allsim"),
               showWarnings = FALSE)

    if (output_method == "awk") {
      select_output_variables_w_awk(
        output_variables = output_variables,
        output_folder = output_folder,
        run = runID,
        output_initiation = 1
      )

    } else if (output_method == "r") {
      data_out = select_output_variables_R(
        output_variables = output_variables,
        output_folder = output_folder,
        output_filename = output_filename,
        run = runID,
        return_data = return_data
      )
      return(data_out)

    } else {
      select_output_variables(
        output_variables = output_variables,
        output_folder = input_rhessys$output_folder,
        run = runID,
        output_initiation = output_initiation
      )

    }

    return()

  }

}
