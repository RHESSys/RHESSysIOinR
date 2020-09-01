#' output_control
#'
#' Passes output variables to appropriate output method - eventually should put all the output code in here directly maybe?
#' @inheritParams run_rhessys2
#' @inheritParams select_output_variables_R

output_control = function(output_method,
                          output_variables,
                          return_data,
                          output_folder,
                          output_filename,
                          run = 1) {

  if (!is.null(output_variables[1]) & !is.null(output_method)) {

    # make allsim folder in output location - won't overwrite, warning for if exists is supressed
    dir.create(file.path(input_rhessys$output_folder, "allsim"),
               showWarnings = FALSE)

    if (output_method == "awk") {
      select_output_variables_w_awk(
        output_variables = output_variables,
        output_folder = output_folder,
        run = run,
        output_initiation = 1
      )

    } else if (output_method == "r") {
      data_out = select_output_variables_R(
        output_variables = output_variables,
        output_folder = output_folder,
        output_filename = output_filename,
        run = run,
        return_data = return_data
      )
      return(data_out)

    } else {
      select_output_variables(
        output_variables = output_variables,
        output_folder = input_rhessys$output_folder,
        run = run,
        output_initiation = output_initiation
      )

    }

    return()

  }

}
