#' Executes single RHESSys run on command line
#'
#' \code{rhessys_command} Assembles command line RHESSys call, and runs it. See the RHESSys wiki:
#' https://github.com/RHESSys/RHESSys/wiki/RHESSys-command-line-options
#' @param rhessys_version Path and file name of compiled version of RHESSys. When
#' \code{docker_image} is used, this must be the path to the RHESSys binary *inside
#' the image* (e.g. baked in at build time), not a path on the host.
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
#' @param docker_image Name (and optionally tag) of a docker image containing RHESSys, e.g. \code{"rhessys:develop"}. If supplied, a fresh, disposable container is started for this single run via \code{docker run --rm} (no \code{--name} is set, so many calls can be launched in parallel without colliding). \code{docker_host_dir} is bind-mounted into the container so the relative file paths (world_file, tec_file, etc.) resolve correctly, and the container is removed automatically when the run finishes.
#' @param docker_host_dir Path on the host machine to bind-mount into the container - this should (probably) be your project directory, the directory the relative file paths (world_file, tec_file, etc.) are written relative to. Defaults to the current working directory (\code{getwd()}) if NULL. Converted to an absolute path internally, since docker requires this for bind mounts.
#' @param docker_container_dir Path inside the container to mount \code{docker_host_dir} to, and the working directory the RHESSys call is run from inside the container. Defaults to \code{"/mnt/rhessys_run"} if NULL.
#' @param docker_run_options Optional additional flags appended to \code{docker run}, e.g. \code{"--cpus=2 --memory=4g"} to cap resources for a given parallel run.
#' @param return_cmd true/false should the command be returned as a string instead of executed. This is useful for debugging or if you want to run the command outside of R.
# @param supress_console TRUE/FALSE if console output from system() should be supressed
# This can be used to source a shell script, which itself can run multiple commands if needed.
#'
#' @export

rhessys_command <- function(
  rhessys_version,
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
  docker_image = NULL,
  docker_host_dir = NULL,
  docker_container_dir = NULL,
  docker_run_options = NULL,
  return_cmd = FALSE
) {
  
  tmp <- paste0(
    rhessys_version,
    " -w ",
    world_file,
    " -whdr ",
    world_hdr_file,
    " -t ",
    tec_file,
    " -r ",
    flow_file,
    " -st ",
    start_date,
    " -ed ",
    end_date
  )

  if (!is.null(output_file)) {
    tmp <- paste0(tmp, " -pre ", output_file)
  }
  if (!is.null(output_filter)) {
    tmp <- paste0(tmp, " -of ", output_filter)
  }
  if (!is.null(par_option_ID)) {
    tmp <- paste0(tmp, " -par ", par_option_ID)
  }

  if (!is.null(input_parameters)) {
    tmp <- paste0(tmp, " ", input_parameters)
  }

  if (length(command_options) > 0) {
    tmp <- paste0(tmp, " ", command_options)
  }

  # add prefix command optionally
  if (!is.null(prefix_command)) {
    tmp <- paste0(prefix_command, "; ", tmp)
  }

  # check execution target, in order of precedence: docker > windows/WSL > local
  if (!is.null(docker_image)) {
    # Spin up a fresh, disposable container for this one run. This is called
    # directly via the docker CLI, which is available on Windows/Mac/Linux alike
    # once Docker Desktop (or the docker engine) is installed - so no WSL wrapping
    # is needed here even on Windows hosts. No --name is set, so this is safe to
    # call many times in parallel (e.g. from multiple R processes/workers).

    if (is.null(docker_host_dir)) {
      docker_host_dir <- getwd()
    }
    # docker requires an absolute path for bind mounts
    docker_host_dir <- normalizePath(docker_host_dir, mustWork = TRUE)

    if (is.null(docker_container_dir)) {
      docker_container_dir <- "/mnt/rhessys_run"
    }

    run_opts <- if (!is.null(docker_run_options)) {
      paste0(" ", docker_run_options)
    } else {
      ""
    }

    cmd <- paste0(
      "docker run --rm",
      run_opts,
      " -v ",
      shQuote(docker_host_dir),
      ":",
      shQuote(docker_container_dir),
      " -w ",
      shQuote(docker_container_dir),
      " ",
      docker_image,
      " bash -lc ",
      shQuote(tmp)
    )
  } else if (.Platform$OS.type == "windows") {
    wsl_loc <- Sys.which("wsl") # just to check if wsl is available
    if (wsl_loc == "") {
      stop(
        "WSL not found on system - cannot run RHESSys command on Windows without WSL."
      )
    }
    # system2("wsl", c("bash", "-lc", shQuote(cmd)))
    cmd <- paste0("wsl bash -lc \"", tmp, "\"")
  } else {
    cmd <- tmp
  }

  cat("Command line echo:", cmd, "\n")

  if (return_cmd) {
    return(cmd)
  } else {
    cat("\n----------------------------------------\n")
    cat("===== Beginning RHESSys Simulation =====\n")
    cat("----------------------------------------\n\n")

    system(cmd)

    return(NULL)
  }
}