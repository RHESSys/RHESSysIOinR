#' Adds a dated sequence to the bottom of a clim base file
#'
#' This function does ...
#'
#' @param ??? ???
#' @param ??? ???
#' @param ??? ???
#'
#'
#' @export
make_clim_base_file <- function(input_clim_base,
                                clim_base_path,
                                input_dated_seq,
                                clim_dated_ext){

  # Create core part of climate base file
  clim_base_core <- do.call(bind_rows, input_clim_base)

  # Add (optional) dated sequence
  if (is.null(input_dated_seq)==FALSE){

    # Generate paths and names
    path <- dirname(clim_base_path)
    name_no_ext <- tools::file_path_sans_ext(basename(clim_base_path))
    ext <- tools::file_ext(clim_base_path)
    dated_name_ext <- paste(input_dated_seq$name, "_", clim_dated_ext, sep="")
    dated_path_name_ext <- file.path(path,dated_name_ext)
    dated_file_name <- paste(dated_name_ext, ".", input_dated_seq$type, sep="")
    dated_file_count <- length(unique(input_dated_seq$name))
    dated_file_type <- unique(input_dated_seq$type)

    # Create new directory
    path_new <- file.path(path, name_no_ext)
    if(dir.exists(path_new) == FALSE){dir.create(path_new)}

    # Tack on dated sequence to climate base file
    c1 <- c(dated_path_name_ext, dated_file_count, dated_file_type)
    clim_base_dated <- data.frame(c1 = c1, c2 = rep("", length(c1)), stringsAsFactors=FALSE)
    clim_base_file <- bind_rows(clim_base_core,clim_base_dated)

    # Output clim base file
    file_name_out <- file.path(path_new, paste(name_no_ext,"_",clim_dated_ext,".",ext,sep=""))
    write.table(clim_base_file, file = file_name_out, row.names = FALSE, col.names = FALSE, quote=FALSE, sep="       ")

    # Make and output dated sequence file
    dated_seq_file <- file.path(path_new, dated_file_name)
    make_dated_seq(input_dated_seq = dplyr::select(input_dated_seq,-name,-type), dated_seq_file = dated_seq_file)

  } else {

    # Output standard clim file
    write.table(clim_base_core, file = clim_base_path, row.names = FALSE, col.names = FALSE, quote=FALSE, sep="       ")
  }
}

