#' Run multiple RHESSys simulations
#'
#' Runs RHESSys simulations.
#' @inheritParams run_rhessys_single
#' @param parallel Defaults to TRUE. Should the parallel package be used to
#'   parallelize the rhessys runs.
#' @param n_cores The number of cores to use in a parallelized cluster. If left
#'   NULL, will autodetect number of cores and use total - 1.
#' @param parallel_method Method for running rhessys in parallel. Default is
#'   'simple', which is used for running on a local machine or equivalent.
#'   "slurm' allows direct access to slurm scheduler
#' @param combine_by_linking The approach for combining multiple def files, tec
#'   files, header files, etc. Default 'multiplicity' combines all unique
#'   combinations (e.g. 2 def pars scenarios and 2 tec file scenarios = 4
#'   scenarios total). 'recycling' will repeat shorter inputs to match longer
#'   input (e.g. with 4 def_pars scenarios and 2 tec file scenarios, the 2 tec
#'   file scenarios will be repeated for a total of 4 scenarios total).
#' @param combine_by_multiplicity The approach for c
#' @param combine_by_repeating There is no easy approach for combining this. May
#'   need to be removed.
#' @param output_table_name Name for all-options dataframe that is exported.
#'   Default name is ...
#' @author Will Burke
#'
#' @export
#'


run_rhessys_multi = function(input_rhessys,
                             hdr_files,
                             tec_data,
                             def_pars = NULL,
                             clim_base = NULL,
                             output_filter = NULL,
                             combine_by_linking = NULL,
                             combine_by_multiplicity = "def",
                             combine_by_repeating = NULL,
                             output_table_name = NULL,
                             parallel = TRUE,
                             parallel_method = "simple",
                             n_cores = NULL,
                             nodes = NULL,
                             cpus_per_node = NULL,
                             rscript_path = NULL
                             ){

  # NOTES ON ADDING TO THIS FUNCTION AND NEW SOURCES OF SCENARIO VARIATION
  # - if you're just adding a different means of varying def pars, that should be outside of the funciton,
  # and just come into this function as a single def_pars object
  # - if you're adding a new source of variation - e.g. tec or clim or whatever, decide how it's going to combine
  #   - the existing infastructure below is set up to be combined multiplicitively, e.g. 2 def pars scenarios and 2 tec file scenarios
  #   lead to 4 scenarios total, VS recycling data, where all inputs must be multiples of the largest, and will be repeated,
  #   e.g. 4 def_pars scenarios and 2 tec file scenarios, with those 2 tec file scenarios being repeated, so the total is still 4.
  # - ADD YOUR VARS
  #   - to the run_parallel function (this function is just to simplify the call to parLapply(), new inputs will need
  #   to be indexed/subset like the def pars within that function so that each parallelized run uses the correct scenario info)
  #   - to the cluster export - the cluster method (PSOCK) requires objects being used to be explicitly exported,
  #   here via the clusterExport function. Add the var/object to the existing vector of names.
  # - Double check any object generation and potentially add cleanup - many runs will create a lot of garbage files potentially.

  # TODO add logic on how to combine scenarios/iterations/param sets ACROSS data objects
  # The infrastructure for this is already here - add the scenarios to the list of DFs and
  # TODO add option to only aggregate to a table and output as R object, add opposite - to use input R object
  # to run scenarios

  # list of data frames, each containing nrows where n is number of sims for each data object
  dfs = NULL

  # ---------- rhessys inputs ----------
  if (!is.null(input_rhessys)) {
    rhessys_df <- as.data.frame(input_rhessys)

    dfs <- c(dfs, rhessys = list(rhessys_df))
  }

  # ---------- hdr files ----------
  if (!is.null(hdr_files)) {
    hdr_files_df = as.data.frame(hdr_files)

    dfs <- c(dfs, hdr = list(hdr_files_df))
  }

  # ---------- tec events ----------
  if (length(tec_data) > 1) {
    tec_names <- names(tec_data)
    tec_data_df <- as.data.frame(tec_names)

    dfs <- c(dfs, tec = list(tec_data_df))
  }

  # ---------- def pars ----------
  if (!is.null(def_pars)) {
    # transform into dataframe, both for convenience and storing, tho will need to go back to list form
    def_pars_df = as.data.frame(lapply(def_pars, "[[", 3))
    def_names = sapply(def_pars, function(x) paste(x[[1]], x[[2]], sep = "::"))
    names(def_pars_df) = def_names

    dfs <- c(dfs, def = list(def_pars_df))
  }

  # ---------- combine scenarios ----------

  # Combine by linking (currently does a dumb cbind. Could be more complex with a join)
  if (!is.null(combine_by_linking)){
    dfs_linked <- dfs[names(dfs) %in% combine_by_linking]
    df <- do.call(cbind, dfs_linked)
    colnames(df) <- unlist(lapply(dfs_linked, colnames))
    rownames(df) <- 1:nrow(df)
  } else {
    df <- NULL
  }

  # Combine by multiplicity (note that if tables are linked, they are automatically passed to combine_by_multiplicity)
  if (!is.null(combine_by_multiplicity)){
    dfs_multiplicity <- c(df = list(df), dfs[names(dfs) %in% combine_by_multiplicity])
    indexes <- lapply(dfs_multiplicity, function(x) 1:nrow(x))

    # get all combinations - expand grid but with dfs_multiplicity, taken from reshape::expand.grid.df)
    grid <- do.call(expand.grid, indexes)
    df <- do.call(data.frame, mapply(function(df, index) df[index, , drop = FALSE], dfs_multiplicity, grid))
    colnames(df) <- unlist(lapply(dfs_multiplicity, colnames))
    rownames(df) <- 1:nrow(df)
  } else {
    df <- NULL
  }

  # Combine by repeating
  if (!is.null(combine_by_repeating)){
    dfs_repeated <- c(df = list(df), dfs[names(dfs) %in% combine_by_repeating])
    indexes <- lapply(dfs_repeated, function(x) 1:nrow(x))

    # Identify the longest input and test if it is a multiple of the other inputs
    l_max <- max(unlist(lapply(indexes, length)))
    if (max(unlist(lapply(indexes, function(x) l_max%%length(x)))) > 0){
      stop(paste("The length of the longest input is not a multiple of one (or more) of the others"))
    }

    # Make all inputs same length
    dfs_repeated <- lapply(dfs_repeated, function(x){
      tmp <- rep(list(x), (l_max / nrow(x)))
      dfs_repeated <- do.call(rbind, tmp)
      return(dfs_repeated)
    })
    # Combine all inputs via cbind
    df <- do.call(cbind, dfs_repeated)
    colnames(df) <- unlist(lapply(dfs_repeated, colnames))
    rownames(df) <- 1:nrow(df)
  }


  # ---------- Export df ----------
  # Maybe add date/time to name so default doesn't overwrite itself
  if (!is.null(output_table_name)){
    write.csv(df, file.path(input_rhessys$output_folder[1], output_table_name, ".csv"))
  } else {
    write.csv(df, file.path(input_rhessys$output_folder[1],"df.csv"))
  }


  # ---------- parsing functions ----------

  parse_input_rhessys <- function(input_rhessys = NULL, df, i){
    if (!is.null(input_rhessys)) {
      input_rhessys_i = as.list(df[i, names(df) %in% names(input_rhessys)])
    } else {
      input_rhessys_i = NULL
    }
    return(input_rhessys_i)
  }
  parse_hdr_files <- function(hdr_files = NULL, df, i){
    if (!is.null(hdr_files)) {
      hdr_files_i = as.list(df[i, names(df) %in% names(hdr_files)])
    } else {
      hdr_files_i = NULL
    }
    return(hdr_files_i)
  }
  parse_tec_data <- function(tec_data = NULL, df, i){
    if (!is.null(tec_data)) {
      if (length(tec_data) > 1) {
        tec_data_i <- "[["(tec_data, df[i, "tec_names"])
      } else {
        tec_data_i <- tec_data
      }
    } else {
      tec_data_i = NULL
    }
    return(tec_data_i)
  }
  parse_def_pars <- function(def_pars = NULL, df, i, def_names){
    if (!is.null(def_pars)) {
      def_pars_values = as.list(df[i, names(df) %in% def_names])
      def_pars_i = mapply(function(x, y) {x[[3]] = y; return(x)}, def_pars, def_pars_values, SIMPLIFY = F)
    } else {
      def_pars_i = NULL
    }
    return(def_pars_i)
  }

  # parse_output_filter <- function(output_filter = NULL, df, i){
  #   if (!is.null(output_filter)) {
  #     # Probably remove this section since output filter should have common name with attached numbers
  #     output_filter_i <- output_filter
  #     for (n in seq_len(length(output_filter) - 1)){
  #       output_filter_i[[n]][[1]] <- output_filter[[n]][[1]][i]
  #       output_filter_i[[n]][[2]][[1]] <- output_filter[[n]][[2]][[1]][i]
  #       output_filter_i[[n]][[2]][[2]] <- output_filter[[n]][[2]][[2]][i]
  #       output_filter_i[[n]][[2]][[3]] <- output_filter[[n]][[2]][[3]][i]
  #       output_filter_i[[n]][[3]][[1]] <- output_filter[[n]][[3]][[1]][i]
  #       output_filter_i[[n]][[3]][[2]] <- output_filter[[n]][[3]][[2]][i]
  #     }
  #     output_filter_i[[length(output_filter_i)]] <- output_filter[[length(output_filter)]][i]
  #   } else {
  #     output_filter_i = NULL
  #   }
  # }


  # ---------- dumb for loop ----------

  if (!parallel) {
    for (i in 1:nrow(df)) {

      input_rhessys_i <- parse_input_rhessys(input_rhessys = NULL, df = df, i=i)
      hdr_files_i <- parse_hdr_files(hdr_files = NULL, df = df, i=i)
      tec_data_i <- parse_tec_data(tec_data = NULL, df = df, i=i)
      def_pars_i <- parse_def_pars(def_pars = NULL, df = df, i=i, def_names = def_names)

      run_rhessys_single(
        input_rhessys = input_rhessys_i,
        hdr_files = hdr_files_i,
        tec_data = tec_data_i,
        def_pars = def_pars_i,
        clim_base = clim_base,
        output_filter = output_filter,
        runID = i
      )
    }
  }


  # ---------- parallelized: simple ----------
  if (parallel & parallel_method == "simple") {

    if (is.null(n_cores)) {
      n_cores = parallel::detectCores() - 1
    }

    run_parallel_simple = function(i,
                                   input_rhessys,
                                   hdr_files ,
                                   tec_data,
                                   def_pars,
                                   clim_base,
                                   output_filter,
                                   df,
                                   def_names = NULL){

      library(RHESSysIOinR)

      input_rhessys_i <- parse_input_rhessys(input_rhessys = NULL, df = df, i=i)
      hdr_files_i <- parse_hdr_files(hdr_files = NULL, df = df, i=i)
      tec_data_i <- parse_tec_data(tec_data = NULL, df = df, i=i)
      def_pars_i <- parse_def_pars(def_pars = NULL, df = df, i=i, def_names = def_names)

      run_rhessys_single(
        input_rhessys = input_rhessys_i,
        hdr_files = hdr_files_i,
        tec_data = tec_data_i,
        def_pars = def_pars_i,
        clim_base = clim_base,
        output_filter = output_filter,
        runID = i)
    }

    # make a psock cluster
    cl = parallel::makeCluster(n_cores)
    # manually add objects to the cluster
    parallel::clusterExport(
      cl = cl,
      varlist = c(
        "input_rhessys",
        "hdr_files",
        "tec_data",
        "def_pars",
        "clim_base",
        "output_filter",
        "df",
        "def_names"
      ),
      envir = environment()
    )
    # run run_parallel in function with parLapply
    parallel::parLapply(
      cl = cl,
      X = 1:nrow(df),
      fun = run_parallel_simple,
      input_rhessys = input_rhessys,
      hdr_files = hdr_files,
      tec_data = tec_data,
      def_pars = def_pars,
      clim_base = clim_base,
      output_filter = output_filter,
      df = df,
      def_names = def_names
    )
    # stop the cluster
    parallel::stopCluster(cl)

  }


  # ---------- parallelized: slurm ----------
  if (parallel & parallel_method == "slurm") {

    library(rslurm)

    # Initial function
    run_parallel_slurm_initial <- function(f,
                                           params,
                                           input_rhessys,
                                           hdr_files ,
                                           tec_data,
                                           def_pars,
                                           clim_base,
                                           output_filter,
                                           df,
                                           def_names,
                                           nodes,
                                           cpus_per_node,
                                           rscript_path){

      # Need to have an 'initial' slurm function because the params passed to
      # slurm_apply must all be arguments in the passed function. rslurm_apply
      # will parse slurm_initial_input into i, which can then be processed
      # 'properly' in the primary function.

      # Primary function
      run_parallel_slurm = function(i,
                                    input_rhessys,
                                    hdr_files ,
                                    tec_data,
                                    def_pars,
                                    clim_base,
                                    output_filter,
                                    df,
                                    def_names = NULL,
                                    nodes,
                                    cpus_per_node,
                                    rscript_path){

        library(RHESSysIOinR)

        input_rhessys_i <- parse_input_rhessys(input_rhessys = NULL, df = df, i=i)
        hdr_files_i <- parse_hdr_files(hdr_files = NULL, df = df, i=i)
        tec_data_i <- parse_tec_data(tec_data = NULL, df = df, i=i)
        def_pars_i <- parse_def_pars(def_pars = NULL, df = df, i=i, def_names = def_names)

        run_rhessys_single(
          input_rhessys = input_rhessys_i,
          hdr_files = hdr_files_i,
          tec_data = tec_data_i,
          def_pars = def_pars_i,
          clim_base = clim_base,
          output_filter = output_filter,
          runID = i)
      }

      # Call function
      run_parallel_slurm(i,
                         input_rhessys = input_rhessys,
                         hdr_files = hdr_files,
                         tec_data = tec_data,
                         def_pars = def_pars,
                         clim_base = clim_base,
                         output_filter = output_filter,
                         df = df,
                         def_names = def_names,
                         nodes = nodes,
                         cpus_per_node = cpus_per_node,
                         rscript_path = rscript_path)
    }

    # ----

    slurm_initial_input <- data.frame(i = seq_len(nrow(df)))

    sjob <- rslurm::slurm_apply(f = run_parallel_slurm_initial,
                                params = slurm_initial_input,
                                input_rhessys = input_rhessys,
                                hdr_files = hdr_files,
                                tec_data = tec_data,
                                def_pars = def_pars,
                                clim_base = clim_base,
                                output_filter = output_filter,
                                df = df,
                                def_names = def_names,
                                nodes = nodes,
                                cpus_per_node = cpus_per_node,
                                rscript_path = rscript_path)

  }
}


