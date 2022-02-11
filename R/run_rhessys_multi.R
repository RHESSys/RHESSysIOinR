#' Run multiple RHESSys simulations
#'
#' Runs multiple RHESSys simulations via for loop or in parallel
#'
#' @section Note: For multiple tec files, a named list of dataframes produced by
#'   an IOin_tec function should be passed to tec_data. For hdr files, stratum
#'   variables for each unique run must now be a single quoted string separated
#'   by commas (e.g.
#'   "path/veg_tree.def,path/veg_understory.def,path/veg_shrub.def")
#'
#'   With the current code, All four options for combining ('rhessys', 'tec',
#'   'hdr', and 'def') must be specified in either combine_by_linking or
#'   combine_by_multiplicity. The default option is used when only def file
#'   parameters are varied.
#'
#'   The slurm option automatically enables processing with a slurm scheduler
#'   via the r package rslurm. (https://github.com/SESYNC-ci/rslurm). In order
#'   to run rhessys via rslurm, two templates within the rslurm package need to
#'   be modified. First, in the submission template
#'   (rslurm/templates/submit_sh.txt), optional environment variables may need
#'   to be added between the 'options' and 'rscript' line (e.g. module load
#'   anaconda3; conda activate <env>). Second, the r template
#'   (rslurm/templates/slurm_run_R.txt) requires a 'setwd(“..”)' before the
#'   do.call line. This will correct the home directory to be consistent with
#'   the 'simple' parallel approach.
#'
#' @inheritParams run_rhessys_single
#' @param combine_by_linking Option for combining 'rhessys', 'tec', 'hdr',
#'   and/or 'def' files by linking files with of the same number of inputs.
#'   (e.g. with 2 def_pars scenarios and 2 tec file scenarios = 2 scenarios
#'   total). Currently joined by a cbind, so the order of each variable must
#'   match.
#' @param combine_by_multiplicity Option for combining 'rhessys', 'tec', 'hdr',
#'   'def' and/or previous 'combine_by_linking' files by multiplicity, which
#'   combines all unique combinations of inputs (e.g. 2 def pars scenarios and 2
#'   tec file scenarios = 4 scenarios total).
#' @param all_combinations_output_name Name for all-options csv dataframe that
#'   is exported. Default name is "all_combinations_table" and is located in
#'   rhessys output folder.
#' @param parallel Defaults to TRUE. Should the parallel package be used to
#'   parallelize the rhessys runs. FALSE processes runs via a for loop.
#' @param parallel_method Method for running rhessys in parallel. Default is
#'   'simple', which can be used for running on a local machine or equivalent.
#'   "slurm' allows direct access to slurm scheduler
#' @param n_cores The number of cores to use in a parallelized cluster. If left
#'   NULL, will autodetect number of cores and use total - 1.
#' @param nodes The number of cluster nodes to spread the calculation over.
#'   \code{slurm_apply} automatically divides \code{params} in chunks of
#'   approximately equal size to send to each node.
#' @param cpus_per_node The number of CPUs requested per node.
#' @param rscript_path The location of the Rscript command. If not specified,
#'   defaults to the location of Rscript within the R installation being run.
#' @param slurm_options A named list of options recognized by \code{sbatch}; see
#'   rslurm slurm_apply function for more details.
#'
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
                             combine_by_linking = c("rhessys", "tec", "hdr"),
                             combine_by_multiplicity = "def",
                             all_combinations_output_name = "all_combinations_table",
                             parallel = TRUE,
                             parallel_method = "simple",
                             n_cores = NULL,
                             nodes = NULL,
                             cpus_per_node = NULL,
                             rscript_path = NULL,
                             slurm_options = NULL
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
  df = NULL

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
  if(!is.data.frame(tec_data)){
    tec_names <- names(tec_data)
    tec_data_df <- as.data.frame(tec_names)
    dfs <- c(dfs, tec = list(tec_data_df))
  } else {
    dfs <- c(dfs, tec = list(data.frame(tec = "tec")))
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
  }

  # Combine by multiplicity (note that if tables are linked, they are automatically passed to combine_by_multiplicity)
  if (!is.null(combine_by_multiplicity)){
    if(is.null(df)){dfs_multiplicity <- c(dfs[names(dfs) %in% combine_by_multiplicity])}
    if(!is.null(df)){dfs_multiplicity <- c(df = list(df), dfs[names(dfs) %in% combine_by_multiplicity])}
    indexes <- lapply(dfs_multiplicity, function(x) 1:nrow(x))

    # get all combinations - expand grid but with dfs_multiplicity, taken from reshape::expand.grid.df)
    grid <- do.call(expand.grid, indexes)
    df <- do.call(data.frame, mapply(function(df, index) df[index, , drop = FALSE], dfs_multiplicity, grid))
    colnames(df) <- unlist(lapply(dfs_multiplicity, colnames))
    rownames(df) <- 1:nrow(df)
  }


  # ---------- Export df ----------
  # Maybe add date/time to name so default doesn't overwrite itself?
  write.csv(df, file.path(input_rhessys$output_folder[1], all_combinations_output_name, ".csv"))


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
      if(!is.data.frame(tec_data)){
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


  # ---------- dumb for loop ----------

  if (!parallel) {
    for (i in 1:nrow(df)) {

      input_rhessys_i <- parse_input_rhessys(input_rhessys = input_rhessys, df = df, i=i)
      hdr_files_i <- parse_hdr_files(hdr_files = hdr_files, df = df, i=i)
      tec_data_i <- parse_tec_data(tec_data = tec_data, df = df, i=i)
      def_pars_i <- parse_def_pars(def_pars = def_pars, df = df, i=i, def_names = def_names)

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


  # ---------- parallelize: simple ----------
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
                                   def_names = NULL,
                                   parse_input_rhessys,
                                   parse_hdr_files,
                                   parse_tec_data,
                                   parse_def_pars){

      library(RHESSysIOinR)

      input_rhessys_i <- parse_input_rhessys(input_rhessys = input_rhessys, df = df, i=i)
      hdr_files_i <- parse_hdr_files(hdr_files = hdr_files, df = df, i=i)
      tec_data_i <- parse_tec_data(tec_data = tec_data, df = df, i=i)
      def_pars_i <- parse_def_pars(def_pars = def_pars, df = df, i=i, def_names = def_names)

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
      varlist = c("input_rhessys", "hdr_files",
                  "tec_data", "def_pars",
                  "clim_base", "output_filter",
                  "df", "def_names",
                  "parse_input_rhessys", "parse_hdr_files",
                  "parse_tec_data", "parse_def_pars"),
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
      def_names = def_names,
      parse_input_rhessys = parse_input_rhessys,
      parse_hdr_files = parse_hdr_files,
      parse_tec_data = parse_tec_data,
      parse_def_pars = parse_def_pars
    )
    # stop the cluster
    parallel::stopCluster(cl)

  }


  # ---------- parallelize: slurm ----------
  if (parallel & parallel_method == "slurm") {

    library(rslurm)

    run_parallel_slurm <- function(i){
      # Note: only i is passed directly as a parameter. All others passed via 'global objects'.

      library(RHESSysIOinR)

      input_rhessys_i <- parse_input_rhessys(input_rhessys = input_rhessys, df = df, i=i)
      hdr_files_i <- parse_hdr_files(hdr_files = hdr_files, df = df, i=i)
      tec_data_i <- parse_tec_data(tec_data = tec_data, df = df, i=i)
      def_pars_i <- parse_def_pars(def_pars = def_pars, df = df, i=i, def_names = def_names)

      run_rhessys_single(
        input_rhessys = input_rhessys_i,
        hdr_files = hdr_files_i,
        tec_data = tec_data_i,
        def_pars = def_pars_i,
        clim_base = clim_base,
        output_filter = output_filter,
        runID = i)
    }


    # ----

    slurm_initial_input <- data.frame(i = seq_len(nrow(df)))

    sjob <- rslurm::slurm_apply(f = run_parallel_slurm,
                                params = slurm_initial_input,
                                nodes = nodes,
                                cpus_per_node = cpus_per_node,
                                rscript_path = rscript_path,
                                global_objects = c("input_rhessys", "hdr_files",
                                                   "tec_data", "def_pars",
                                                   "clim_base", "output_filter",
                                                   "df", "def_names",
                                                   "parse_input_rhessys", "parse_hdr_files",
                                                   "parse_tec_data", "parse_def_pars"),
                                slurm_options = slurm_options)
  }
}


