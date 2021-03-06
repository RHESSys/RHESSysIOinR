#' Run multiple RHESSys simulations
#'
#' Runs RHESSys simulations.
#' @inheritParams run_rhessys_single
#' @param parallel Defaults to TRUE. Should the parallel package be used to parallelize the rhessys runs.
#' @param n_cores The number of cores to use in a parallelized cluster. If left NULL, will autodetect number of cores and use total - 1.
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
                             parallel = TRUE,
                             n_cores = NULL) {

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

  # ---------- def pars ----------
  if (!is.null(def_pars)) {
    # transform into dataframe, both for convenience and storing, tho will need to go back to list form
    def_pars_df = as.data.frame(lapply(def_pars, "[[", 3))
    def_names = sapply(def_pars, function(x) paste(x[[1]], x[[2]], sep = "::"))
    names(def_pars_df) = def_names

    dfs <- c(dfs, list(def_pars_df))

  }

  # ---------- tec events ----------
  # add here when tec event scenarios/iterations get added

  # ---------- clim sequences ----------

  # ---------- combine scenarios ----------
  # get all combinations - expand grid but with dfs, taken from reshape::expand.grid.df
  indexes <- lapply(dfs, function(x) 1:nrow(x))
  grid <- do.call(expand.grid, indexes)
  df <- do.call(data.frame, mapply(function(df, index) df[index, , drop = FALSE], dfs, grid))
  colnames(df) <- unlist(lapply(dfs, colnames))
  rownames(df) <- 1:nrow(df)

  # ---------- dumb for loop ----------
  if (!parallel) {
    for (i in 1:nrow(df)) {

      if (!is.null(def_pars)) {
        def_pars_values = as.list(df[i, names(df) %in% def_names])
        def_pars_i = mapply(function(x, y) {x[[3]] = y; return(x)}, def_pars, def_pars_values, SIMPLIFY = F)
      } else {
        def_pars_i = NULL
      }

      run_rhessys_single(
        input_rhessys = input_rhessys,
        hdr_files = hdr_files,
        tec_data = tec_data,
        def_pars = def_pars_i,
        output_filter = output_filter,
        runID = i
      )
    }
  }



  # ---------- parallelized ----------
  if (parallel) {

    if (is.null(n_cores)) {
      n_cores = parallel::detectCores() - 1
    }

    run_parallel = function(i,
                            input_rhessys,
                            hdr_files ,
                            tec_data,
                            df,
                            def_pars,
                            clim_base,
                            output_filter) {

      library(RHESSysIOinR)

      if (!is.null(def_pars)) {
        def_pars_i = mapply(function(x, y) {x[[3]] = y; return(x)}, def_pars, df[i, ], SIMPLIFY = F)
      } else {
        def_pars_i = NULL
      }

      run_rhessys_single(
        input_rhessys = input_rhessys,
        hdr_files = hdr_files,
        tec_data = tec_data,
        def_pars = def_pars_i,
        clim_base = clim_base,
        output_filter = output_filter,
        runID = i
      )

    }

    # make a psock cluster
    cl = parallel::makeCluster(n_cores)
    # manually add objects to the cluster
    parallel::clusterExport(
      cl = cl,
      varlist = c(
        "df",
        "input_rhessys",
        "hdr_files",
        "tec_data",
        "def_pars",
        "clim_base",
        "output_filter"
      ),
      envir = environment()
    )
    # run run_parallel in function with parLapply
    parallel::parLapply(
      cl = cl,
      X = 1:nrow(df),
      fun = run_parallel,
      input_rhessys = input_rhessys,
      hdr_files = hdr_files,
      tec_data = tec_data,
      df = df,
      def_pars = def_pars,
      clim_base = clim_base,
      output_filter = output_filter
    )
    # stop the cluster
    parallel::stopCluster(cl)


  }


}
