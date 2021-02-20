#' Run multiple RHESSys simulations
#'
#' Runs RHESSys simulations.
#' @inheritParams run_rhessys_single
#' @param parallel Defaults to TRUE. Should the parallel package be used to parallelize the rhessys runs.
#' @param n_cores The number of cores to use in a parallelized cluster.
#' @author Will Burke
#'
#' @export
#'


run_rhessys_multi = function(input_rhessys,
                             hdr_files,
                             tec_data,
                             def_pars = NULL,
                             output_filter = NULL,
                             runID = NULL,
                             parallel = TRUE,
                             n_cores = 2) {

  # TODO add logic on how to combine scenarios/iterations/param sets ACROSS data objects
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

    run_parallel = function(i,
                            input_rhessys,
                            hdr_files ,
                            tec_data,
                            df,
                            def_pars,
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
        output_filter = output_filter,
        runID = i
      )

    }

    cl = parallel::makeCluster(n_cores)
    parallel::clusterExport(cl, c("df", "input_rhessys", "hdr_files", "tec_data", "def_pars"))
    parallel::parLapply(cl = cl, X = 1:nrow(df), fun = run_parallel, input_rhessys, hdr_files, tec_data, df, def_pars)
    parallel::stopCluster(cl)
  }


}
