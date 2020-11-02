#' Run RHESSys simulations
#'
#' Runs RHESSys simulations.
#' @inheritParams run_rhessys_core
#' @author Will Burke
#'
#' @export
#'

# --- NOTES ---
# Feel free to change the name and really anytthing about this function, it's mostly here to show the hypothetical structure -WB


# some testing
def_pars = input_def_pars
def_pars = input_def_parsx1
def_pars = NULL

std_pars = input_std_pars
std_pars = input_std_parsx1



run_rhessys_sims = function(input_rhessys,
                            hdr_files,
                            std_pars,
                            tec_data,
                            def_pars = NULL,
                            clim_base = NULL,
                            output_method = NULL,
                            output_variables = NULL,
                            return_data = FALSE,
                            runID = NULL) {

  # TODO add logic on how to combine scenarios/iterations/param sets ACROSS data objects
  # e.g. if def_pars has 10 values for each paramter, there shouldbe 10 RHESSys runs. but if std_pars has 5 values for each parameter
  # do we multiply to cover all combinations to make 50 total iterations, or repeat the std_pars to only have 10?
  # Should proabaly default to all combinations but allow user control

  # TODO add option to only aggregate to a table and output as R object (with a custom attribute), add opposite - to use input R object
  # to run scenarios

  # --- std pars ---
  # check that all required inputs are in the names
  std_pars_df = as.data.frame(std_pars)
  if (any(!c("m", "k", "m_v", "k_v", "pa", "po", "gw1", "gw2") %in% names(std_pars_df))) {
    stop("Input standard paramters must include: 'm', 'k', 'm_v', 'k_v', 'pa', 'po', 'gw1', and 'gw2' ")
  }
  # TODO add check for same number of std pars?

  # start list of dfs of inputs to include in making scenarios
  dfs = list(std_pars_df)

  # --- def pars ---
  # check if there are def pars
  if (!is.null(def_pars)) {
    # TODO check that there are same number of def pars

    # transform into dataframe, both for convenience and storing, tho will need to go back to list form
    def_pars_df = as.data.frame(lapply(def_pars, "[[", 3))
    def_names = sapply(def_pars, function(x) paste(x[[1]], x[[2]], sep = "::"))
    names(def_pars_df) = def_names

    dfs <- c(dfs, list(def_pars_df))

  } else {
    # i think do nothing
  }

  # --- tec events ---
  # add here when tec event scenarios/iterations get added

  # get all combinations - expand grid but with dfs, take from reshape::expand.grid.df
  indexes <- lapply(dfs, function(x) 1:nrow(x))
  grid <- do.call(expand.grid, indexes)
  df <- do.call(data.frame, mapply(function(df, index) df[index, , drop = FALSE], dfs, grid))
  colnames(df) <- unlist(lapply(dfs, colnames))
  rownames(df) <- 1:nrow(df)

  # as a for loop
  for (i in nrow(df)) {

    # if there's variation as far as what std pars can be included vs have to be, do a %in% w names()
    # turned back into into a list to be safe
    std_pars_i = as.list(df[i,c("m", "k", "m_v", "k_v", "pa", "po", "gw1", "gw2")])

    if (!is.null(def_pars)) {
      def_pars_values = as.list(df[i, names(df) %in% def_names])
      def_pars_i = mapply(function(x, y) {x$Value = y; return(x)}, def_pars, def_pars_values, SIMPLIFY = F)
    } else {
      def_pars_i = NULL
    }

    run_rhessys_core(
      input_rhessys = input_rhessys,
      hdr_files = hdr_files,
      std_pars = std_pars_i,
      tec_data = tec_data,
      def_pars =def_pars_i,
      runID = i
    )
  }

  # as a lapply


}
