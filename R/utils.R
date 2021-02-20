# file to contain small utility functions that don't need to be exposed to users (not exported)
#
# ---------- Function to test if filter is valid ----------
valid_filter = function(f) {
  # assumes f is the already indexed into object, so names(f) is 'timestamp', 'output', etc., not 'filter'
  if (any(!c('timestep', 'output') %in% names(f))) {
    stop("Filter object is missing: ", c('timestep', 'output')[!c('timestep', 'output') %in% names(f)])
  }
  if (any(!c('format', 'path', 'filename') %in% names(f$output))) {
    stop("Filter object is missing output sub item: ", c('format', 'path', 'filename')[!c('format', 'path', 'filename') %in% names(f$output)])
  }

  if (all(!c('basin', 'hillslope', 'zone', 'patch',  'stratum') %in% names(f) )) {
    stop("Filter object is missing the spatial level. Can be: 'basin', 'hillslope', 'zone', 'patch', 'stratum'.")
  }
  spatial = which(names(f) %in% c('basin', 'hillslope', 'zone', 'patch', 'stratum'))
  if (any(!c('ids', 'variables') %in% names(f[[spatial]]))) {
    stop("Filter object is missing spatial level sub item: ", c('ids', 'variables')[!c('ids', 'variables') %in% names(f[[spatial]])])
  }
  return(TRUE)

}
