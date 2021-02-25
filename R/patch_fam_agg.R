#' patch_fam_agg
#'
#' Aggregate (aspatial) patches back to spatial patches via weighted mean.
#' Add in exceptions if you want to aggregate via different methods.
#' @param X Data Frame or Data Table to aggregate
#' @param areas Data frame (or table) of patch IDs and associated areas.
#' Needed for stratum outputs which don't include area.
#' @param na.rm Should NAs ne removed when calculating weighed average (passed through to weighted.mean)
#' @export

patch_fam_agg = function(X, areas = NULL, na.rm = FALSE) {

  if (!"familyID" %in% colnames(X)) {
    if (all(nchar(X$patchID) >= 3)) {
      X$familyID = floor(X$patchID/100)
      print(noquote("'familyID' was missing, generated based on 'patchID's."))
    } else {
      stop(paste0("Couldn't find 'familyID' column and patchID length is too short to contain familyIDs"))
    }
  }

  keep_cols = c("day", "month", "year", "basinID", "hillID", "zoneID", "patchID", "stratumID",
                "familyID", "date","wy","yd","wyd", "area")
  keep_cols = keep_cols[keep_cols %in% colnames(X)]
  group_cols = c("date", "familyID")
  if ("stratumID" %in% colnames(X)) {group_cols = c(group_cols, "stratumID")}
  agg_cols = colnames(X)[!colnames(X) %in% keep_cols]

  if (!"area" %in% colnames(X)) {
    if (!is.null(areas)) {
      X = data.table::merge.data.table(x = X, y = areas, by = "patchID")
    } else {
      stop("No 'area' column found, and no dataframe of patch areas provided")
    }
  }

  patch_area = X[, sum(area), by = group_cols]
  X = data.table::merge.data.table(x = X, y = patch_area, by = group_cols)
  pfam = X[, lapply(.SD, stats::weighted.mean, area/V1, na.rm = na.rm), by = group_cols, .SDcols = agg_cols]

  return(pfam)
}
