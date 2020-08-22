#' clim_auto
#'
#' Generate input for run_rhessys basestation input
#'
#' @author Will Burke
#'
#' @export

clim_auto = function(base_station_id,
                     x_coordinate,
                     y_coordinate,
                     z_coordinate,
                     effective_lai,
                     screen_height,
                     annual_prefix = "annual",
                     num_non_critical_annual_sequences = 0,
                     monthly_prefix = "monthly",
                     num_non_critical_monthly_sequences = 0,
                     daily_prefix = "daily",
                     num_non_critical_daily_sequences = 0,
                     hourly_prefix = "hourly",
                     num_non_critical_hourly_sequences = 0) {

  input_clim_base_list <- list(
    list(
      core = data.frame(
        values = c(base_station_id, x_coordinate, y_coordinate, effective_lai, screen_height),
        vars = c("base_station_id", "x_coordinate", "y_coordinate", "effective_lai", "screen_height")
      ),
      annual = data.frame(
        values = c(annual_prefix, num_non_critical_annual_sequences),
        vars = c("annual_climate_prefix","number_non_critical_annual_sequences")
      ),
      monthly = data.frame(
        values = c(monthly_prefix, num_non_critical_monthly_sequences),
        vars = c("monthly_climate_prefix","number_non_critical_monthly_sequences")
      ),
      daily = data.frame(
        values = c(daily_prefix, num_non_critical_daily_sequences),
        vars = c("daily_climate_prefix","number_non_critical_daily_sequences")
      ),
      hourly = data.frame(
        values = c(hourly_prefix, num_non_critical_hourly_sequences),
        vars = c("hourly_climate_prefix","number_non_critical_hourly_sequences")
      )
    )
  )

  return(input_clim_base_list)
}
