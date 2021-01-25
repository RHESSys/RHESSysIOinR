#' IOin_clim
#'
#' Generate input for run_rhessys climate basestation input
#' @param base_station_id Base station ID.
#' @param x_coordinate X coordinate.
#' @param y_coordinate Y coordinate.
#' @param z_coordinate Z coordinate.
#' @param effective_lai Effective LAI.
#' @param screen_height Screen height.
#' @param annual_prefix Prefix for annual climate inputs.
#' @param num_non_critical_annual_sequences Number of non critical annual climate inputs. Defaults to 0.
#' @param monthly_prefix Prefix for monthly climate inputs.
#' @param num_non_critical_monthly_sequences Number of non critical annual climate inputs. Defaults to 0.
#' @param daily_prefix Prefix for daily climate inputs.
#' @param num_non_critical_daily_sequences Number of non critical annual climate inputs. Defaults to 0.
#' @param hourly_prefix Prefix for hourly climate inputs.
#' @param num_non_critical_hourly_sequences Number of non critical annual climate inputs. Defaults to 0.
#'
#' @author Will Burke
#'
#' @export

IOin_clim = function(base_station_id,
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



  output_clim_base = data.frame(
    "values" = c(
      base_station_id,
      x_coordinate,
      y_coordinate,
      z_coordinate,
      effective_lai,
      screen_height,
      annual_prefix,
      num_non_critical_annual_sequences,
      monthly_prefix,
      num_non_critical_monthly_sequences,
      daily_prefix,
      num_non_critical_daily_sequences,
      hourly_prefix,
      num_non_critical_hourly_sequences
    ),
    "vars" = c(
      "base_station_id",
      "x_coordinate",
      "y_coordinate",
      "z_coordinate",
      "effective_lai",
      "screen_height",
      "annual_climate_prefix",
      "number_non_critical_annual_sequences",
      "monthly_climate_prefix",
      "number_non_critical_monthly_sequences",
      "daily_climate_prefix",
      "number_non_critical_daily_sequences",
      "hourly_climate_prefix",
      "number_non_critical_hourly_sequences"
    )
  )

  return(output_clim_base)
}
