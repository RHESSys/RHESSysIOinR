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
                     monthly_prefix = "monthly",
                     daily_prefix = "daily",
                     hourly_prefix = "hourly") {

  input_clim_base_list <- list(
    list(
      core = data.frame(
        c1 = character(),
        c2 = character(),
        stringsAsFactors = FALSE
      ),
      annual = data.frame(
        c1 = character(),
        c2 = character(),
        stringsAsFactors = FALSE
      ),
      monthly = data.frame(
        c1 = character(),
        c2 = character(),
        stringsAsFactors = FALSE
      ),
      daily = data.frame(
        c1 = character(),
        c2 = character(),
        stringsAsFactors = FALSE
      ),
      hourly = data.frame(
        c1 = character(),
        c2 = character(),
        stringsAsFactors = FALSE
      )
    )
  )

  input_clim_base_list[[1]][[1]][1, ] <-
    data.frame(c1 = base_station_id,
               c2 = "base_station_id",
               stringsAsFactors = FALSE)
  input_clim_base_list[[1]][[1]][2, ] <-
    data.frame(c1 = x_coordinate,
               c2 = "x_coordinate",
               stringsAsFactors = FALSE)
  input_clim_base_list[[1]][[1]][3, ] <-
    data.frame(c1 = y_coordinate,
               c2 = "y_coordinate",
               stringsAsFactors = FALSE)
  input_clim_base_list[[1]][[1]][4, ] <-
    data.frame(c1 = z_coordinate,
               c2 = "z_coordinate",
               stringsAsFactors = FALSE)
  input_clim_base_list[[1]][[1]][5, ] <-
    data.frame(c1 = effective_lai,
               c2 = "effective_lai",
               stringsAsFactors = FALSE)
  input_clim_base_list[[1]][[1]][6, ] <-
    data.frame(c1 = screen_height,
               c2 = "screen_height",
               stringsAsFactors = FALSE)

  input_clim_base_list[[1]][[2]][1, ] <-
    data.frame(c1 = annual_prefix,
               c2 = "annual_climate_prefix",
               stringsAsFactors = FALSE)
  input_clim_base_list[[1]][[2]][2, ] <-
    data.frame(c1 = 0,
               c2 = "number_non_critical_annual_sequences",
               stringsAsFactors = FALSE)

  input_clim_base_list[[1]][[3]][1, ] <-
    data.frame(c1 = monthly_prefix,
               c2 = "monthly_climate_prefix",
               stringsAsFactors = FALSE)
  input_clim_base_list[[1]][[3]][2, ] <-
    data.frame(c1 = 0,
               c2 = "number_non_critical_monthly_sequences",
               stringsAsFactors = FALSE)

  input_clim_base_list[[1]][[4]][1, ] <-
    data.frame(c1 = daily_prefix,
               c2 = "daily_climate_prefix",
               stringsAsFactors = FALSE)
  input_clim_base_list[[1]][[4]][2, ] <-
    data.frame(c1 = 0,
               c2 = "number_non_critical_daily_sequences",
               stringsAsFactors = FALSE)

  input_clim_base_list[[1]][[5]][1, ] <-
    data.frame(c1 = hourly_prefix,
               c2 = "hourly_climate_prefix",
               stringsAsFactors = FALSE)
  input_clim_base_list[[1]][[5]][2, ] <-
    data.frame(c1 = 0,
               c2 = "number_non_critical_hourly_sequences",
               stringsAsFactors = FALSE)

  return(input_clim_base_list)

}
