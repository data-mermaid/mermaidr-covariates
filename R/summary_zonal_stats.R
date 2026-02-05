#' Get summary zonal statistics
#'
#' Get zonal statistics. Unlike \code{get_zonal_statistics}, this summarises the
#' data for *all `n_days`* prior to the sample event date, using `spatial_stats`
#' to determine how to spatially summarise the data, and `temporal_stats` to
#' summarise the resulting data over time.
#'
#' @param se Sample events from \code{mermaidr}
#' @param covariate Covariate to get statistics for. Both covariate title or ID are permitted.
#' @param n_days Number of days to get statistics for. Includes the sample date
#'  itself, and days prior to it -- e.g., 365 days would include the sample date
#'  and the 364 days prior. Defaults to 365.
#' @param radius Radius around site location, in metres. Defaults to 1000.
#' @param spatial_stats Spatial statistics -- used to summarise all data around the site location, according to the \code{radius} set.
#' @param temporal_stats Temporal statistics -- used to summarise the data over time.
#'
#' @export
get_summary_zonal_statistics <- function(se, covariate, n_days = 365,
                                         radius = 1000,
                                         spatial_stats = c("min", "max", "mean"),
                                         temporal_stats = c("min", "max", "mean")) {
  covariate_id <- get_covariate_id(covariate)

  if (covariate_id == covariate) {
    covariate_name <- get_covariate_name_from_id(covariate_id)
  } else {
    covariate_name <- covariate
  }

  # Add an ID for iterating over (with site/date/lat/long distinct)
  se <- se %>%
    add_id_for_iteration()

  # Get (non-summary) zonal statistics
  zonal_stats <- get_zonal_statistics(se, covariate, n_days, radius, spatial_stats)

  zonal_stats <- zonal_stats %>%
    add_id_for_iteration() %>% # Add id back on
    left_join(zonal_stats, by = c("site", "sample_date", "latitude", "longitude")) %>%
    # just keep ID and covariates -> do not need lat/long/date, join back on later
    dplyr::select(...id, covariates) %>%
    # Unnest covariates, remove date
    tidyr::unnest(covariates) %>%
    dplyr::select(-date)

  # Set up to group by non-stat columns
  id_cols <- zonal_stats %>%
    dplyr::select(-dplyr::any_of("value")) %>%
    names()

  # Apply summary function to each of the cols
  zonal_stats_summary <- temporal_stats %>%
    purrr::map(\(x) {
      zonal_stats %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) %>%
        dplyr::summarise(
          dplyr::across(
            value,
            ~ {
              if (x == "mean") {
                if (all(is.na(.x))) {
                  NA
                } else {
                  mean(.x, trim = 0, na.rm = TRUE)
                }
              } else {
                do.call(x, as.list(.x, na.rm = TRUE))
              }
            }
          ),
          .groups = "drop"
        ) %>%
        dplyr::mutate(temporal_stat = x)
    }) %>%
    dplyr::bind_rows()

  # Reshape zonal stats into the following format:
  # covariate, start_date, n_days, end_date, band, temporal_stat, spatial_stat, value
  # covariate will just be covariate name
  # band is as is, with "_band" removed
  # Put into a df-column called covariates

  zonal_stats_df <- zonal_stats_summary %>%
    dplyr::right_join(se, by = "...id") %>%
    dplyr::mutate(
      covariate = covariate_name,
      start_date = start_date,
      end_date = end_date
    ) %>%
    dplyr::select(...id, covariate, start_date, end_date, n_dates, band, temporal_stat, spatial_stat, value) %>%
    tidyr::nest(covariates = -...id)

  # Re-attach to existing df, even if it was not distinct
  se %>%
    dplyr::left_join(zonal_stats_df, by = "...id") %>%
    dplyr::select(-...id)
}
