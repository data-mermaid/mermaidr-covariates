#' Summarise zonal statistics
#'
#' Summarise zonal statistics. Using the results from  \code{get_zonal_statistics},
#' this summarises the data for *all `n_days`* prior to the sample event date, using
#' `temporal_stats` to summarise the resulting data over time.
#'
#' @param zonal_statistics Sample events with zonal statistics, from \code{get_zonal_statistics}.
#' @param temporal_stats Temporal statistics -- used to summarise the data over time.
#'
#' @export
summarise_zonal_statistics <- function(zonal_statistics,
                                       temporal_stats = c("min", "max", "mean")) {
  # Check that zonal_statistics contains the `zonal_statistics` column and it is in the correct format
  valid_format <- identical(zonal_statistics %>%
    dplyr::select(dplyr::any_of("zonal_statistics")) %>%
    dplyr::select(dplyr::where(is.list)) %>%
    names(), "zonal_statistics")

  if (!valid_format) {
    usethis::ui_stop("Input must be the result from `get_zonal_statistics()`, with column `zonal_statistics`.")
  }

  # Add row number to identify after unnesting
  zonal_statistics <- zonal_statistics %>%
    dplyr::mutate(...summary_id = dplyr::row_number())

  zonal_stats <- zonal_statistics %>%
    dplyr::select(...summary_id, zonal_statistics) %>%
    tidyr::unnest(zonal_statistics)

  # Set up to group by non-stat columns
  id_cols <- zonal_stats %>%
    dplyr::select(-value, -date) %>%
    names()

  # Check whether there is only 1 row per group -- then there is no summarising needed, return message along with summarised df
  one_per_group <- identical(zonal_stats %>%
    dplyr::add_count(dplyr::across(dplyr::all_of(id_cols)), name = "...group_n") %>% dplyr::pull(...group_n) %>% unique(), 1L)

  # Calculate min_date, max_date, n_dates, EXCLUDING any dates where value is NA
  zonal_stats_dates <- zonal_stats %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) %>%
    dplyr::summarise(
      start_date = min(date),
      end_date = max(date),
      n_dates = dplyr::n_distinct(date),
      .groups = "drop"
    )

  # Add back on to zonal_stats, making n_dates 0 if there are none
  # Remove "date"
  zonal_stats <- zonal_stats %>%
    dplyr::select(-date) %>%
    dplyr::left_join(zonal_stats_dates, by = dplyr::join_by(
      ...summary_id, covariate, band,
      spatial_stat
    )) %>%
    dplyr::mutate(n_dates = dplyr::coalesce(n_dates, 0))

  # Re-determine ID cols
  id_cols <- zonal_stats %>%
    dplyr::select(-value) %>%
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

  # Calculate

  # Reshape zonal stats into the following format:
  # covariate, start_date, n_days, end_date, band, band_name (if relevant), temporal_stat, spatial_stat, value
  # covariate will just be covariate name
  # Put into a df-column called summary_zonal_statistics

  zonal_stats_df <- zonal_stats_summary %>%
    dplyr::select(...summary_id, covariate, start_date, end_date, n_dates, band, dplyr::any_of("band_name"), spatial_stat, temporal_stat, value) %>%
    tidyr::nest(summary_zonal_statistics = -...summary_id)

  # Check that there is a row for every original row
  if (nrow(zonal_stats_df) != nrow(zonal_statistics)) {
      usethis::ui_stop("Unexpected error, please report: mismatch between summary zonal statistics and original data")
  }

  # Join back on to original zonal_statistics
  original_names <- zonal_statistics %>%
    dplyr::select(-...summary_id) %>%
    names()

  if (one_per_group) {
    usethis::ui_warn("Note: there is only one data point to summarise for each sample event, band, and temporal statistic -- the results before and after summarising are identical.")
  }

  # Re-attach to existing df
  zonal_statistics %>%
    dplyr::left_join(zonal_stats_df, by = "...summary_id") %>%
    dplyr::arrange(...summary_id) %>%
    dplyr::select(dplyr::all_of(original_names), summary_zonal_statistics)
}
