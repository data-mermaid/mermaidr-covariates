#' Get summary of zonal statistics
#'
#' @param df Sample events from \code{mermaidr}
#' @param covariates Covariate IDs to get statistics for
#' @param n_days Number of days prior to sample date to get statistics for. Defaults to 365.
#' @param buffer Buffer around site location, in metres. Defaults to 1000.
#' @param stats Summary statistics. One of: min, max, or mean.
#'
#' @export
get_zonal_statistics <- function(df, covariates, n_days = 365, buffer = 1000, stats = c("min", "max", "mean")) {
  # Allow for the possibility that they have more than one record at each site at each date -> make distinct for them
  df <- df %>%
    dplyr::mutate(...id = glue::glue("{site}_{sample_date}"))

  df_distinct <- df %>%
    dplyr::distinct(site, latitude, longitude, sample_date, ...id)

  zonal_stats <- df_distinct %>%
    split(.$...id) %>%
    purrr::map_dfr(summary_zonal_stats_single, covariates, n_days = n_days, buffer = buffer, stats = stats, .progress = TRUE)

  # Re-attach to existing df, even if it was not distinct
  df %>%
    dplyr::left_join(zonal_stats %>%
      dplyr::select(...id, covariates), by = "...id") %>%
    dplyr::select(-...id)
}

summary_zonal_stats_single <- function(df, covariates, n_days = 365, buffer = 1000, stats = c("min", "max", "mean")) {
  # Get zonal stats for X days before

  # Get the specific stat, AND use that summary of it
  # e.g. if min, get mins, then summarise using min
  # if mean, get means, then summarise using mean
  # this does not work the same way for median....
  # what if e.g. they want the "average lowest value"?
  # they might be different

  # but for now, just do same stat + summary stat

  # Get sample_date
  sample_date <- df %>%
    dplyr::pull(sample_date)

  # Add 1 day -> end will be midnight on this day
  input_sample_date_end <- sample_date + lubridate::days(1)

  # Subtract `n_days` days
  input_sample_date_start <- sample_date - lubridate::days(n_days)

  # # Construct interval
  # input_interval <- start_end_to_interval(input_sample_date_start, input_sample_date_end)
  #
  # # Search for items between those dates
  # relevant_items <- rstac::stac(stac_url) |>
  #   rstac::stac_search(
  #     collections = covariates,
  #     datetime = input_interval,
  #     limit = 999999
  #   ) |>
  #   rstac::get_request()

  # There is currently a bug in the API that does not allow intervals from {start}/{end}

  # So have to get all items after start, then filter items that are before end
  after_start_input_interval <- after_date_to_datetime(input_sample_date_start)
  after_start_items <- rstac::stac(stac_url) |>
    rstac::stac_search(
      collections = covariates,
      datetime = after_start_input_interval,
      limit = 999999
    ) |>
    rstac::get_request()

  # Keep only items on or before sample date

  relevant_items <- after_start_items[["features"]] %>%
    purrr::keep(\(x) {
      item_date <- lubridate::ymd_hms(x[["properties"]][["datetime"]])

      item_date <= sample_date
    })

  # Only include items that have the assets "data"
  relevant_items <- relevant_items %>%
    purrr::keep(\(x) {
      # No assets
      if (!"assets" %in% names(x)) {
        return(FALSE)
      }

      # Check for "data" asset
      "data" %in% names(x[["assets"]])
    })

  # Get the dates of items, to have start/end date
  item_dates <- relevant_items %>%
    purrr::map_chr(\(x) {
      x[["properties"]][["datetime"]]
    })

  item_dates <- as.Date(item_dates)
  start_date <- min(item_dates)
  end_date <- max(item_dates)

  # Get URL of each item's `data` asset
  zonal_stats_urls <- relevant_items %>%
    purrr::map(\(x) {
      x[["assets"]][["data"]][["href"]]
    })

  # Get zonal stats for each URL

  zonal_stats <- zonal_stats_urls %>%
    purrr::map_dfr(\(x) {
      get_zonal_stats(df[["longitude"]], df[["latitude"]], x, buffer = buffer, stats = stats)
    })

  # TODO -> handle no stats returned

  if (nrow(zonal_stats) == 0) {
    browser()
  }

  # Keep non-summary stat cols
  id_cols <- zonal_stats %>%
    dplyr::select(-dplyr::all_of(stats)) %>%
    dplyr::distinct()

  # Apply summary function to each of the cols
  zonal_stats_summary <- stats %>%
    purrr::map(\(x) {
      zonal_stats %>%
        dplyr::summarise(dplyr::across(dplyr::all_of(x), ~ do.call(x, as.list(.x))))
    }) %>%
    dplyr::bind_cols()

  zonal_stats_summary <- bind_cols(id_cols, zonal_stats_summary)

  # Reshape zonal stats into the following format:
  # covariate, start_date, end_date, band, statistic, value
  # covariate will just be covariates
  # band is as is, with "_band" removed
  # Put into a df-column called covariates
  zonal_stats_df <- zonal_stats_summary %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(stats),
      names_to = "statistic",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      band = stringr::str_remove(band, "band_"),
      band = as.numeric(band),
      covariate = covariates,
      start_date = start_date,
      end_date = end_date
    ) %>%
    dplyr::select(covariate, start_date, end_date, band, statistic, value) %>%
    tidyr::nest(covariates = dplyr::everything())

  dplyr::bind_cols(df, zonal_stats_df)
}


get_zonal_stats <- function(longitude, latitude, url, buffer = 1000, bands = list(1, 2, 3), approx_stats = FALSE,
                            stats = c(
                              "min", "max", "mean", "count", "sum", "std", "median", "majority", "minority", "unique", "range", "nodata", "area", "freq_hist"
                            )) {
  res <- httr2::request(zonal_stats_url) %>%
    httr2::req_body_json(list(
      aoi = list(
        type = "Point", coordinates = c(longitude, latitude),
        buffer_size = buffer
      ),
      image = list(
        url = url,
        # bands = list(1), # TODO, not working -> only works with band: 1
        approx_stats = approx_stats
      ),
      stats = as.list(stats)
    )) %>%
    httr2::req_perform()

  res_tbl <- res %>%
    httr2::resp_body_json() %>%
    purrr::map_dfr(\(x) {
      x <- purrr::map(x, \(x) if (is.null(x)) NA else x)
      dplyr::as_tibble(x)
    }, .id = "band")

  dplyr::bind_cols(
    dplyr::tibble(longitude = longitude, latitude = latitude),
    res_tbl
  )
}
