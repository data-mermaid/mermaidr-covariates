start_end_to_interval <- function(start_date, end_date) {
  glue::glue("{start_date}T00:00:00Z/{end_date}T00:00:00Z")
}

summary_zonal_stats_single <- function(df, collection_id, n_days = 365, buffer = 1000, stats = c("min", "max", "mean")) {
  # Get zonal stats for X days before

  # Get the specific stat, AND use that summary of it
  # e.g. if min, get mins, then summarise using min
  # if mean, get means, then summarise using mean
  # this does not work the same way for mediacn....
  # what if e.g. they want the "average lowest value"?
  # they might be different

  # but for now, just do same stat + summary stat

  # Get sample_date
  sample_date <- df %>%
    pull(sample_date)

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
  #     collections = collection_id,
  #     datetime = input_interval,
  #     limit = 999999
  #   ) |>
  #   rstac::get_request()

  # There is currently a bug in the API that does not allow intervals from {start}/{end}

  # So have to get all items after start, then filter items that are before end
  after_start_input_interval <- after_date_to_datetime(input_sample_date_start)
  after_start_items <- rstac::stac(stac_url) |>
    rstac::stac_search(
      collections = collection_id,
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
    select(-all_of(stats)) %>%
    distinct()

  # Apply summary function to each of the cols
  zonal_stats_summary <- stats %>%
    purrr::map(\(x) {
      zonal_stats %>%
        dplyr::summarise(dplyr::across(all_of(x), ~ do.call(x, as.list(.x))))
    }) %>%
    bind_cols()

  zonal_stats_summary <- bind_cols(id_cols, zonal_stats_summary)

  # Reshape zonal stats into the following format:
  # covariate, start_date, end_date, band, statistic, value
  # covariate will just be collection_id
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
      covariate = collection_id,
      start_date = start_date,
      end_date = end_date
    ) %>%
    dplyr::select(covariate, start_date, end_date, band, statistic, value) %>%
    tidyr::nest(covariates = dplyr::everything())

  dplyr::bind_cols(df, zonal_stats_df)
}

#' @export
summary_zonal_stats <- function(df, collection, n_days = 365, buffer = 1000, stats = c("min", "max", "mean")) {
  # Allow for the possibility that they have more than one record at each site at each date -> make distinct for them
  df <- df %>%
    dplyr::mutate(...id = glue::glue("{site}_{sample_date}"))

  df_distinct <- df %>%
    dplyr::distinct(site, latitude, longitude, sample_date, ...id)

  zonal_stats <- df_distinct %>%
    split(.$...id) %>%
    purrr::map_dfr(summary_zonal_stats_single, collection, n_days = n_days, buffer = buffer, stats = stats, .progress = TRUE)

  # Re-attach to existing df, even if it was not distinct
  df %>%
    dplyr::left_join(zonal_stats %>%
      dplyr::select(...id, covariates), by = "...id") %>%
    dplyr::select(-...id)
}
