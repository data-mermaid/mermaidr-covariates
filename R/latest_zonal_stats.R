before_date_to_datetime <- function(date) {
  glue::glue("../{date}T00:00:00Z")
}

after_date_to_datetime <- function(date) {
  glue::glue("{date}T00:00:00Z/..")
}


latest_zonal_stats_single <- function(df, collection_id, buffer = 1000, stats = c("min", "max", "mean", "median")) {
  # Get sample_date
  sample_date <- df %>%
    pull(sample_date)

  # Add 1 day
  input_sample_date <- sample_date + lubridate::days(1)

  # Construct interval
  # TODO -> is there a reasonable lower band for this, e.g. if there haven't been any in a year?
  # Kind of annoying to filter for all time
  input_interval <- before_date_to_datetime(input_sample_date)

  # Search for items before that date
  relevant_items <- rstac::stac(stac_url) |>
    rstac::stac_search(
      collections = collection_id,
      datetime = input_interval,
      limit = 999999
    ) |>
    rstac::get_request()

  # Only include items that have the assets "data"
  relevant_items <- relevant_items %>%
    rstac::items_filter(filter_fn = function(x) {
      # No assets
      if (!"assets" %in% names(x)) {
        return(FALSE)
      }

      # Check for "data" asset
      "data" %in% names(x[["assets"]])
    })

  # Get the latest item WITH ASSETS -> get datetimes of each
  item_datetimes <- relevant_items[["features"]] %>%
    purrr::map_dfr(\(x) {
      dplyr::tibble(
        id = x[["id"]],
        datetime = x[["properties"]][["datetime"]]
      )
    }) %>%
    dplyr::mutate(datetime = lubridate::ymd_hms(datetime))

  # Latest item
  latest_item <- item_datetimes %>%
    dplyr::filter(datetime == max(datetime))

  latest_item_id <- latest_item[["id"]]

  # Get URL of latest item
  # TODO -> we already have the asset here, no need to hit the API again
  zonal_stats_input_url <- rstac::stac(stac_url) %>%
    rstac::stac_search(
      collections = collection_id,
      ids = latest_item_id
    ) %>%
    rstac::get_request() %>%
    rstac::assets_select(asset_names = "data") %>%
    rstac::assets_url()

  zonal_stats <- get_zonal_stats(df[["longitude"]], df[["latitude"]], zonal_stats_input_url, buffer = buffer, stats = stats)

  # Return df along with zonal stats, in df-column called: covariates
  zonal_stats_df <- zonal_stats %>%
    dplyr::select(-longitude, -latitude) %>%
    # Add date
    dplyr::mutate(covariates_date = as.Date(latest_item[["datetime"]])) %>%
    dplyr::relocate(covariates_date, .before = dplyr::everything()) %>%
    tidyr::nest(covariates = dplyr::everything())

  dplyr::bind_cols(df, zonal_stats_df)
}

#' @export
latest_zonal_stats <- function(df, collection, buffer = 1000, stats = c("min", "max", "mean", "median")) {
  # Allow for the possibility that they have more than one record at each site at each date -> make distinct for them
  df <- df %>%
    dplyr::mutate(...id = glue::glue("{site}_{sample_date}"))

  df_distinct <- df %>%
    dplyr::distinct(site, latitude, longitude, sample_date, ...id)

  zonal_stats <- df_distinct %>%
    split(.$...id) %>%
    purrr::map_dfr(latest_zonal_stats_single, collection, buffer = buffer, stats = stats, .progress = TRUE)

  # Re-attach to existing df, even if it was not distinct
  df %>%
    dplyr::left_join(zonal_stats %>%
      dplyr::select(...id, covariates), by = "...id") %>%
    dplyr::select(-...id)
}
