#' Get zonal statistics
#'
#' Get zonal statistics. For *each* of the `n_days` prior to the sample event date,
#' returns summarised values of covariate data for `radius` metres around each site
#' location, using `spatial_stats` to determine how to spatially summarise the data.
#'
#' @param se Sample events from \code{mermaidr}
#' @param covariate Covariate to get statistics for. Both covariate title or ID are permitted.
#' @param n_days Number of days to get statistics for. Includes the sample date
#'  itself, and days prior to it -- e.g., 365 days would include the sample date
#'  and the 364 days prior. Defaults to 365.
#' @param radius Radius around site location, in metres. Defaults to 1000.
#' @param spatial_stats Spatial statistics -- used to summarise all data around
#' the site location, according to the \code{radius} set.
#' @param date_col Date back from (using \code{n_days}). Defaults to "sample_date".
#' @param .progress Whether to show progress bar and time remaining. Defaults to TRUE.
#'
#' @export
get_zonal_statistics <- function(se, covariate, n_days = 365,
                                 radius = 1000,
                                 spatial_stats = c("min", "max", "mean"),
                                 date_col = "sample_date",
                                 .progress = TRUE) {
  chunk_size <- 100

  if (nrow(se) == 0) {
    stop("No sample events to get zonal statistics for.", .call = FALSE)
  }

  covariate_id <- get_covariate_id(covariate)

  if (covariate_id == covariate) {
    covariate_name <- get_covariate_name_from_id(covariate_id)
  } else {
    covariate_name <- covariate
  }

  if (!"...id" %in% names(se)) {
    # Don't need to do this if get_summary_zonal_statistics() called get_zonal_
    # Already done there

    # Add an ID for iterating over (splitting by lat/long/sample date,
    # accounting for overlapping sample dates to reduce duplicating API calls)
    se <- se %>%
      add_id_for_iteration(date_col, n_days)
  }

  # Get zonal stats for all SEs
  zonal_stats <- get_zonal_stats(se, covariate_id, covariate_name,
    n_days = n_days, radius = radius, date_col = date_col,
    spatial_stats = spatial_stats, .progress = .progress
  )

  # Attach to sample events and remove ID
  se <- se %>%
    dplyr::left_join(zonal_stats,
      by = "...id",
      relationship = "many-to-many"
    ) %>%
    dplyr::select(-...id)

  # Only keep zonal stats that are actually relevant for SE, not all combined intervals
  # Also updating start_date and end_date
  covariates_cols <- se %>%
    dplyr::pull(covariates) %>%
    purrr::pluck(1) %>%
    names()


  se_flag_relevant <- se %>%
    dplyr::rename_with(\(x) "...date_temp", dplyr::all_of(date_col)) %>%
    dplyr::mutate(...date_temp = as.Date(...date_temp)) %>%
    tidyr::unnest(covariates) %>%
    dplyr::mutate(
      ...start_date = ...date_temp - (n_days - 1),
      ...end_date = ...date_temp,
      ...date_relevant = (date >= ...start_date & date <= ...end_date) | (is.na(date))
    )

  se_relevant <- se_flag_relevant %>%
    dplyr::filter(...date_relevant) %>%
    dplyr::select(-...date_relevant, -...start_date, -...end_date) %>%
    dplyr::group_by(project, site, latitude, longitude, ...date_temp) %>%
    # Recalculate based on relevant dates
    dplyr::mutate(
      n_dates = dplyr::n_distinct(date, na.rm = TRUE),
      start_date = min(date),
      end_date = max(date)
    ) %>%
    dplyr::ungroup()

  se_relevant %>%
    tidyr::nest(covariates = dplyr::all_of(covariates_cols)) %>%
    dplyr::rename_with(\(x) date_col, ...date_temp)
}

get_items_for_zonal_stats <- function(df, covariate_id, n_days = 365) {
  # Since intervals are combined, we can no longer just go back n_days from sample_date
  df <- df %>%
    dplyr::distinct(...start_date, ...end_date)
  # Subtract `n_days - 1` days - so it will be `ndays - 1` days before, and the sample date
  input_sample_date_start <- df[["...start_date"]] - lubridate::days(n_days - 1)
  input_sample_date_end <- df[["...end_date"]]

  # Construct interval
  input_interval <- start_end_to_interval(input_sample_date_start, input_sample_date_end)

  # Search for items between those dates
  relevant_items <- rstac::stac(stac_url) |>
    rstac::stac_search(
      collections = covariate_id,
      datetime = input_interval,
      limit = 999999
    ) |>
    rstac::get_request()

  if (relevant_items[["numberReturned"]] == 0) {
    return(
      dplyr::tibble(
        start_date = NA,
        end_date = NA,
        date = NA,
        url = NA
      )
    )
  }

  relevant_items <- relevant_items[["features"]]

  if (length(relevant_items) == 0) {
    return(NULL)
  }

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
    purrr::map_chr(\(x) {
      x[["assets"]][["data"]][["href"]]
    })

  return(
    dplyr::tibble(
      start_date = start_date,
      end_date = end_date,
      date = item_dates,
      url = zonal_stats_urls
    )
  )
}

get_zonal_stats <- function(ses, covariate_id, covariate_name, n_days, radius,
                            date_col, spatial_stats, .progress = TRUE) {
  # Potentially split not by ...id, if n_days is small

  se_list <- ses %>%
    split_for_chunking(covariate_id, n_days)

  zonal_stats <- se_list %>%
    purrr::map(
      \(se)
      get_zonal_stats_chunked(
        se,
        covariate_id,
        n_days,
        radius = radius,
        spatial_stats = spatial_stats
      ),
      .progress = .progress
    )

  zonal_stats <- zonal_stats %>%
    combine_from_chunking()

  zonal_stats %>%
    dplyr::group_by(...id) %>%
    dplyr::mutate(n_dates = dplyr::n_distinct(date, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    # TODO -> handle no data returned, e.g. with covariate "Daily Sea Surface Temperature (SST)"
    # Add n_dates, add covariate, remove "band_" character
    dplyr::mutate(
      covariate = covariate_name,
      band = stringr::str_remove(band, "band_"),
      band = as.numeric(band)
    ) %>%
    # reorganize to have a column `spatial_stat` and `value`, instead of a column for each stat
    tidyr::pivot_longer(
      cols = dplyr::all_of(spatial_stats),
      names_to = "spatial_stat",
      values_to = "value"
    ) %>%
    # reorganize covariates columns
    dplyr::select(...id, covariate, start_date, end_date, n_dates, date, band, spatial_stat, value) %>%
    tidyr::nest(covariates = -...id, .by = "...id") # Nest covariates
}

get_zonal_stats_chunked <- function(se, covariate_id, n_days = 30, radius = 1000,
                                    bands = list(1), approx_stats = FALSE,
                                    spatial_stats = c(
                                      "min", "max", "mean", "count", "sum", "std",
                                      "median", "majority", "minority", "unique",
                                      "range", "nodata", "area", "freq_hist"
                                    )) {
  # Multiple SEs here, so get items for each
  # Set up zonal_stats requests by getting relevant STAC items for each sample event

  stac_items <- se %>%
    split(.$...id) %>%
    purrr::map_dfr(\(x) get_items_for_zonal_stats(x, covariate_id, n_days = n_days),
      .id = "...id"
    ) %>%
    dplyr::mutate(...secondary_id = glue::glue("{...id}__{date}")) %>%
    dplyr::left_join(
      se %>%
        dplyr::select(...id, latitude, longitude),
      by = "...id",
      relationship = "many-to-many"
    ) %>%
    dplyr::distinct()

  # Returns a list with elements start_date, end_date, urls
  # and NULL if there are no items

  # Handle case where SEs do not have any items

  if (all(is.na(stac_items[["url"]]))) {
    res <- se %>%
      dplyr::select(...id) %>%
      dplyr::bind_cols(
        dplyr::tibble(
          start_date = NA,
          end_date = NA,
          date = NA,
          band = NA,
          spatial_stat = spatial_stats,
          value = NA
        )
      ) %>%
      tidyr::pivot_wider(names_from = spatial_stat, values_from = value)

    return(res)
  }

  # Get zonal stats for each URL

  # TODO -> handle no stats returned

  # Set up requests to parallelize
  request_base <- httr2::request(zonal_stats_raster_url) %>%
    httr2::req_throttle(capacity = chunk_size, fill_time_s = 3) %>%
    httr2::req_user_agent("mermaidr-covariates") %>%
    httr2::req_body_json(list(
      aoi = NULL,
      url = NULL,
      stats = as.list(spatial_stats),
      bands = bands,
      approx_stats = approx_stats
    )) %>%
    httr2::req_error(is_error = \(res) FALSE)

  stac_items <- stac_items %>%
    split(.$...secondary_id)

  requests <- purrr::map(
    stac_items,
    \(x) {
      request_base %>%
        httr2::req_body_json_modify(
          url = x[["url"]],
          aoi = list(
            type = "Point",
            coordinates = c(x[["longitude"]], x[["latitude"]]),
            radius = radius
          ),
        )
    }
  )

  res <- httr2::req_perform_parallel(requests, progress = FALSE)

  names(res) <- names(stac_items)

  # Format the results of each call
  res %>%
    purrr::keep(\(x) x$status_code == 200) %>%
    purrr::imap(
      \(res, date) {
        res %>%
          httr2::resp_body_json() %>%
          purrr::map_dfr(\(x) {
            x <- purrr::map(x, \(x) if (is.null(x)) NA else x)
            dplyr::as_tibble(x)
          }, .id = "band")
      }
    ) %>%
    purrr::list_rbind(names_to = "...secondary_id") %>%
    dplyr::left_join(stac_items %>% dplyr::bind_rows(),
      by = "...secondary_id",
      relationship = "one-to-one"
    ) %>%
    dplyr::select(-...secondary_id)
}
