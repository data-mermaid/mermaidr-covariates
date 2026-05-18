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
#'
#' @export
get_zonal_statistics <- function(se, covariate, n_days = 365,
                                 radius = 1000,
                                 spatial_stats = c("min", "max", "mean"),
                                 date_col = "sample_date",
                                 chunk_threshold = 100,
                                 dedupe_items) {
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
        add_id_for_iteration(date_col, n_days, dedupe_items)
  }

  # Get zonal stats for all SEs
  zonal_stats <- get_zonal_stats(se, covariate_id, covariate_name, n_days,
    radius, spatial_stats,
    chunk_threshold = chunk_threshold,
    dedupe_items
  )

  # Attach to sample events and remove ID
  se %>%
    dplyr::left_join(zonal_stats, by = "...id") %>%
    dplyr::select(-...id)
}
prepare_rstac_request <- function(q, ...) {
  rstac:::check_query(q)
  q$verb <- "GET"
  q$encode <- NULL
  q$version <- rstac:::stac_version(q, ...)
  q <- rstac:::before_request(q)
  list(
    q = q,
    url = rstac:::resolve_url(q$base_url, q$endpoint),
    query = rstac:::query_encode(q$params)
  )
}

get_items_for_zonal_stats <- function(df, covariate_id, n_days = 365) {
  # If intervals are combined, then we can no longer just go back n_days from sample_date
  if ("...start_date" %in% names(df)) {
    df <- df %>%
      dplyr::distinct(...start_date, ...end_date)
    # Subtract `n_days - 1` days - so it will be `ndays - 1` days before, and the sample date
    input_sample_date_start <- df[["...start_date"]] - lubridate::days(n_days - 1)
    input_sample_date_end <- df[["...end_date"]]
  } else {
    input_sample_date_end <- df[["sample_date"]]
    input_sample_date_start <- input_sample_date_end - lubridate::days(n_days - 1)
  }

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

get_zonal_stats <- function(se_list, covariate_id, covariate_name, n_days, radius,
                            date_col, spatial_stats, chunk_threshold = 100, dedupe_items) {

  # If n_days >= chunk_threshold, just get by SE
  # Also checking that the interval is daily, in this case
  # Otherwise, if it is e.g. monthly, then we might want to increase the chunk size
  # Because then we would not actually be getting 50 items, but say 1-2 if it is monthly

  covariate_interval <- determine_covariate_interval(covariate_id)

  if (!covariate_interval %in% c("daily")) {
    browser()
  }

  if (do_by_se) {
    zonal_stats <- se_list %>%
      purrr::map(
        \(se)
        get_zonal_stats_single(se,
          covariate_id,
          n_days,
          radius = radius,
          spatial_stats = spatial_stats,
          date_col = date_col
        ),
        .progress = TRUE
      )
  } else {
    # Otherwise, chunk SEs and get that way
    ses <- dplyr::bind_rows(se_list)
    chunk_size <- floor(chunk_threshold / n_days)

  se_list <- ses %>%
    split(.$...id)

  zonal_stats <- se_list %>%
    purrr::map(
      \(se)
      get_zonal_stats_chunked(
        se,
        covariate_id,
        n_days,
        radius = radius,
        spatial_stats = spatial_stats,
        chunk_threshold = chunk_threshold,
        dedupe_items = dedupe_items
      ),
      .progress = TRUE
    )

  zonal_stats <- zonal_stats %>%
    purrr::map(\(x) {
      if (nrow(x) == 0) {
        dplyr::tibble(
          start_date = NA,
          end_date = NA,
          date = NA,
          band = NA,
          spatial_stat = spatial_stats,
          value = NA
        ) %>%
          tidyr::pivot_wider(names_from = spatial_stat, values_from = value)
      } else {
        x
      }
    })

  zonal_stats <- zonal_stats %>%
    purrr::list_rbind(names_to = "...id")

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
}

get_zonal_stats_single <- function(se, covariate_id, n_days = 30, radius = 1000,
                                   bands = list(1), approx_stats = FALSE,
                                   spatial_stats = c(
                                     "min", "max", "mean", "count", "sum", "std",
                                     "median", "majority", "minority", "unique",
                                     "range", "nodata", "area", "freq_hist"
                                   ),
                                   date_col = date_col,  chunk_threshold) {
  # Set up zonal_stats requests by getting relevant STAC items for each sample event
  stac_items <- get_items_for_zonal_stats(
    se,
    covariate_id,
    n_days = n_days,
    date_col = date_col
  )

  # Returns a list with elements start_date, end_date, urls
  # and NULL if there are no items

  # Handle case where SE does not have any items

  if (all(is.na(stac_items[["url"]]))) {
    return(dplyr::tibble())
  }

  # Get zonal stats for each URL

  # TODO -> handle no stats returned

  # Set up requests to parallelize

  request_base <- httr2::request(zonal_stats_raster_url) %>%
    httr2::req_throttle(capacity = nrow(stac_items), fill_time_s = 30) %>%
    httr2::req_user_agent("mermaidr-covariates") %>%
    httr2::req_body_json(list(
      aoi = list(
        type = "Point",
        coordinates = c(se[["longitude"]], se[["latitude"]]),
        radius = radius
      ),
      url = NULL,
      stats = as.list(spatial_stats),
      bands = bands,
      approx_stats = approx_stats
    )) %>%
    httr2::req_error(is_error = \(res) FALSE)

  requests <- purrr::map(
    stac_items[["url"]],
    \(x) {
      request_base %>%
        httr2::req_body_json_modify(
          url = x
        )
    }
  )

  res <- httr2::req_perform_parallel(requests, progress = FALSE)

  names(res) <- stac_items[["date"]]

  # TODO -> handle this later on
  # if (httr2::resp_status(res) != 200) {
  #   stop(call. = FALSE, paste0(
  #     "Error getting zonal statistics: ",
  #     httr2::resp_status(res), " ",
  #     httr2::resp_status_desc(res)
  #   ))
  # }

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
    purrr::list_rbind(names_to = "date") %>%
    dplyr::bind_cols(stac_items %>% dplyr::distinct(start_date, end_date))
}

get_zonal_stats_chunked <- function(se, covariate_id, n_days = 30, radius = 1000,
                                    bands = list(1), approx_stats = FALSE,
                                    spatial_stats = c(
                                      "min", "max", "mean", "count", "sum", "std",
                                      "median", "majority", "minority", "unique",
                                      "range", "nodata", "area", "freq_hist"
                                    ), chunk_threshold, dedupe_items) {
  # TODO -> reduce ALL duplication from get_zonal_stats_single

  # Multiple SEs here, so get items for each
  # Set up zonal_stats requests by getting relevant STAC items for each sample event

  stac_items <- se %>%
    split(.$...id) %>%
    purrr::map_dfr(\(x) get_items_for_zonal_stats(se, covariate_id, n_days = n_days),
      .id = "...id"
    ) %>%
    dplyr::mutate(...secondary_id = glue::glue("{...id}__{date}")) %>%
    dplyr::left_join(
      se %>%
        dplyr::select(...id, latitude, longitude),
      by = "...id"
    ) %>%
      dplyr::distinct()

  # Returns a list with elements start_date, end_date, urls
  # and NULL if there are no items

  # Handle case where SEs do not have any items

  if (all(is.na(stac_items[["url"]]))) {
    return(dplyr::tibble())
  }

  # Get zonal stats for each URL

  # TODO -> handle no stats returned

  # Set up requests to parallelize
  request_base <- httr2::request(zonal_stats_raster_url) %>%
    httr2::req_throttle(capacity = chunk_threshold, fill_time_s = 3) %>%
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
    dplyr::left_join(stac_items %>% dplyr::bind_rows(), by = "...secondary_id") %>%
    dplyr::select(-...secondary_id)
}

hit_zonal_stats_api <- function(x, capacity, radius = 1000, spatial_stats = "mean",
                                bands = list(1), approx_stats = FALSE, dedupe_items = FALSE) {
  if (dedupe_items) {
    x <- x %>%
      mutate(...secondary_id = glue::glue("{latitude}_{longitude}_{url}"))
  } else {
    x <- x %>%
      mutate(...secondary_id = glue::glue("{latitude}_{longitude}_{sample_date}_{url}"))
  }

  x_list <- x %>%
    distinct(latitude, longitude, url, ...secondary_id) %>%
    split(.$...secondary_id)

  request_base <- httr2::request(zonal_stats_raster_url) %>%
    httr2::req_throttle(capacity = capacity, fill_time_s = 5) %>%
    httr2::req_user_agent("mermaidr-covariates") %>%
    httr2::req_body_json(list(
      aoi = NULL,
      url = NULL,
      stats = as.list(spatial_stats),
      bands = bands,
      approx_stats = approx_stats
    )) %>%
    httr2::req_error(is_error = \(res) FALSE)

  requests <- purrr::map(
    x_list,
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

  names(res) <- purrr::map_chr(x_list, "...secondary_id")

  # TODO -> handle this later on
  # if (httr2::resp_status(res) != 200) {
  #   stop(call. = FALSE, paste0(
  #     "Error getting zonal statistics: ",
  #     httr2::resp_status(res), " ",
  #     httr2::resp_status_desc(res)
  #   ))
  # }

  # Format the results of each call
  res %>%
    purrr::keep(\(x) x$status_code == 200) %>%
    purrr::map(
      \(res, ...secondary_id) {
        res %>%
          httr2::resp_body_json() %>%
          purrr::map_dfr(\(x) {
            x <- purrr::map(x, \(x) if (is.null(x)) NA else x)
            dplyr::as_tibble(x)
          }, .id = "band")
      }
    ) %>%
    purrr::list_rbind(names_to = "...secondary_id") %>%
    dplyr::left_join(x %>% dplyr::distinct(start_date, end_date, ...secondary_id), by = "...secondary_id") %>%
    dplyr::select(-...secondary_id) %>%
    dplyr::arrange(start_date, end_date, mean) %>%
    nrow()
}
