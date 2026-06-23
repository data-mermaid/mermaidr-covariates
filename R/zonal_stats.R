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
                                 bands = list(1),
                                 spatial_stats = c("min", "max", "mean"),
                                 date_col = "sample_date",
                                 type = "raster",
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
    n_days = n_days, radius = radius, bands = bands, date_col = date_col,
    spatial_stats = spatial_stats, type = type, .progress = .progress
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

  # Only do this if daily, otherwise just return SE
  covariate_interval <- get_covariate_interval(covariate_id)

  if (covariate_interval != "daily") {
    return(se)
  }

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
  # Double check covariate interval -- if it is annual/once, then do not need to get items by date
  covariate_interval <- get_covariate_interval(covariate_id)

  if (covariate_interval == "once") {
    relevant_items <- rstac::stac(stac_url) |>
      rstac::stac_search(
        collections = covariate_id
      ) |>
      rstac::get_request()
  } else if (covariate_interval == "periodic") {
    # Look for items before the sample date
    search_interval <- before_date_to_datetime(unique(df[["...end_date"]]))
    potential_items <- rstac::stac(stac_url) |>
      rstac::stac_search(
        collections = covariate_id,
        datetime = search_interval,
        limit = 99999
      ) |>
      rstac::get_request()

    # Get the most recent item
    item_dates <- potential_items[["features"]] %>%
      purrr::imap_dfr(\(x, y) {
        dplyr::tibble(
          date = as.Date(x[["properties"]][["datetime"]]),
          id = x[["id"]]
        )
      })

    latest_item <- item_dates %>%
      dplyr::filter(date == max(date)) %>%
      dplyr::pull(id)

    relevant_items <- rstac::stac(stac_url) |>
      rstac::stac_search(
        collections = covariate_id, ids = latest_item,
        limit = 1
      ) |>
      rstac::get_request()
  } else if (covariate_interval == "daily") {
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
  }

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
      properties <- x[["properties"]]

      if ("datetime" %in% names(properties)) {
        properties[["datetime"]]
      } else {
        browser()
      }
    })

  item_dates <- as.Date(item_dates)
  start_date <- min(item_dates)
  end_date <- max(item_dates)

  # Get URL of each item's relevant asset (cog type)
  zonal_stats_urls <- relevant_items %>%
    purrr::map_chr(\(x) {
      assets <- x[["assets"]]
      cog_asset <- get_cog_assets(x)
      if (length(cog_asset) != 1) {
        browser()
      } else {
        assets[[cog_asset]][["href"]]
      }
    })

  return(
    dplyr::tibble(
      date = item_dates,
      url = zonal_stats_urls
    )
  )
}

get_zonal_stats <- function(ses, covariate_id, covariate_name, n_days, radius, bands,
                            date_col, spatial_stats, type, .progress = TRUE) {
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
        bands = bands,
        spatial_stats = spatial_stats,
        type = type
      ),
      .progress = .progress
    )

  zonal_stats <- zonal_stats %>%
    dplyr::bind_rows()

  zonal_stats %>%
    dplyr::group_by(...id) %>%
    dplyr::mutate(n_dates = dplyr::n_distinct(date, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    # Add n_dates, add covariate, remove "band_" character
    dplyr::mutate(
      covariate = covariate_name,
      # NEW, returning "column" instead of band, so it's consistent across vector and raster
      # and then keep "band_", so it's e.g. column = band_1, pretty clear
      # band = stringr::str_remove(band, "band_"),
      # Not necessarily numeric, in case it is column
      # If they have specified band by name, can we return that?
      # band = as.numeric(band)
    ) %>%
    # reorganize to have a column `spatial_stat` and `value`, instead of a column for each stat
    tidyr::pivot_longer(
      cols = dplyr::all_of(spatial_stats),
      names_to = "spatial_stat",
      values_to = "value"
    ) %>%
    # reorganize covariates columns
    dplyr::select(...id, covariate, dplyr::any_of(c("start_date", "end_date")), n_dates, date, column, spatial_stat, value) %>%
    tidyr::nest(covariates = -...id, .by = "...id") # Nest covariates
}

get_zonal_stats_chunked <- function(se, covariate_id, n_days = 30, radius = 1000,
                                    bands = list(1), approx_stats = FALSE,
                                    spatial_stats = c(
                                      "min", "max", "mean", "count", "sum", "std",
                                      "median", "majority", "minority", "unique",
                                      "range", "nodata", "area", "freq_hist"
                                    ),
                                    type) {
  covariate_interval <- get_covariate_interval(covariate_id)

  if (covariate_interval != "daily") {
    # Rather than getting items each time, just get all of the items, then attach the relevant one
    # TODO -- this doesn't even need to happen in chunks, could just happen at top level
    items_info <- rstac::stac(stac_url) |>
      rstac::stac_search(
        collections = covariate_id,
        limit = 1
      ) |>
      rstac::get_request()

    n_items <- items_info$numberMatched

    items <- rstac::stac(stac_url) |>
      rstac::stac_search(
        collections = covariate_id,
        limit = n_items
      ) |>
      rstac::get_request()

    # Get each item's date and COG asset url
    cog_assets <- items[["features"]] %>%
      purrr::map_dfr(\(x) {
        dplyr::tibble(
          date = as.Date(x[["properties"]][["datetime"]]),
          url = x[["assets"]][[get_cog_assets(x)]][["href"]]
        )
      })

    if (covariate_interval == "once") {
      stac_items <- se %>%
        dplyr::mutate(...join = TRUE) %>%
        dplyr::left_join(cog_assets %>%
          dplyr::mutate(...join = TRUE), by = "...join") %>%
        dplyr::mutate(
          ...secondary_id = glue::glue("{...id}__{date}")
        )
    } else {
      stac_items <- se %>%
        dplyr::mutate(...join = TRUE) %>%
        dplyr::left_join(cog_assets %>%
          dplyr::mutate(...join = TRUE), by = "...join") %>%
        dplyr::filter(...end_date >= date) %>%
        dplyr::group_by(...id) %>%
        dplyr::filter(date == max(date)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          ...secondary_id = glue::glue("{...id}__{date}")
        )
    }
  } else {
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
  }

  # Returns a list with elements start_date, end_date, urls
  # and NULL if there are no items

  # Handle case where SEs do not have any items

  if (all(is.na(stac_items[["url"]]))) {
    res <- create_empty_zonal_stats(se, spatial_stats)

    return(res)
  }

  # Get zonal stats for each URL
  GET_zonal_stats(stac_items, "...secondary_id", radius, bands, approx_stats, spatial_stats, type = type)
}

create_empty_zonal_stats <- function(se, spatial_stats) {
  se %>%
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
}
