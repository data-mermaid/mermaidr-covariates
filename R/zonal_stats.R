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
#'
#' @export
get_zonal_statistics <- function(se, covariate, n_days = 365,
                                 radius = 1000,
                                 spatial_stats = c("min", "max", "mean")) {
  covariate_id <- get_covariate_id(covariate)

  if (covariate_id == covariate) {
    covariate_name <- get_covariate_name_from_id(covariate_id)
  } else {
    covariate_name <- covariate
  }

  if (!"...id" %in% names(se)) { # Don't need to do this if get_summary_zonal_statistics()
    # called get_zonal_

    # Add an ID for iterating over (with site/date/lat/long distinct)
    se <- se %>%
      add_id_for_iteration()
  }

  se_list <- se %>%
    split(.$...id)

  # Get zonal stats for all SEs
  zonal_stats <- get_zonal_stats(se_list, covariate_id, n_days, radius, spatial_stats)

  # Attach to sample events and remove ID
  se %>%
    dplyr::left_join(zonal_stats, by = "...id") %>%
    dplyr::select(-...id)
}

get_items_for_zonal_stats <- function(df, covariate_id, n_days = 365) {
  # Get sample_date
  sample_date <- df %>%
    dplyr::pull(sample_date)

  input_sample_date_end <- sample_date

  # Subtract `n_days - 1` days - so it will be `ndays - 1` days before, and the sample date
  input_sample_date_start <- sample_date - lubridate::days(n_days - 1)

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

  # Only include items that have the assets "data"
  relevant_items <- relevant_items[["features"]] %>%
    purrr::keep(\(x) {
      # No assets
      if (!"assets" %in% names(x)) {
        return(FALSE)
      }

      # Check for "data" asset
      "data" %in% names(x[["assets"]])
    })

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


get_zonal_stats <- function(se_list, covariate_id, n_days, radius, spatial_stats) {
  se_list %>%
    purrr::map(
      \(se)
      safely_get_zonal_stats_single(se,
        covariate_id,
        n_days,
        radius = radius,
        spatial_stats = spatial_stats
      ),
      .progress = TRUE
    ) %>%
    purrr::map("result") %>%
    purrr::compact() %>% # Remove those without any items/results
    purrr::list_rbind(names_to = "...id") %>%
    dplyr::mutate(n_dates = dplyr::n()) %>%
    tidyr::nest(covariates = -...id, .by = "...id") # Nest covariates
}

get_zonal_stats_single <- function(se, covariate_id, n_days = 30, radius = 1000,
                                   bands = list(1), approx_stats = FALSE,
                                   spatial_stats = c(
                                     "min", "max", "mean", "count", "sum", "std",
                                     "median", "majority", "minority", "unique",
                                     "range", "nodata", "area", "freq_hist"
                                   )) {
  # Set up zonal_stats requests by getting relevant STAC items for each sample event
  stac_items <- get_items_for_zonal_stats(se,
    covariate_id,
    n_days = n_days
  )
  # Returns a list with elements start_date, end_date, urls
  # and NULL if there are no items

  # Handle case where SE does not have any items

  if (all(is.na(stac_items[["url"]]))) {
    return(stac_items)
  }

  # Get zonal stats for each URL

  # TODO -> handle no stats returned

  # Set up requests to parallelize

  request_base <- httr2::request(zonal_stats_url) %>%
    httr2::req_user_agent("mermaidr-covariates") %>%
    httr2::req_body_json(list(
      aoi = list(
        type = "Point",
        coordinates = c(se[["longitude"]], se[["latitude"]]),
        buffer_size = radius
      ),
      image = NULL,
      stats = as.list(spatial_stats)
    )) %>%
    httr2::req_error(is_error = \(res) FALSE)

  requests <- purrr::map(
    stac_items[["url"]],
    \(x) {
      request_base %>%
        httr2::req_body_json_modify(
          image =
            list(
              url = x,
              bands = bands,
              approx_stats = approx_stats
            )
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
          }, .id = "band") %>%
          # This should happen further out
          dplyr::mutate(
            date = date,
            band = stringr::str_remove(band, "band_"),
            band = as.numeric(band)
          )
      }
    ) %>%
    purrr::list_rbind() %>%
    dplyr::bind_cols(stac_items %>% dplyr::distinct(start_date, end_date)) %>%
    # This should happen further out
    tidyr::pivot_longer(
      cols =
        dplyr::any_of(spatial_stats),
      names_to = "spatial_stat",
      values_to = "value"
    )
  # TODO -> reorder cols too, ALSO further out
}

safely_get_zonal_stats_single <- purrr::safely(get_zonal_stats_single, otherwise = NULL)

# TODO -> replace this
empty_covariates <- function(df, covariate_name) {
  covariates <- dplyr::tibble(
    covariate = covariate_name,
    start_date = NA,
    end_date = NA,
    band = NA_integer_,
    statistic = NA_character_,
    value = NA_real_
  ) %>%
    dplyr::mutate(dplyr::across(c(start_date, end_date), as.Date)) %>%
    tidyr::nest(covariates = dplyr::everything())

  df %>%
    dplyr::bind_cols(covariates)
}
