#' Get summary of zonal statistics
#'
#' @param df Sample events from \code{mermaidr}
#' @param covariate_id Covariate ID to get statistics for
#' @param n_days Number of days prior to sample date to get statistics for. Defaults to 365.
#' @param buffer Buffer around site location, in metres. Defaults to 1000.
#' @param stats Summary statistics. One of: min, max, or mean.
#'
#' @export
get_zonal_statistics <- function(se, covariate_id, n_days = 365, buffer = 1000, stats = c("min", "max", "mean")) {
  covariate_name <- get_covariate_name_from_id(covariate_id)

  # Add an ID for iterating over (with site/date/lat/long distinct)
  se <- se %>%
    add_id_for_iteration()

  se_list <- se %>%
    split(.$...id)

  # Get zonal stats for all SEs
  zonal_stats <- get_zonal_stats(se_list, covariate_id, n_days, buffer, stats)

  # Set up to group by non-stat columns
  id_cols <- zonal_stats %>%
    dplyr::select(-dplyr::any_of(stats)) %>%
    names()

  # Apply summary function to each of the cols
  zonal_stats_summary <- stats %>%
    purrr::map(\(x) {
      zonal_stats %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) %>%
        dplyr::summarise(
          dplyr::across(
            dplyr::any_of(x),
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
        )
    }) %>%
    dplyr::bind_cols()

  # Reshape zonal stats into the following format:
  # covariate, start_date, end_date, band, statistic, value
  # covariate will just be covariate name
  # band is as is, with "_band" removed
  # Put into a df-column called covariates

  if (!any(stats %in% names(zonal_stats_summary))) {
    zonal_stats_summary <- zonal_stats_summary %>%
      dplyr::bind_cols(dplyr::tibble(
        statistic = NA_character_,
        value = NA_real_,
        band = NA_character_
      ))
  } else {
    zonal_stats_summary <- zonal_stats_summary %>%
      tidyr::pivot_longer(
        cols = dplyr::any_of(stats),
        names_to = "statistic",
        values_to = "value"
      ) %>%
      dplyr::mutate(
        band = stringr::str_remove(band, "band_"),
        band = as.numeric(band)
      )
  }

  zonal_stats_df <- zonal_stats_summary %>%
    dplyr::right_join(se, by = "...id") %>%
    dplyr::mutate(
      covariate = covariate_name,
      start_date = start_date,
      end_date = end_date
    ) %>%
    dplyr::select(...id, covariate, start_date, end_date, band, statistic, value) %>%
    tidyr::nest(covariates = -...id)

  # Re-attach to existing df, even if it was not distinct
  se %>%
    dplyr::left_join(zonal_stats_df, by = "...id") %>%
    dplyr::select(-...id)
}

get_items_for_zonal_stats <- function(df, covariate_id, n_days = 365) {
  # Get sample_date
  sample_date <- df %>%
    dplyr::pull(sample_date)

  # Add 1 day -> end will be midnight on this day
  input_sample_date_end <- sample_date + lubridate::days(1)

  # Subtract `n_days` days
  input_sample_date_start <- sample_date - lubridate::days(n_days)

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
        urls = NA
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
    purrr::map(\(x) {
      x[["assets"]][["data"]][["href"]]
    })

  return(
    dplyr::tibble(
      start_date = start_date,
      end_date = end_date,
      urls = list(zonal_stats_urls)
    )
  )
}


get_zonal_stats <- function(se_list, covariate_id, n_days, buffer, stats) {
  se_list %>%
    purrr::map(
      \(se)
      get_zonal_stats_single(se,
        covariate_id,
        n_days,
        buffer = buffer,
        stats = stats
      ),
      .progress = TRUE
    ) %>%
    purrr::compact() %>% # Remove those without any items/results
    purrr::list_rbind(names_to = "...id")
}

get_zonal_stats_single <- function(se, covariate_id, n_days = 30, buffer = 1000,
                                   bands = list(1), approx_stats = FALSE,
                                   stats = c(
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
  if (is.na(stac_items[["urls"]])) {
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
        buffer_size = buffer
      ),
      image = NULL,
      stats = as.list(stats)
    )) %>%
    httr2::req_error(is_error = \(res) FALSE)

  requests <- purrr::map(
    stac_items[["urls"]][[1]],
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
      \(res) {
        res %>%
          httr2::resp_body_json() %>%
          purrr::map_dfr(\(x) {
            x <- purrr::map(x, \(x) if (is.null(x)) NA else x)
            dplyr::as_tibble(x)
          }, .id = "band")
      }
    ) %>%
    purrr::list_rbind() %>%
    dplyr::bind_cols(stac_items)
}


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
