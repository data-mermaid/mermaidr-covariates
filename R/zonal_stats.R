#' Get summary of zonal statistics
#'
#' @param df Sample events from \code{mermaidr}
#' @param covariate_id Covariate ID to get statistics for
#' @param n_days Number of days prior to sample date to get statistics for. Defaults to 365.
#' @param buffer Buffer around site location, in metres. Defaults to 1000.
#' @param stats Summary statistics. One of: min, max, or mean.
#'
#' @export
get_zonal_statistics <- function(df, covariate_id, n_days = 365, buffer = 1000, stats = c("min", "max", "mean")) {
  covariate_name <- rstac::stac(stac_url) %>%
    rstac::collections(covariate_id) %>%
    rstac::get_request() %>%
    purrr::pluck("title")

  # Allow for the possibility that they have more than one record at each site at each date -> make distinct for them
  df <- df %>%
    dplyr::mutate(...id = glue::glue("{site}_{sample_date}"))

  df_distinct <- df %>%
    dplyr::distinct(site, latitude, longitude, sample_date, ...id) %>%
    split(.$...id)

  # Set up zonal_stats requests by getting relevant STAC items for each sample event
  zonal_stats_setup <- withCallingHandlers(
    purrr::map(
      df_distinct,
      \(x)
      get_items_for_zonal_stats(x,
        covariate_id,
        n_days = n_days
      )
    ),
    purrr_error_indexed = function(err) {
      rlang::cnd_signal(err$parent)
    }
  )

  zonal_stats_setup_df <- zonal_stats_setup %>%
    purrr::list_rbind(names_to = "...id")

  # TODO -> handle the case where an SE does not have any items, do not include it in the following call

  # Now get zonal stats for each SE
  # TODO -> add back purrr error indexed?
  zonal_stats <- withCallingHandlers(
    purrr::map2(
      df_distinct,
      zonal_stats_setup,
      \(se, setup)
      get_zonal_stats_single(se, setup,
        buffer = buffer,
        stats = stats
      ),
      .progress = TRUE
    )
  ) %>%
    purrr::list_rbind(names_to = "...id")

  # TODO -> separately handle case where an SE doesn't return anything
  # I don't think the API will return 0 rows here, but just an NA
  # The 0 rows is more likely to come from above, when fetching the items
  if (nrow(zonal_stats) == 0) {
    browser()
  }

  # Set up to group by non-stat columns
  id_cols <- zonal_stats %>%
    dplyr::select(-dplyr::all_of(stats)) %>%
    names()

  # Apply summary function to each of the cols
  zonal_stats_summary <- stats %>%
    purrr::map(\(x) {
      zonal_stats %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) %>%
        dplyr::summarise(
          dplyr::across(
            dplyr::all_of(x),
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
  zonal_stats_df <- zonal_stats_summary %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(stats),
      names_to = "statistic",
      values_to = "value"
    ) %>%
    dplyr::right_join(df, by = "...id") %>%
    dplyr::left_join(zonal_stats_setup_df, by = "...id") %>%
    dplyr::mutate(
      band = stringr::str_remove(band, "band_"),
      band = as.numeric(band),
      covariate = covariate_name,
      start_date = start_date,
      end_date = end_date
    ) %>%
    dplyr::select(...id, covariate, start_date, end_date, band, statistic, value) %>%
    tidyr::nest(covariates = -...id)

  # Re-attach to existing df, even if it was not distinct
  df %>%
    dplyr::left_join(zonal_stats_df, by = "...id") %>%
    dplyr::select(-...id)
}

get_items_for_zonal_stats <- function(df, covariate_id, n_days = 365) {
  # , buffer = 1000
  # , stats = c("min", "max", "mean")) {

  # Get covariate name, since ID is not informative
  covariate_info <- rstac::stac(stac_url) %>%
    rstac::collections(covariate_id) %>%
    rstac::get_request()

  covariate_name <- covariate_info[["title"]]

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
    return(list(
      start_date = NULL,
      end_date = NULL,
      urls = NULL
    ))
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
    return(list(
      start_date = NULL,
      end_date = NULL,
      urls = NULL
    ))
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

get_zonal_stats_single <- function(se, setup, buffer = 1000,
                                   bands = list(1), approx_stats = FALSE,
                                   stats = c(
                                     "min", "max", "mean", "count", "sum", "std",
                                     "median", "majority", "minority", "unique",
                                     "range", "nodata", "area", "freq_hist"
                                   )) {
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
    setup[["urls"]][[1]],
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

  purrr::map(
    res,
    \(res) {
      res %>%
        httr2::resp_body_json() %>%
        purrr::map_dfr(\(x) {
          x <- purrr::map(x, \(x) if (is.null(x)) NA else x)
          dplyr::as_tibble(x)
        }, .id = "band")
    }
  ) %>%
    purrr::list_rbind()
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
