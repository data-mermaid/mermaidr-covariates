#' Get zonal statistics
#'
#' Get zonal statistics. Using \code{spatial_stats} to spatially summarise covariates data, for each relevant date prior to the sample event date. For example, for a daily covariate like Daily Sea Surface Temperature, the function returns SST for *each* of the \code{n_days} prior to the sample event date. If the covariate is periodic, e.g. occurring every 5 years, it returns the most recent value. If the covariate only occurs once (e.g. 50 Reefs+ prioritization), it returns that data.
#'
#' @param se Sample events from \code{mermaidr}
#' @param covariate Covariate to get statistics for. Both covariate title or ID are permitted. Run \code{\link{list_covariates}} to see available covariates.
#' @param dataset Dataset within the covariate. Not required in most cases, when there is only one dataset. Run \code{\link{list_datasets_for_covariate}} to see datasets and bands.
#' @param bands Bands within the dataset. Not required in most cases, when there is only one band. When required, can be numeric or named band. Run \code{\link{list_datasets_for_covariate}} to see datasets and bands.
#' @param n_days Number of days to get statistics for. Includes the sample date
#'  itself, and days prior to it -- e.g., 365 days would include the sample date
#'  and the 364 days prior. Only relevant for covariates that are daily; otherwise ignored.
#' @param radius Radius around site location, in metres. Defaults to 0 (just the site location itself).
#' @param spatial_stats Spatial statistics -- used to summarise all data around
#' the site location, according to the \code{radius} set. If \code{radius} is 0, then \code{spatial_stats} is not relevant; it is just the value itself.
#' @param date_col Sample date column -- used for date-dependent covariates (e.g. daily or periodic). Defaults to "sample_date".
#' @param .progress Whether to show progress bar and time remaining. Defaults to TRUE.
#'
#' @export
get_zonal_statistics <- function(se, covariate,
                                 spatial_stats = "mean",
                                 radius = 0,
                                 n_days = NULL,
                                 dataset = NULL,
                                 bands = NULL,
                                 date_col = "sample_date",
                                 .progress = TRUE) {
  if (nrow(se) == 0) {
    stop("No sample events to get zonal statistics for.", .call = FALSE)
  }

  covariate <- get_covariate_id(covariate)
  covariate_name <- get_covariate_name_from_id(covariate)

  # Check inputs -- returns the dataset type, its bands/columns, and URL
  covariate_info <- check_inputs_zonal_stats(
    covariate, dataset, bands, n_days, radius,
    spatial_stats
  )

  # Add an ID for iterating over (splitting by lat/long/sample date,
  # accounting for overlapping sample dates to reduce duplicating API calls)
  se <- se %>%
    add_id_for_iteration(date_col, n_days)

  # Rounding SEs lat/long to 5 digits -- otherwise, causes ID issues when not really important
  se <- se %>%
    dplyr::mutate(dplyr::across(c(latitude, longitude), \(x) round(x, 5)))

  # Get zonal stats for all SEs
  zonal_stats <- get_zonal_stats(se, covariate, covariate_name, dataset, covariate_info[["covariate_interval"]],
    n_days = n_days, radius = radius, bands = covariate_info[["bands"]],
    bands_labels = covariate_info[["bands_labels"]], date_col = date_col,
    spatial_stats = spatial_stats, type = type, .progress = .progress
  )

  # Attach to sample events and remove ID
  se <- se %>%
    dplyr::left_join(zonal_stats,
      by = "...id",
      relationship = "many-to-many"
    ) %>%
    dplyr::select(-...id)

  keep_relevant_zonal_stats(se, covariate_info[["covariate_interval"]], n_days, date_col)
}

check_inputs_zonal_stats <- function(covariate, dataset = NULL, bands = NULL,
                                     n_days = NULL, radius = 0, spatial_stats = NULL) {
  items <- get_collection_items(covariate)

  # Use first item for checks
  first_item <- items[[1]]

  # Check the covariate contains raster data
  cog_assets <- get_cog_assets(first_item)

  # If there are NO cog assets, they may need to use attach_covariate_data() -- message with that instead
  if (identical(cog_assets, NA_character_)) {
    parquet_assets <- get_parquet_assets(first_item)
    if (!identical(parquet_assets, NA_character_)) {
      usethis::ui_stop("You cannot get zonal statistics for this covariate. Use `attach_covariate_data()` instead.")
    }
  }

  # If there are multiple cog assets, they need to supply `dataset`
  if (length(cog_assets) > 1) {
    if (is.null(dataset)) {
      usethis::ui_stop("This covariate has multiple datasets to get zonal statistics for. Please specify using the `dataset` argument. Options are: {comma_sep_quoted(cog_assets)}.")
    }

    # If they have supplied `dataset`, it needs to match one of the cog assets

    # Can do the same error for either -- just that `dataset` needs to match one of them, and here are the options
  }

  # Set asset_name to `cog_assets` if length 1, to `dataset` if not
  asset_name <- ifelse(length(cog_assets) == 1, cog_assets, dataset)
  assets <- first_item[["assets"]]
  asset <- assets[[asset_name]]

  # Check bands
  asset_bands <- get_asset_bands_or_columns(asset)
  bands_named <- !all(is.na(asset_bands[["name"]]))

  if (bands_named) {
    bands_err <- paste(capture.output(print(asset_bands)), collapse = "\n")
    bands_name_err <- " You may specify by band number or by name."
  } else {
    bands_err <- paste0(asset_bands[["band"]], collapse = ", ")
    bands_name_err <- ""
  }

  if (nrow(asset_bands) == 1) {
    # If there is only 1 asset, default to 1 -- even if they have not supplied
    # If they DID supply, and it is WRONG, need to error

    if (!is.null(bands)) {
      # Confirm that all bands are valid ones
      bands <- unlist(bands)
      selected_bands <- asset_bands %>%
        dplyr::filter(band %in% bands | name %in% bands)

      if (nrow(selected_bands) != length(bands)) {
        usethis::ui_stop("Invalid band(s) given in `bands`.{bands_name_err}\nOptions: {bands_err}")
      }
    }
    bands <- 1
    bands_labels <- NULL
  } else {
    # If there are multiple, they must supply -- they can supply multiple, but need to be specific
    # They can supply by name or by number
    if (is.null(bands)) {
      usethis::ui_stop(
        "Please specify which band(s) to use in `bands`.{bands_name_err}\nOptions: \n{bands_err}."
      )
    } else {
      # Confirm that all bands are valid ones
      bands <- unlist(bands)
      selected_bands <- asset_bands %>%
        dplyr::filter(band %in% bands | name %in% bands)

      if (nrow(selected_bands) != length(bands)) {
        usethis::ui_stop("Invalid band(s) given in `bands`.{bands_name_err}\nOptions: \n{bands_err}")
      }

      # Ensure `bands` are the numbers, not the names
      bands <- selected_bands[["band"]]

      # Prep band info for return: `bands` as a list, `bands_labels` as a df
      bands <- as.list(bands)

      # If there are no names for the bands, `bands_labels` is NULL
      if (!bands_named) {
        bands_labels <- NULL
      } else {
        bands_labels <- selected_bands
      }
    }
  }

  # Check that they have supplied sample_date if the covariate is date-dependent
  # TODO: not done
  covariate_interval <- get_covariate_interval(covariate)

  # Check they have supplied n_days if the covariate is not once or periodic
  if (!covariate_interval %in% c("once", "periodic") & is.null(n_days)) {
    usethis::ui_stop("Please specify number of days to get covariate for in `n_days`.")
  }

  # Check they have supplied spatial_stat if radius != 0
  if (radius > 0 & is.null(spatial_stats)) {
    usethis::ui_stop("Please specify `spatial_stats`.")
  }

  # Return:
  # covariate_interval
  # bands (as list)
  # bands_labels (df with labels, if relevant)
  list(
    covariate_interval = covariate_interval,
    bands = as.list(bands),
    bands_labels = bands_labels
  )
}

get_items_for_zonal_stats_periodic <- function(se, covariate_id, covariate_interval) {
  # Rather than getting items each time, just get all of the items, then attach the relevant one
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
        date = as.Date(dplyr::coalesce(x[["properties"]][["datetime"]], x[["properties"]][["end_datetime"]])),
        url = x[["assets"]][[get_cog_assets(x)]][["href"]]
      )
    })

  if (covariate_interval == "once") {
    stac_items <- se %>%
      dplyr::bind_cols(cog_assets) %>%
      dplyr::mutate(
        ...secondary_id = glue::glue("{...id}__{date}")
      )
  } else {
    stac_items <- se %>%
      dplyr::left_join(
        cog_assets,
        dplyr::join_by(closest(...date_temp >= date))
      ) %>%
      dplyr::mutate(
        ...secondary_id = glue::glue("{...id}__{date}")
      )
  }

  stac_items
}

get_items_for_zonal_stats_daily <- function(se, covariate_id, dataset, n_days = 365) {
  # Since intervals are combined, we can no longer just go back n_days from sample_date
  df <- se %>%
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
    items <- dplyr::tibble(date = NA, url = NA)
  } else {
    relevant_items <- relevant_items[["features"]]

    # Get the dates of items, to have start/end date
    item_dates <- relevant_items %>%
      purrr::map_chr(\(x) {
        properties <- x[["properties"]]
        if ("datetime" %in% names(properties)) {
          properties[["datetime"]]
        } else {
          usethis::ui_stop("Unexpected error, please report: no datetime attached to STAC asset")
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
          if (!is.null(dataset)) {
            assets[[dataset]][["href"]]
          }
        } else {
          assets[[cog_asset]][["href"]]
        }
      })

    items <- dplyr::tibble(
      date = item_dates,
      url = zonal_stats_urls
    )
  }

  se %>%
    dplyr::select(...id, latitude, longitude) %>%
    dplyr::bind_cols(items) %>%
    dplyr::mutate(...secondary_id = glue::glue("{...id}__{date}")) %>%
    dplyr::distinct()
}

get_zonal_stats <- function(se, covariate_id, covariate_name,
                            dataset, covariate_interval,
                            n_days, radius, bands, bands_labels,
                            date_col, spatial_stats, type, .progress = TRUE) {
  # Potentially get the STAC items up front
  get_stac_items_now <- covariate_interval %in% c("once", "periodic")

  if (get_stac_items_now) {
    se <- get_items_for_zonal_stats_periodic(se, covariate_id, covariate_interval)
  }

  # If there are no items, then zonal_stats is empty
  if (all(is.na(se[["date"]])) & get_stac_items_now) {
    zonal_stats <- create_empty_zonal_stats(se, spatial_stats, interval = FALSE)
  } else {
    # Split SEs up into list that can be processed iteratively
    se_list <- se %>%
      split_for_chunking(covariate_interval, n_days)

    zonal_stats <- se_list %>%
      purrr::map(
        \(se)
        get_zonal_stats_chunked(
          se,
          covariate_id,
          covariate_interval,
          dataset,
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
  }

  zonal_stats <- zonal_stats %>%
    dplyr::mutate(
      band = stringr::str_remove(band, "band_")
    ) %>%
    # reorganize to have a column `spatial_stat` and `value`, instead of a column for each stat
    tidyr::pivot_longer(
      cols = dplyr::all_of(spatial_stats),
      names_to = "spatial_stat",
      values_to = "value"
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      band = forcats::fct_expand(band, as.character(unlist(bands))),
      spatial_stat = forcats::fct_expand(spatial_stats)
    ) %>%
    tidyr::complete(...id, band, spatial_stat) %>%
    # Remove NA bands, they were "completed" but now not relevant
    dplyr::filter(!is.na(band)) %>%
    dplyr::mutate(
      band = as.character(band), # Make character first so that it doesn't become FACTOR level, i.e. automatically 1, 2, ..., even if band = 4, 5
      band = as.numeric(band),
      spatial_stat = as.character(spatial_stat),
      covariate = covariate_name
    ) %>%
    # reorganize covariates columns
    dplyr::select(...id, covariate, date, band, spatial_stat, value)

  if (!is.null(bands_labels)) {
    zonal_stats <- zonal_stats %>%
      dplyr::left_join(bands_labels, by = "band") %>%
      dplyr::relocate(name, .after = "band") %>%
      dplyr::rename(band_name = name)
  }

  zonal_stats %>%
    tidyr::nest(zonal_statistics = -...id, .by = "...id")
}

get_zonal_stats_chunked <- function(se, covariate_id, covariate_interval, dataset, n_days = 30, radius = 1000,
                                    bands = list(1), approx_stats = FALSE,
                                    spatial_stats = c(
                                      "min", "max", "mean", "count", "sum", "std",
                                      "median", "majority", "minority", "unique",
                                      "range", "nodata", "area", "freq_hist"
                                    ),
                                    type) {
  # If covariate_interval is daily, we need to get the stac items for each chunk

  if (covariate_interval == "daily") {
    # Set up zonal_stats requests by getting relevant STAC items for each sample event
    stac_items <- se %>%
      split(.$...id) %>%
      purrr::map_dfr(\(x) get_items_for_zonal_stats_daily(x, covariate_id, dataset, n_days = n_days))
  } else {
    # Otherwise, `se` already has the covariate info attached
    stac_items <- se %>%
      dplyr::distinct(...id, latitude, longitude, ...secondary_id, url, date)
  }

  # Handle case where SEs do not have any items

  if (all(is.na(stac_items[["url"]]))) {
    res <- create_empty_zonal_stats(se, spatial_stats)

    return(res)
  }

  # Get zonal stats for each URL
  zonal_stats <- GET_zonal_stats(stac_items, radius, bands, approx_stats, spatial_stats, type = "raster")

  # Not all SEs have zonal stats, if e.g. they got filtered out by date issues
  # So make sure there is a row for each SE, with band and spatial_stat the same, with value NA, date NA

  if (covariate_interval %in% c("annual", "periodic")) {
    if (length(unique(se[["...secondary_id"]])) != length(unique(zonal_stats[["...secondary_id"]]))) {
      usethis::ui_stop("Unexpected error, please report: SE secondary IDs do not match zonal stats'.")
    }

    se %>%
      dplyr::select(...id, ...secondary_id) %>%
      dplyr::left_join(zonal_stats, by = c("...id", "...secondary_id"))
  } else {
    se %>%
      dplyr::select(...id) %>%
      dplyr::left_join(zonal_stats, by = "...id")
  }
}

create_empty_zonal_stats <- function(se, spatial_stats, interval = TRUE) {
  zonal_stats <- se %>%
    dplyr::distinct(...id) %>%
    dplyr::bind_cols(
      dplyr::tibble(
        start_date = NA,
        end_date = NA,
        date = NA,
        band = NA,
        spatial_stat = spatial_stats,
        value = NA
      )
    )

  if (!interval) {
    zonal_stats <- zonal_stats %>%
      dplyr::select(-start_date, -end_date)
  }

  zonal_stats %>%
    tidyr::pivot_wider(names_from = spatial_stat, values_from = value)
}

keep_relevant_zonal_stats <- function(se, covariate_interval, n_days, date_col) {
  # Only keep zonal stats that are actually relevant for SE, not all combined intervals
  # Also updating start_date and end_date

  # Only do this if daily, otherwise just return SE

  if (covariate_interval %in% c("once", "periodic")) {
    return(se %>%
      dplyr::select(-...date_temp))
  }

  covariates_cols <- se %>%
    dplyr::pull(zonal_statistics) %>%
    purrr::pluck(1) %>%
    names()

  se_flag_relevant <- se %>%
    dplyr::rename_with(\(x) "...date_temp", dplyr::all_of(date_col)) %>%
    dplyr::mutate(...date_temp = as.Date(...date_temp)) %>%
    tidyr::unnest(zonal_statistics) %>%
    dplyr::mutate(
      ...start_date = ...date_temp - (n_days - 1),
      ...end_date = ...date_temp,
      ...date_relevant = (date >= ...start_date & date <= ...end_date) | (is.na(date))
    )

  se_relevant <- se_flag_relevant %>%
    dplyr::filter(...date_relevant) %>%
    dplyr::select(-...date_relevant, -...start_date, -...end_date)

  se_relevant %>%
    tidyr::nest(zonal_statistics = dplyr::all_of(covariates_cols)) %>%
    dplyr::rename_with(\(x) date_col, ...date_temp)
}
