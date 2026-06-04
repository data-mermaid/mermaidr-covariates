before_date_to_datetime <- function(date) {
  glue::glue("../{date}T00:00:00Z")
}

after_date_to_datetime <- function(date) {
  glue::glue("{date}T00:00:00Z/..")
}

start_end_to_interval <- function(start_date, end_date) {
  glue::glue("{start_date}T00:00:00Z/{end_date}T23:59:59Z")
}

get_covariate_name_from_id <- function(id) {
  rstac::stac(stac_url) %>%
    rstac::collections(id) %>%
    rstac::get_request() %>%
    purrr::pluck("title")
}

add_id_for_iteration <- function(df, date_col, n_days) {
  df <- df %>%
    dplyr::mutate(
      ...date_temp = !!rlang::sym(date_col),
      ...date_temp = as.Date(...date_temp)
    )

  # Deduplicate overlapping API calls
  # e.g. if they have two samples within a year (or whatever n_days is),
  # many of the items will be the same
  # So determine which overlap, then iterate over those
  # Rather than just the site and sample date

  df %>%
    dplyr::group_by(latitude, longitude) %>% # For "site", actually just use lat/long
    dplyr::arrange(...date_temp) %>%
    dplyr::mutate(
      ...prev_date = dplyr::lag(...date_temp),
      ...diff = ...date_temp - ...prev_date,
      ...over_n_days = ...diff > n_days,
      ...start_new = dplyr::coalesce(...over_n_days, TRUE), # So that the first row is filled in
      ...group = dplyr::case_when(...start_new ~ ...date_temp)
    ) %>%
    tidyr::fill(...group, .direction = "down") %>%
    dplyr::group_by(latitude, longitude, ...group) %>%
    dplyr::mutate(
      ...start_date = min(...date_temp),
      ...end_date = max(...date_temp)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(...id = glue::glue("{latitude}_{longitude}_{...start_date}_{...end_date}")) %>%
    dplyr::select(dplyr::all_of(names(df)), ...start_date, ...end_date, ...id) %>%
    dplyr::select(-...date_temp)
}

add_id_for_joining <- function(df, date_col) {
  df %>%
    dplyr::mutate(
      ...date_temp = !!rlang::sym(date_col),
      ...date_temp = as.Date(...date_temp),
      ...join_id = glue::glue("{project}_{site}_{latitude}_{longitude}_{...date_temp}_{dplyr::row_number()}")
    ) %>%
    dplyr::select(-...date_temp)
}

split_for_chunking <- function(df, covariate_id, n_days) {
  covariate_interval <- determine_covariate_interval(covariate_id)

  # Potentially split not by ...id, if n_days is small
  if (covariate_interval == "daily" & n_days < chunk_size) {
    se_per_chunk <- ceiling(chunk_size / n_days)

    df %>%
      dplyr::mutate(...chunk = (dplyr::row_number() - 1) %/% se_per_chunk) %>%
      split(.$...chunk)
  } else {
    df %>%
      split(.$...id)
  }
}

combine_from_chunking <- function(df) {
  df %>%
    dplyr::bind_rows()
}

lookup_collection <- function(x) {
  rstac::stac(stac_url) %>%
    rstac::collections(collection_id = x) %>%
    rstac::get_request()
}

safely_lookup_collection <- purrr::safely(lookup_collection)

check_covariate_id <- function(x) {
  # Checks if `x` is a covariate ID (collection ID)
  res <- safely_lookup_collection(x)

  is.null(res$error)
}

lookup_covariate_id_by_name <- function(name) {
  covariate <- list_covariates() %>%
    dplyr::filter(title == name)

  if (nrow(covariate) == 0) {
    return(NA)
  }

  covariate[["id"]]
}

get_covariate_id <- function(x) {
  # If x is an ID, just return it
  is_covariate_id <- check_covariate_id(x)

  if (is_covariate_id) {
    return(x)
  }

  # Otherwise, assume it is a name and look up the ID
  covariate_id <- lookup_covariate_id_by_name(x)

  # If no ID, error
  if (is.na(covariate_id)) {
    stop(x, " is not a valid covariate title or ID.", call. = FALSE)
  }

  covariate_id
}

determine_covariate_interval <- function(covariate_id) {
  # Determine whether a covariate is:
  # daily
  # monthly
  # annually
  # once only

  # by looking at its start date, adding one year, then seeing how many items are returned

  covariates <- list_covariates()

  start_date <- covariates %>%
    dplyr::filter(id == covariate_id) %>%
    dplyr::pull(start_date)

  items <- rstac::stac(stac_url) %>%
    rstac::stac_search(
      collections = covariate_id,
      datetime = start_end_to_interval(start_date, start_date + lubridate::years(1))
    ) %>%
    rstac::get_request()

  n_items <- items$numberMatched

  dplyr::case_when(
    n_items < 2 ~ "annual/once",
    n_items < 15 ~ "monthly",
    n_items >= 15 & n_items <= 300 ~ "check",
    n_items > 300 ~ "daily"
  )
}

# Determining whether a covariate is vector/raster/vector + raster
# Taking cue from isCogAsset() etc
# https://github.com/data-mermaid/mermaid-zonal-stats-ui/blob/main/src/services/stacApi.js#L45

is_x_asset <- function(asset, mime_types, file_extensions) {
  if (is.null(asset)) {
    return(FALSE)
  }

  mime_type_matches <- stringr::str_detect(
    asset[["type"]], mime_types
  )

  if (any(mime_type_matches)) {
    return(TRUE)
  }

  href <- tolower(asset[["href"]])

  file_extension_matches <- stringr::str_ends(href, file_extensions)

  if (any(file_extension_matches)) {
    return(TRUE)
  }

  FALSE
}

is_cog_asset <- function(asset) {
  is_x_asset(
    asset,
    c(
      "profile=cloud-optimized", "image/tiff", "application/geotiff",
      "image/tiff; application=geotiff"
    ),
    c(".tif", ".tiff")
  )
}

is_parquet_asset <- function(asset) {
  is_x_asset(
    asset,
    c("parquet", "application/x-parquet", "application/vnd.apache.parquet"),
    ".parquet"
  )
}

matching_x_asset <- function(assets, asset_names, type) {
  matches_x_asset <- purrr::map_lgl(
    asset_names,
    \(asset_name) {
      switch(type,
        "cog" = is_cog_asset(assets[[asset_name]]),
        "parquet" = is_parquet_asset(assets[[asset_name]])
      )
    }
  )

  names(matches_x_asset) <- asset_names

  if (any(matches_x_asset)) {
    matching_assets <- matches_x_asset %>%
      purrr::keep(\(x) x) %>%
      names()

    return(matching_assets)
  }

  NULL
}

find_x_assets <- function(item, common_asset_names, type) {
  assets <- item[["assets"]]

  # Check common asset names first
  matching_assets <- matching_x_asset(assets, common_asset_names, type)

  if (!is.null(matching_assets)) {
    return(matching_assets)
  }

  # As a fallback, check all assets
  matching_assets <- matching_x_asset(assets, names(assets), type)

  if (!is.null(matching_assets)) {
    return(matching_assets)
  }

  # Otherwise, no matching assets
  NA_character_
}

find_cog_assets <- function(item) {
  find_x_assets(item, c("data", "Cloud Optimized GeoTIFF", "cog", "image"), "cog")
}

find_parquet_assets <- function(item) {
  find_x_assets(item, c("data", "parquet", "geoparquet", "vector"), "parquet")
}

check_collection_type <- function(collection) {
  # Look at the first item
  item <- rstac::stac(stac_url) %>%
    rstac::collections(collection) %>%
    rstac::items(limit = 1) %>%
    rstac::get_request()

  item <- item[["features"]][[1]]

  # Look for COG assets and parquet assets
  cog_assets <- find_cog_assets(item)
  parquet_assets <- find_parquet_assets(item)

  is_raster <- !identical(cog_assets, NA_character_)
  is_vector <- !identical(parquet_assets, NA_character_)

  if (is_raster & is_vector) {
      "raster + vector"
  } else if (is_raster) {
      "raster"
  } else if (is_vector) {
      "vector"
  } else {
      "unknown"
  }
}
