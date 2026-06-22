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

get_asset_type <- function(asset) {
  cog <- is_cog_asset(asset)
  if (cog) {
    return("cog")
  }

  parquet <- is_parquet_asset(asset)
  if (parquet) {
    return("parquet")
  }

  NA_character_
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

get_x_assets <- function(item, common_asset_names, type) {
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

get_cog_assets <- function(item) {
  get_x_assets(item, c("data", "Cloud Optimized GeoTIFF", "cog", "image"), "cog")
}

get_parquet_assets <- function(item) {
  get_x_assets(item, c("data", "parquet", "geoparquet", "vector"), "parquet")
}

get_all_asset_types <- function(item) {
  item[["assets"]] %>%
    purrr::map_chr(get_asset_type)
}

get_collection_type <- function(collection) {

  collection <- get_covariate_id(collection)

  # Look at the first item
  item <- rstac::stac(stac_url) %>%
    rstac::collections(collection) %>%
    rstac::items(limit = 1) %>%
    rstac::get_request()

  item <- item[["features"]][[1]]

  # Look for COG assets and parquet assets
  cog_assets <- get_cog_assets(item)
  parquet_assets <- get_parquet_assets(item)

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
get_asset_bands_or_columns <- function(asset) {
  # If parquet, get columns
  # If COG, get bands

  asset_type <- get_asset_type(asset)

  if (is.na(asset_type)) {
    return(NULL)
  }

  names_lookup <- switch(asset_type,
    "cog" = "raster:bands",
    "parquet" = "table:columns"
  )

  if (is.null(asset[[names_lookup]])) {
    return(dplyr::tibble(band = 1, name = NA_character_))
  }

  bands_or_cols <- purrr::map(
    asset[[names_lookup]],
    \(x) {
      x_name <- x[["name"]]
      if (is.null(x_name)) {
        x_name <- NA_character_
      }
      dplyr::tibble(name = x_name)
    }
  )

  # If cog, need to return what # band they are
  id_col <- switch(asset_type,
    "cog" = "band",
    "parquet" = NULL
  )

  bands_or_cols %>%
    purrr:::list_rbind(names_to = id_col)
}
