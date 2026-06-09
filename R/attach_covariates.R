attach_covariate_data <- function(se, covariate, dataset = NULL, col = NULL, date_col = "sample_date") {
  # Get covariate items
  items <- get_collection_items(covariate)
  items

  # Check inputs
  type_and_bands_cols <- check_inputs_covariate_data(items, covariate, dataset, col, date_col)

  # Based on type,

  #
}

get_collection_items <- function(x) {
  items <- rstac::stac(stac_url) %>%
    rstac::collections(collection_id = x) %>%
    rstac::items() %>%
    rstac::get_request()

  items[["features"]]
}

check_inputs_covariate_data <- function(items, covariate, dataset = NULL, col = NULL, date_col = "sample_date") {
  # If the covariate contains more than one item, they need to give date information
  if (length(items) > 1 & is.null(date_col)) {
    usethis::ui_stop("Covariate \"{covariate}\" is date-dependent. Please supply a date column in `date_col`.")
  }

  # Use first item for checks
  first_item <- items[[1]]
  assets <- first_item[["assets"]]

  # If there is more than one COG/parquet dataset (asset), they need to specify which to use
  asset_types <- get_all_asset_types(first_item) %>%
    purrr::keep(\(x) !is.na(x))

  assets_names <- paste0(names(asset_types), collapse = "\", \"")
  assets_names <- glue::glue('"{assets_names}"')

  if (!is.null(dataset)) {
    # Check that they only specified one dataset
    if (length(dataset) > 1) {
      usethis::ui_stop("You may only specify one dataset.")
    }

    # Check that specified asset exists
    asset <- assets[[dataset]]
    if (is.null(asset)) {
      usethis::ui_stop(
        "Dataset \"{dataset}\" does not exist. Valid datasets are: {assets_names}."
      )
    }
  }

  if (length(asset_types) > 1 & is.null(dataset)) {
    usethis::ui_stop("Covariate \"{covariate}\" contains more than one dataset. Please specify which to use in `dataset`.
      Options: {assets_names}.")
  }

  if (length(asset_types) == 1) {
    dataset <- names(asset_types)
  }

  asset <- assets[[dataset]]
  asset_type <- asset_types[[dataset]]

  # If there is more than one col (band or column) in the specified asset, they need to specify
  bands_cols <- get_asset_bands_or_columns(asset)

  if (nrow(bands_cols) > 1 & is.null(col)) {
    if (asset_types[[dataset]] == "parquet") {
      cols <- bands_cols[["name"]] %>% paste0(collapse = '", "')
      cols <- glue::glue('"{cols}"')

      usethis::ui_stop(
        "Dataset \"{dataset}\" contains more than one column of data. Please specify which to use in `col`.
      Options: {cols}."
      )
    } else {
      tibble_string <- paste(capture.output(print(bands_cols)), collapse = "\n")
      usethis::ui_stop(
        "Dataset \"{dataset}\" contains more than one band of data. Please specify which to use in `col`. You may specify by band number or by name.\nOptions: \n{tibble_string}"
      )
    }
  }

  # Check that band/col are valid
  if (asset_type == "cog") {
    if (is.null(col)) { # There is, by definition, only one band, otherwise would have errored with col being NULL
      col <- bands_cols[["band"]]
    }
    band <- col
    numeric_band <- suppressWarnings(as.numeric(band))
    if (is.numeric(band)) {
      valid_band <- band %in% bands_cols[["band"]]
    } else if (!is.na(numeric_band)) {
      valid_band <- band %in% bands_cols[["band"]]
      if (valid_band) {
        band <- numeric_band
      }
    } else if (is.character(band)) {
      valid_band <- band %in% bands_cols[["name"]]

      if (valid_band) {
        band <- bands_cols %>%
          dplyr::filter(name == !!band) %>%
          dplyr::pull(band)
      }
    }

    if (!valid_band) {
      tibble_string <- paste(capture.output(print(bands_cols)), collapse = "\n")
      usethis::ui_stop(
        "Band \"{col}\" is not a valid band.\nOptions (You may specify by band number or by name): \n{tibble_string}"
      )
    }
  } else if (asset_type == "parquet") {
    valid_col <- col %in% bands_cols[["name"]]

    if (!valid_col) {
      cols <- bands_cols[["name"]] %>% paste0(collapse = '", "')
      cols <- glue::glue('"{cols}"')
      usethis::ui_stop(
        "Column \"{col}\" is not valid. \nOptions: \n{cols}"
      )
    }
  }

  # If all inputs pass, return info on the data set's type and its bands/cols
  res <- list(
    asset_type = asset_type,
    bands_cols = bands_cols
  )

  # If it is a cog, attach the band NUMBER too
  if (asset_type == "cog") {
    res <- append(res, list(band = band))
  }

  return(res)
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

get_collection_bands_and_columns <- function(collection) {
  # Look through all assets -- determine if each is cog or parquet, then get either bands or columns

  # Look at the first item
  item <- rstac::stac(stac_url) %>%
    rstac::collections(collection) %>%
    rstac::items(limit = 1) %>%
    rstac::get_request()

  item <- item[["features"]][[1]]

  # Look through all assets
  assets <- item[["assets"]]

  # Determine asset type
  bands_or_cols <- purrr::map(
    assets, get_asset_bands_or_columns
  )

  names(bands_or_cols) <- names(assets)

  bands_or_cols %>%
    purrr::compact()
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
