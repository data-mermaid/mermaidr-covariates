#' Attach covariate data
#'
#' Attach covariate data. Not date dependent -- only attach based on sample event location.
#'
#' @param se Sample events from \code{mermaidr}
#' @param covariate Covariate to attach. Both covariate title or ID are permitted. Run \code{list_covariates()} to see available covariates.
#' @param dataset Dataset within the covariate. Not required when there is only one dataset. Run \code{\link{list_datasets_for_covariate}} to see datasets and columns.
#' @param columns Columns within the dataset. When NULL (the default), returns all columns. Run \code{\link{list_datasets_for_covariate}} to see datasets and bands.
#'
#' @export
attach_covariate_data <- function(se, covariate, dataset = NULL, columns = NULL, date_col = "sample_date") {
  covariate_id <- get_covariate_id(covariate)

  # Get covariate items
  items <- get_collection_items(covariate_id)
  items

  # Check inputs -- returns the dataset type, its bands/columns, and URL
  asset_info <- check_inputs_covariate_data(items, covariate_id, dataset, columns, date_col)

  # Do spatial join with DuckDB, select relevant columns (all, if columns = NULL)
  se %>%
    join_se_to_parquet(asset_info[["url"]], columns)
}

get_collection_items <- function(x, simplify = TRUE) {
  x <- get_covariate_id(x)

  items <- rstac::stac(stac_url) %>%
    rstac::collections(collection_id = x) %>%
    rstac::items() %>%
    rstac::get_request()

  if (simplify) {
    items[["features"]]
  } else {
    items
  }
}

check_inputs_covariate_data <- function(items, covariate, dataset = NULL, col = NULL, date_col = "sample_date") {
  # Check the covariate contains parquet data
  parquet_assets <- get_parquet_assets(items[[1]])

  # If there are NO parquet assets, they may need to use get_zonal_statistics() -- message with that instead
  if (identical(parquet_assets, NA_character_)) {
    cog_assets <- get_cog_assets(items[[1]])
    if (!identical(cog_assets, NA_character_)) {
      usethis::ui_stop("You cannot attach this covariate. Use `get_zonal_statistics()` instead.")
    }
  }

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

  if (!is.null(dataset)) {
    # Check that they only specified one dataset
    if (length(dataset) > 1) {
      usethis::ui_stop("You may only specify one dataset.")
    }

    # Check that specified asset exists
    asset <- assets[[dataset]]
    if (is.null(asset)) {
      usethis::ui_stop(
        "Dataset \"{dataset}\" does not exist. Valid datasets are: {comma_sep_quoted(names(asset_types))}."
      )
    }
  }

  if (length(asset_types) > 1 & is.null(dataset)) {
    usethis::ui_stop("Covariate \"{covariate}\" contains more than one dataset. Please specify which to use in `dataset`.
      Options: {comma_sep_quoted(names(asset_types))}.")
  }

  if (length(asset_types) == 1) {
    dataset <- names(asset_types)
  }

  asset <- assets[[dataset]]
  asset_type <- asset_types[[dataset]]

  # If there is more than one col (band or column) in the specified asset, they need to specify
  # Remove -- this is not needed for now, attaching all of the data
  bands_cols <- get_asset_bands_or_columns(asset)
  #
  # if (nrow(bands_cols) > 1 & is.null(col)) {
  #   if (asset_types[[dataset]] == "parquet") {
  #     cols <- bands_cols[["name"]] %>% paste0(collapse = '", "')
  #     cols <- glue::glue('"{cols}"')
  #
  #     usethis::ui_stop(
  #       "Dataset \"{dataset}\" contains more than one column of data. Please specify which to use in `col`.
  #     Options: {cols}."
  #     )
  #   } else {
  #     tibble_string <- paste(capture.output(print(bands_cols)), collapse = "\n")
  #     usethis::ui_stop(
  #       "Dataset \"{dataset}\" contains more than one band of data. Please specify which to use in `col`. You may specify by band number or by name.\nOptions: \n{tibble_string}"
  #     )
  #   }
  # }

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
        "Band \"{col}\" is not a valid band.\nOptions (You may specify by band number OR by name): \n{tibble_string}"
      )
    }
  } else if (asset_type == "parquet" & !is.null(col)) {
    valid_col <- col %in% bands_cols[["name"]]

    if (!valid_col) {
      usethis::ui_stop(
        "Column \"{col}\" is not valid. \nOptions: \n{comma_sep_quoted(bands_cols[['name']])}"
      )
    }
  }

  # If all inputs pass, return info on the dataset's type, URL, and its bands/cols
  res <- list(
    type = asset_type,
    url = asset[["href"]],
    bands_cols = bands_cols
  )

  # If it is a cog, attach the band NUMBER too
  if (asset_type == "cog") {
    res <- append(res, list(band = band))
  }

  return(res)
}

join_se_to_parquet <- function(se, url, columns) {
  # Set up connection, create a table called "temp" with the data from the parquet file
  conn <- create_parquet_table(url)

  # Do not need to check that columns are in the data, because done upstream in attach_covariate_data()

  # TODO -> check that they share a CRS?

  se <- se %>%
    # Adding an internal ID, just to deal with any duplication issues
    dplyr::mutate(...duckdb_id = dplyr::row_number())

  # Convert the SEs to sf
  se_sf <- se %>%
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

  # Write to connection
  duckspatial::ddbs_write_table(conn, se_sf, "points", quiet = TRUE)

  res <- duckspatial::ddbs_join(
    conn  = conn,
    x     = "points", # se
    y     = "temp", # parquet
    join  = "intersects"
  )

  # If relevant, select specific columns
  if (!is.null(columns)) {
    res <- res %>%
      dplyr::select(dplyr::all_of(c(names(se), columns)))
  }

  res <- res %>%
    duckspatial::ddbs_collect() %>%
    sf::st_drop_geometry() %>%
    dplyr::select(-dplyr::any_of("bbox")) # If bbox is present, remove it

  # Disconnect db connection
  DBI::dbDisconnect(conn)

  # If there is no data returned (i.e., nothing joined), then the df is 0 rows
  # Need to return the SEs still, even if there is no data
  # This applies if there is no data for SOME of them, not just ALL of them
  res <- se %>%
    dplyr::left_join(res, by = names(se)) %>%
    dplyr::select(-...duckdb_id)

  res
}

create_parquet_table <- function(url) {
  # Create connection, enable reading remote parquet
  conn <- duckspatial::ddbs_create_conn()
  DBI::dbExecute(conn, "INSTALL httpfs; LOAD httpfs;")
  DBI::dbExecute(conn, "SET enable_progress_bar = false;") # Don't show progress bar

  exec_command <- glue::glue("
  CREATE OR REPLACE VIEW temp AS
  SELECT * FROM read_parquet('{url}')")

  exec_command <- as.character(exec_command)

  DBI::dbExecute(conn, exec_command)

  conn
}
