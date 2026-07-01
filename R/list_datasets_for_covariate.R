#' List datasets and bands or columns for a covariate
#'
#' @param covariate Covariate
#'
#' @export
list_datasets_for_covariate <- function(covariate) {
  # Use this name, because list_covariate_datasets() is too easily confused with list_covariates()
  # Covert covariate title to ID, then run get_colelction_bands_and_columns()

  covariate_id <- get_covariate_id(covariate)

  get_collection_bands_and_columns(covariate_id)
}

get_collection_bands_and_columns <- function(collection) {
  # Look through all assets -- determine if each is cog or parquet, then get either bands or columns

  collection <- get_covariate_id(collection)

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
