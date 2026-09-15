#' Get covariate type
#'
#' Tells you covariate type (raster, vector, raster and vectors, or unknown)
#'
#' @param covariate Covariate name or ID
#'
#' @export
#'
#' @examples
#' # get_covariate_type("Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)")
#' # get_covariate_type("GPW MEOW Realms")
get_covariate_type <- function(covariate) {
  # Look at the first item
  item <- rstac::stac(stac_url) %>%
    rstac::collections(covariate) %>%
    rstac::items(limit = 1) %>%
    rstac::get_request()

  item <- item[["features"]][[1]]

  # Look for COG assets and parquet assets
  cog_assets <- get_cog_assets(item)
  parquet_assets <- get_parquet_assets(item)

  is_raster <- !identical(cog_assets, NA_character_)
  is_vector <- !identical(parquet_assets, NA_character_)

  if (is_raster & is_vector) {
    "raster and vector"
  } else if (is_raster) {
    "raster"
  } else if (is_vector) {
    "vector"
  } else {
    "unknown"
  }
}
