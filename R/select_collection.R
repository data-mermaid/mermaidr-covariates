select_collection <- function(collection_id) {
  rstac::stac(stac_url) |>
    rstac::collections(collection_id = collection_id) |>
    rstac::get_request()
}
