select_item <- function(collection, item_id) {
  # Check for "collection" class
  # TODO

  # Extract the ID
  collection_id <- collection[["id"]]

  # Get the specific item
  res <- rstac::stac(stac_url) |>
    rstac::collections(collection_id = collection_id) |>
    rstac::items(item_id) |>
    rstac::get_request()

  res
}
