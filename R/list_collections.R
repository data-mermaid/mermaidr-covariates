list_collections <- function(limit = NULL) {

  # Get collections
  res <- rstac::stac(stac_url) |>
    rstac::collections(limit = limit) |>
    rstac::get_request()

  # Just keep "collections" entry
  res <- res[["collections"]]

  # Name with ID
  names(res) <- sapply(res, \(x) {x[["id"]]})

  # Give it "collections" class
  # TODO

  res
}
