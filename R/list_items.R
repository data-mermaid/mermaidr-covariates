list_items <- function(collection, limit = NULL) {
  # Check for "collection" class
  # TODO

  # Extract the ID
  collection_id <- collection[["id"]]

  # Query collection's items
  res <- rstac::stac(stac_url) |>
    rstac::collections(collection_id = collection_id) |>
    rstac::items(limit = limit) |>
    rstac::get_request()

  # Just keep "features" entry
  res <- res[["features"]]

  # Name with ID
  names(res) <- sapply(res, \(x) {x[["id"]]})

  res
}

list_all_items <- function(collection) {
    list_items(collection, limit = 9999999)
}

doc_items <- function(x, base_url = NULL, query = NULL) {
  if (!is.list(x) || !"type" %in% names(x))
      x$type <- "FeatureCollection"
  if (x$type != "FeatureCollection")
    .error("Invalid Items object. Type '%s' is not supported.", x$type)
  if (!"features" %in% names(x))
    .error("Invalid Items object. Expecting 'features' key.")
  x$features <- lapply(x$features, rstac:::doc_item)
  if ("links" %in% names(x))
    x$links <- rstac:::doc_links(x$links, base_url = base_url)
  items <- rstac:::rstac_doc(x, subclass = c("doc_items", "rstac_doc"))
  attr(items, "query") <- query
  items
}

assignInNamespace("doc_items", doc_items, ns = "rstac")
