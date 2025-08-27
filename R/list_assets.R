list_assets <- function(item, limit = NULL) {
  # Check for "item" class
  # TODO

  # Just extract "assets" entry
  assets <- item[["assets"]]

  if (!is.null(limit) & !is.null(assets)) {
    if (limit > length(assets)) {
      limit <- length(assets)
    }
    assets[1:limit]
  } else {
    assets
  }
}
