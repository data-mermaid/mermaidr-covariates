#' List available covariates
#'
#' @export
#'
#' @examples
#' list_covariates()
list_covariates <- function() {
  # Get collections
  res <- rstac::stac(stac_url) |>
    rstac::collections() |>
    rstac::get_request()

  # Just keep "collections" entry
  res <- res[["collections"]]

  # Name with ID
  names(res) <- sapply(res, \(x) {
    x[["id"]]
  })

  # Give it "collections" class
  # TODO

  res
}
