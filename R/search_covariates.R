#' Search available covariates
#'
#' @param title Search term within title. Case insensitive.
#' @param description Search term within description. Case insensitive.
#' @param as_data_frame Whether to return the covariates as a data frame. Defaults to \code{TRUE}.
#'
#' @export
#'
#' @examples
#' # search_covariates("MEOW")
search_covariates <- function(title, description, as_data_frame = TRUE) {
  # Get all covariates -- as data frame, for easier filtering, then convert to list if required
  covariates <- list_covariates(as_data_frame = TRUE)

  if (missing(title) & missing(description)) {
    if (!as_data_frame) {
      covariates <- reshape_covariates_list(covariates)
    }
    return(covariates)
  }

  if (!missing(title)) {
    covariates <- covariates %>%
      dplyr::filter(stringr::str_detect(tolower(title), tolower(!!title)))
  }

  if (!missing(description)) {
    covariates <- covariates %>%
      dplyr::filter(stringr::str_detect(tolower(description), tolower(!!description)))
  }

  if (!as_data_frame) {
    if (!as_data_frame) {
      covariates <- reshape_covariates_list(covariates)
    }
  }

  covariates
}

reshape_covariates_list <- function(covariates) {
  covariates %>%
    split(.$id) %>%
    purrr::map(as.list) %>%
    add_covariates_list_classes()
}
