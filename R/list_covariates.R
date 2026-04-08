#' List available covariates
#'
#' @param as_data_frame Whether to return the covariates as a data frame. Defaults to \code{TRUE}.
#'
#' @export
#'
#' @examples
#' # list_covariates()
list_covariates <- function(as_data_frame = TRUE) {
  # Get collections
  res <- rstac::stac(stac_url) |>
    rstac::collections() |>
    rstac::get_request()

  # Just keep "collections" entry
  res <- res[["collections"]]

  # Replace any NULLs with NAs

  # Reformat and remove some entries
  res <- purrr::map(
    res,
    \(x) {
      x$type <- NULL
      x$links <- NULL # TODO
      x$stac_version <- NULL
      x$stac_extensions <- NULL
      x$summaries <- NULL

      if (length(x$keywords) == 0) {
        x$keywords <- NA_character_
      } else {
        x$keywords <- paste0(sort(x$keywords), collapse = ", ")
      }

      x$providers <- x$providers %>%
        purrr::map_dfr(dplyr::as_tibble) %>%
        list()

      x$bbox <- x$extent$spatial$bbox
      temporal_interval <- x$extent$temporal$interval[[1]]

      if (!is.null(temporal_interval)) {
        temporal_interval <- as.Date(temporal_interval)
        x$start_date <- temporal_interval[1]
        x$end_date <- temporal_interval[2]
      }

      x$extent <- NULL

      x
    }
  )

  # If as_data_frame = TRUE (the default), reformat
  if (as_data_frame) {
    reshape_covariates_df(res)
  } else {
    # Otherwise, just return it as a list
    add_covariates_list_classes(res)
  }
}

# Overwrite rstac's existing print method for collections, to be more relevant for us

#' @export
print.covariate <- function(x) {
  cat("# Covariate:", x$title, fill = TRUE)

  dates <- ifelse(is.na(x$end_date),
    paste0(x$start_date, " - "),
    paste0(x$start_date, " - ", x$end_date)
  )

  cat("-", "Dates:", dates, fill = TRUE)

  if (!is.na(x$keywords)) {
    cat("-", "Keywords:", x$keywords, fill = TRUE)
  }

  providers <- x$providers[[1]] %>%
    dplyr::mutate(text = glue::glue("{name} ({roles})")) %>%
    dplyr::pull(text) %>%
    sort() %>%
    paste(collapse = ", ")

  if (!is.na(providers)) {
    cat("-", "Providers:", providers, fill = TRUE)
  }


  cat("-", "bbox:", paste0(x$bbox[[1]], collapse = ", "), fill = TRUE)

  cat("-", "id:", x$id, fill = TRUE)

  cat("-", "Description:", x$description, fill = TRUE)

  invisible(x)
}

reshape_covariates_df <- function(covariates) {
  covariates %>%
    purrr::map_dfr(\(x) {
      x %>%
        # purrr::compact() %>% # In case of any empty entries
        dplyr::as_tibble()
    }) %>%
    dplyr::select(id, title, description, start_date, end_date, dplyr::everything())
}

add_covariates_list_classes <- function(covariates) {
  covariates <- purrr::map(covariates, \(x) {
    structure(x, class = c("list", "covariate"))
  })
  names(covariates) <- covariates %>% purrr::map_chr("title")
  covariates
}
