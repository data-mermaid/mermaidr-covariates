before_date_to_datetime <- function(date) {
  glue::glue("../{date}T00:00:00Z")
}

after_date_to_datetime <- function(date) {
  glue::glue("{date}T00:00:00Z/..")
}

start_end_to_interval <- function(start_date, end_date) {
  glue::glue("{start_date}T00:00:00Z/{end_date}T23:59:59Z")
}

get_covariate_name_from_id <- function(id) {
  rstac::stac(stac_url) %>%
    rstac::collections(id) %>%
    rstac::get_request() %>%
    purrr::pluck("title")
}

add_id_for_iteration <- function(df, strip_cols, date_col) {
  # Allow for the possibility that they have more than one record at each site at each date
  # Make distinct for them, but also handle the possibility of different latitude/longitude
  # So best to just distinguish entirely, using row number
  id <- df %>%
    dplyr::mutate(...date_temp = !!rlang::sym(date_col)) %>%
    dplyr::distinct(site, latitude, longitude, ...date_temp) %>%
    dplyr::arrange(site, ...date_temp, latitude, longitude) %>%
    dplyr::mutate(...id = glue::glue("{site}_{...date_temp}")) %>%
    dplyr::group_by(...id) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(...id = glue::glue("{...id}_{row}")) %>%
    dplyr::select(-row)

  id <- id %>%
    dplyr::rename_with(\(x) date_col, ...date_temp)

  if (strip_cols) {
    id
  } else {
    df %>%
      dplyr::left_join(id, by = c("site", "latitude", "longitude", date_col))
  }
}

lookup_collection <- function(x) {
  rstac::stac(stac_url) %>%
    rstac::collections(collection_id = x) %>%
    rstac::get_request()
}

safely_lookup_collection <- purrr::safely(lookup_collection)

check_covariate_id <- function(x) {
  # Checks if `x` is a covariate ID (collection ID)
  res <- safely_lookup_collection(x)

  is.null(res$error)
}

lookup_covariate_id_by_name <- function(name) {
  covariate <- list_covariates() %>%
    dplyr::filter(title == name)

  if (nrow(covariate) == 0) {
    return(NA)
  }

  covariate[["id"]]
}

get_covariate_id <- function(x) {
  # If x is an ID, just return it
  is_covariate_id <- check_covariate_id(x)

  if (is_covariate_id) {
    return(x)
  }

  # Otherwise, assume it is a name and look up the ID
  covariate_id <- lookup_covariate_id_by_name(x)

  # If no ID, error
  if (is.na(covariate_id)) {
    stop(x, " is not a valid covariate title or ID.", call. = FALSE)
  }

  covariate_id
}

determine_covariate_interval <- function(covariate_id) {
  # Determine whether a covariate is:
  # daily
  # monthly
  # annually
  # once only

  # by looking at its start date, adding one year, then seeing how many items are returned

  covariates <- list_covariates()

  start_date <- covariates %>%
    dplyr::filter(id == covariate_id) %>%
    pull(start_date)

  items <- rstac::stac(stac_url) %>%
    rstac::stac_search(
      collections = covariate_id,
      datetime = start_end_to_interval(start_date, start_date + lubridate::years(1))
    ) %>%
    rstac::get_request()

  n_items <- items$numberMatched

  dplyr::case_when(
    n_items < 2 ~ "annual/once",
    n_items < 15 ~ "monthly",
    n_items >= 15 & n_items <= 300 ~ "check",
    n_items > 300 ~ "daily"
  )
}
