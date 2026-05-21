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

add_id_for_iteration <- function(df, date_col, n_days) {
  df <- df %>%
    dplyr::mutate(
      ...date_temp = !!rlang::sym(date_col),
      ...date_temp = as.Date(...date_temp)
    )

  # Deduplicate overlapping API calls
  # e.g. if they have two samples within a year (or whatever n_days is),
  # many of the items will be the same
  # So determine which overlap, then iterate over those
  # Rather than just the site and sample date

  df %>%
    dplyr::group_by(latitude, longitude) %>% # For "site", actually just use lat/long
    dplyr::arrange(...date_temp) %>%
    dplyr::mutate(
      ...prev_date = dplyr::lag(...date_temp),
      ...diff = ...date_temp - ...prev_date,
      ...over_n_days = ...diff > n_days,
      ...start_new = dplyr::coalesce(...over_n_days, TRUE), # So that the first row is filled in
      ...group = dplyr::case_when(...start_new ~ ...date_temp)
    ) %>%
    tidyr::fill(...group, .direction = "down") %>%
    dplyr::group_by(latitude, longitude, ...group) %>%
    dplyr::mutate(
      ...start_date = min(...date_temp),
      ...end_date = max(...date_temp)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(...id = glue::glue("{latitude}_{longitude}_{...start_date}_{...end_date}")) %>%
    dplyr::select(dplyr::all_of(names(df)), ...start_date, ...end_date, ...id) %>%
    dplyr::select(-...date_temp)
}

add_id_for_joining <- function(df, date_col) {
  df %>%
    dplyr::mutate(
      ...date_temp = !!rlang::sym(date_col),
      ...date_temp = as.Date(...date_temp),
      ...join_id = glue::glue("{project_id}_{site}_{latitude}_{longitude}_{...date_temp}_{dplyr::row_number()}")
    ) %>%
    dplyr::select(-...date_temp)
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
