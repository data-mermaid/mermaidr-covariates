before_date_to_datetime <- function(date) {
  glue::glue("../{date}T00:00:00Z")
}

after_date_to_datetime <- function(date) {
  glue::glue("{date}T00:00:00Z/..")
}

start_end_to_interval <- function(start_date, end_date) {
  glue::glue("{start_date}T00:00:00Z/{end_date}T00:00:00Z")
}

get_covariate_name_from_id <- function(id) {
  rstac::stac(stac_url) %>%
    rstac::collections(id) %>%
    rstac::get_request() %>%
    purrr::pluck("title")
}

add_id_for_iteration <- function(df) {
  # Allow for the possibility that they have more than one record at each site at each date
  # Make distinct for them, but also handle the possibility of different latitude/longitude
  # So best to just distinguish entirely, using row number
  df %>%
    dplyr::distinct(site, latitude, longitude, sample_date) %>%
    dplyr::mutate(...id = glue::glue("{site}_{sample_date}")) %>%
    dplyr::group_by(...id) %>%
    dplyr::mutate(row = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(...id = glue::glue("{...id}_{row}")) %>%
    dplyr::select(-row)
}
