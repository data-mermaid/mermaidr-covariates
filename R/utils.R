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

  # If n_days = NULL, then just use the single sample date, not an interval
  if (is.null(n_days)) {
    df <- df %>%
      dplyr::mutate(...id = glue::glue("{latitude}_{longitude}_{...date_temp}"))

    return(df)
  }

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

split_for_chunking <- function(se, covariate_interval, n_days) {
  split_by_chunk <- covariate_interval %in% c("once", "periodic")
  if (!split_by_chunk) {
    split_by_chunk <- (covariate_interval == "daily" & n_days < chunk_size)
  } else {
    n_days <- 1 # Need for setting SEs by chunk if it is just once
  }

  # Potentially split not by ...id, if n_days is small
  if (split_by_chunk) {
    se_per_chunk <- ceiling(chunk_size / n_days)

    se %>%
      dplyr::mutate(...chunk = (dplyr::row_number() - 1) %/% se_per_chunk) %>%
      split(.$...chunk)
  } else {
    se %>%
      split(.$...id)
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
    stop(x, " is not a valid covariate title or ID. Run `list_covariates()` to see covariates.", call. = FALSE)
  }

  covariate_id
}

get_covariate_interval <- function(covariate) {
  # Determine whether a covariate is:
  # daily
  # periodic (e.g. every year, every 5 years, etc)
  # once only

  # by looking at its start date, adding one year, then seeing how many items are returned
  # This does NOT work, because it doesn't tell us if the data is annual, every 5 years, etc
  # Maybe add two years, then deal with ~ 600 instead

  items <- get_collection_items(covariate, simplify = FALSE)

  n_items <- items[["numberMatched"]]

  dplyr::case_when(
    n_items == 1 ~ "once",
    n_items > 1 & n_items < 50 ~ "periodic",
    n_items >= 50 & n_items <= 300 ~ "check",
    n_items > 300 ~ "daily"
  )
}

comma_sep_quoted <- function(x) {
  x <- paste0(x, collapse = "\", \"")
  glue::glue('"{x}"')
}
