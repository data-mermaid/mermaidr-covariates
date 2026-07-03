GET_zonal_stats <- function(stac_items, radius = 1000, bands = list(1),
                            approx_stats = FALSE, spatial_stats = "mean",
                            type = "raster") {
  endpoint_url <- switch(type,
    "raster" = zonal_stats_raster_url,
    "vector" = zonal_stats_vector_url
  )

  # Set up requests to parallelize
  request_base <- httr2::request(endpoint_url) %>%
    httr2::req_throttle(capacity = chunk_size, fill_time_s = 3) %>%
    httr2::req_user_agent("mermaidr-covariates") %>%
    httr2::req_body_json(list(
      aoi = NULL,
      url = NULL,
      # columns = NULL,
      # bands = NULL,
      # radius = radius
      stats = as.list(spatial_stats)
      # approx_stats = approx_stats
    )) %>%
    httr2::req_error(is_error = \(res) FALSE)

  if (type == "raster") {
    request_base <- request_base %>%
      httr2::req_body_json_modify(
        bands = bands
      )
  } else if (type == "vector") {
    request_base <- request_base %>%
      httr2::req_body_json_modify(
        columns = bands
      )
  }

  stac_items_count <- stac_items %>%
    dplyr::add_count(...secondary_id) %>%
    dplyr::filter(n > 1)

  if (nrow(stac_items_count) > 0) {
    usethis::ui_stop("Unexpected error, please report: no STAC items for sample events")
  }

  stac_items_list <- stac_items %>%
    split(.[["...secondary_id"]])

  requests <- purrr::map(
    stac_items_list,
    \(x) {
      request_base %>%
        httr2::req_body_json_modify(
          url = x[["url"]],
          aoi = list(
            type = "Point",
            coordinates = c(x[["longitude"]], x[["latitude"]])
            # radius = radius
          )
        )
    }
  )

  res <- httr2::req_perform_parallel(requests, progress = FALSE)

  names(res) <- names(stac_items_list)

  # Format the results of each call -- if not a 200, then just remove the result. will complete band/stat/etc later
  res <- res %>%
    purrr::imap(
      \(res, date) {
        if (res[["status_code"]] != 200) {
          return(dplyr::tibble(band = NA))
        }
        res %>%
          httr2::resp_body_json() %>%
          purrr::map_dfr(\(x) {
            x <- purrr::map(x, \(x) if (is.null(x)) NA else x)
            dplyr::as_tibble(x)
          }, .id = "band")
      }
    ) %>%
    purrr::list_rbind(names_to = "...secondary_id")

  # Add ...id back on
  stac_items %>%
    dplyr::left_join(res, by = "...secondary_id") %>%
    # If there is not any data because the API did NOT return anything (NOT that it returned an NA), set date to NA
    dplyr::mutate(
      date = ifelse(is.na(band), NA, date),
      date = as.Date(date)
    )
}
