GET_zonal_stats <- function(stac_items, id_col, radius = 1000, bands = list(1),
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

  stac_items <- stac_items %>%
    split(.[[id_col]])

  requests <- purrr::map(
    stac_items,
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

  names(res) <- names(stac_items)

  if (length(bands) > 1) {
    browser()
  }

  # Format the results of each call
  res %>%
    purrr::imap(
      \(res, date) {
        # If not a 200, return empty df
        if (res[["status_code"]] != 200) {
          res <- vector("list", length = length(bands))
          res <- purrr::map(res,
                     \(x) {
                       x <- vector("list", length = length(spatial_stats))
                       x <- purrr::map(x, \(x) NA)
                       names(x) <- spatial_stats

                       dplyr::as_tibble(x)
                     })

          names(res) <- paste0("band_", unlist(bands))

          res <- res %>%
            dplyr::bind_rows(.id = "band")

          return(res)
        }

        res %>%
          httr2::resp_body_json() %>%
          purrr::map_dfr(\(x) {
            x <- purrr::map(x, \(x) if (is.null(x)) NA else x)
            dplyr::as_tibble(x)
          }, .id = "band")
      }
    ) %>%
    purrr::list_rbind(names_to = id_col) %>%
    # If any blanks, fill the band col -- if > 1 col, the browser above will catch it, and we will see
    tidyr::fill(band, .direction = "updown") %>%
    dplyr::left_join(stac_items %>% dplyr::bind_rows(),
      by = id_col
    ) %>%
    dplyr::select(-dplyr::all_of(id_col))
}
