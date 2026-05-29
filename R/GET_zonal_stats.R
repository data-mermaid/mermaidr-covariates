GET_zonal_stats <- function(stac_items, id_col, radius = 1000, bands = list(1),
                            approx_stats = FALSE, spatial_stats = "mean", columns = NULL) {
  # Set up requests to parallelize
  request_base <- httr2::request(zonal_stats_raster_url) %>%
    httr2::req_throttle(capacity = chunk_size, fill_time_s = 3) %>%
    httr2::req_user_agent("mermaidr-covariates") %>%
    httr2::req_body_json(list(
      aoi = NULL,
      url = NULL,
      columns = NULL,
      stats = as.list(spatial_stats),
      bands = bands,
      approx_stats = approx_stats
    )) %>%
    httr2::req_error(is_error = \(res) FALSE)

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
            coordinates = c(x[["longitude"]], x[["latitude"]]),
            radius = radius
          ),
        )
    }
  )

  res <- httr2::req_perform_parallel(requests, progress = FALSE)

  names(res) <- names(stac_items)

  # Format the results of each call
  res %>%
    purrr::keep(\(x) x$status_code == 200) %>%
    purrr::imap(
      \(res, date) {
        res %>%
          httr2::resp_body_json() %>%
          purrr::map_dfr(\(x) {
            x <- purrr::map(x, \(x) if (is.null(x)) NA else x)
            dplyr::as_tibble(x)
          }, .id = "band")
      }
    ) %>%
    purrr::list_rbind(names_to = id_col) %>%
    dplyr::left_join(stac_items %>% dplyr::bind_rows(),
      by = id_col,
      relationship = "one-to-one"
    ) %>%
    dplyr::select(-dplyr::all_of(id_col))
}
