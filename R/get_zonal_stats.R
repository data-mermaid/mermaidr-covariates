get_zonal_stats <- function(longitude, latitude, url, buffer = 1000, bands = list(1, 2, 3), approx_stats = FALSE,
                            stats = c(
                              "min", "max", "mean", "count", "sum", "std", "median", "majority", "minority", "unique", "range", "nodata", "area", "freq_hist"
                            )) {
  res <- httr2::request(zonal_stats_url) %>%
    httr2::req_body_json(list(
      aoi = list(
        type = "Point", coordinates = c(longitude, latitude),
        buffer_size = buffer
      ),
      image = list(
        url = url,
        # bands = list(1), # TODO, not working -> only works with band: 1
        approx_stats = approx_stats
      ),
      stats = as.list(stats)
    )) %>%
    httr2::req_perform()

  res_tbl <- res %>%
    httr2::resp_body_json() %>%
    purrr::map_dfr(\(x) {
      x <- purrr::map(x, \(x) if (is.null(x)) NA else x)
      dplyr::as_tibble(x)
    }, .id = "band")

  dplyr::bind_cols(
    dplyr::tibble(longitude = longitude, latitude = latitude),
    res_tbl
  )
}
