library(rstac)
library(purrr)
library(dplyr)

ses <- readRDS(here::here("inst/extdata/test_sse.rds"))

ses <- ses %>%
  distinct(site, latitude, longitude, sample_date)

sample_ses <- ses %>%
  sample_n(20)

sample_ses <- sample_ses %>%
  add_id_for_iteration() %>%
  split(.$...id)

covariate_id <- "50b810fb-5f17-4cdb-b34b-c377837e2a29"

get_items <- function(ses, prepare_only = FALSE) {
  map(
    ses,
    \(x) get_items_for_zonal_stats(x, covariate_id,
      n_days = 365, prepare_only = prepare_only
    )
  )
}

get_items_vary_parallel <- function(ses, parallel = TRUE, capacity = 2) {
  query_info <- ses %>%
    get_items(prepare_only = TRUE)

  requests <- purrr::map(
    query_info,
    \(x) {
      url <- "https://mermaid.prescient.earth/stac/search"
      query_url <- httr2::url_modify(url,
        query = x[["query"]]
      )
      request <- httr2::request(query_url) %>%
        httr2::req_throttle(capacity = capacity, fill_time_s = 30) %>%
        httr2::req_user_agent("mermaidr-covariates") %>%
        httr2::req_error(is_error = \(res) FALSE)

      request
    }
  )

  if (parallel) {
    res <- httr2::req_perform_parallel(requests, progress = FALSE)
  } else {
    res <- purrr::map(requests, httr2::req_perform)
  }

  x <- purrr::map(
    res,
    \(x) {
      x <- x %>%
        httr2::resp_body_json() %>%
        purrr::pluck("features")

      purrr::map_df(
        x,
        \(y) {
          tibble(
            id = y$id,
            date = as.Date(y$properties$datetime)
          )
        }
      )
    }
  )

  names(x) <- names(ses)

  x
}

bench::mark(
  get_items_vary_parallel(sample_ses[1:10], FALSE),
  get_items_vary_parallel(sample_ses[1:10], TRUE)
)
1
