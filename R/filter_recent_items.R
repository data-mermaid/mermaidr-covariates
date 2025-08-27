filter_recent_items <- function(collection, date, n_months) {
  # Search for items in a collection

  # If it is a collection, extract the ID

  # Otherwise, just use the ID
  collection_id <- collection

  # Construct time -> expect that date is in UTC? Should check what format they come down in MERMAID, and use that as baseline
  # In sampleevents it is just a DATE, so convert to a datetime
  last_date <- lubridate::as_datetime(date)
  first_date <-  lubridate::`%m-%`(last_date, months(n_months))
  last_date <- format(last_date, "%Y-%m-%dT%H:%M:%SZ")
  first_date <- format(first_date, "%Y-%m-%dT%H:%M:%SZ")
  # Add one day, so it INCLUDES any samples from this day

  # Should it be months to the date, or by calendar month?
  # E.g. if 12 months prior to 2025-07-02, what about one on 2024-07-01?

  # First date is date - n_months
  # Last date is date

  browser()

  rstac::stac(stac_url) |>
    rstac::stac_search(
      collections = collection_id,
      datetime = glue::glue("../{last_date}"),
      # TODO, not working in API
      # datetime = glue::glue("{first_date}/{last_date}")
    ) |>
    rstac::post_request()
}
