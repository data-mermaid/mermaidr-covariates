library(dplyr)
library(lubridate)

se <- readRDS(here::here("inst/extdata/test_sse.rds"))

distinct_se <- se %>%
  distinct(site, latitude, longitude, sample_date)

vary_chunk_threshold <- function(chunk_threshold = 50) {
  distinct_se %>%
    head(500) %>%
    get_summary_zonal_statistics("Daily Sea Surface Temperature",
      n_days = 10,
      radius = 1000,
      spatial_stats = "mean",
      temporal_stats = "max",
      chunk_threshold = chunk_threshold
    )
}

vary_chunk_threshold(1000)

bench::mark(
  vary_chunk_threshold(25),
  vary_chunk_threshold(50),
  vary_chunk_threshold(100),
  vary_chunk_threshold(200),
  vary_chunk_threshold(500),
  vary_chunk_threshold(1000)
)


# Multiple sites with same lat/long is negligible -- don't worry too much about that
se %>%
  group_by(latitude, longitude) %>%
  summarise(
    n_sites = n_distinct(site),
    .groups = "drop"
  ) %>%
  filter(n_sites > 1) %>%
  count(n_sites)


# Checking how to overlap timeframes, so not requesting same item multiple times

se %>%
  distinct(site, latitude, longitude, sample_date) %>%
  mutate(first_date = sample_date - days(10)) %>%
  arrange(site, latitude, longitude, sample_date) %>%
  group_by(site, latitude, longitude) %>%
  filter(n() > 1) %>%
  arrange(sample_date) %>%
  mutate(prev_date = lag(sample_date)) %>%
  filter(!is.na(prev_date)) %>%
  mutate(prev_date_within_sample_period = prev_date >= first_date) %>%
  filter(prev_date_within_sample_period)

# This seems complicated...

# Is getting the items the slow part?
# Not really at all

# So just reduce hitting the zonal stats API for the same item multiple times
vary_chunk_and_dedupe <- function(dedupe_items, chunk_threshold = 500, n_se = 500) {
    se %>% filter(site == "A: Iniban Marine Sanctuary") %>%
    get_summary_zonal_statistics("Daily Sea Surface Temperature",
      n_days = 365,
      radius = 1000,
      spatial_stats = "mean",
      temporal_stats = "max",
      chunk_threshold = chunk_threshold,
      dedupe_items
    )
}

bench::mark(
    vary_chunk_and_dedupe(TRUE),
    vary_chunk_and_dedupe(FALSE)
)
