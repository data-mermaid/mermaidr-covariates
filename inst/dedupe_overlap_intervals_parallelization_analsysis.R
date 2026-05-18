library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
devtools::load_all()

ses <- readRDS(here::here("inst/extdata/test_sse.rds"))

# Testing cases where there are multiple sample events within a year
# To see how de-duplicating fetches sent to the zonal stats API performs

# Get a sample of sites with multiple surveys within X days

n_days <- 365

# ses_with_se_within_n_days <- ses %>%
#   distinct(site, latitude, longitude, sample_date) %>%
#   add_count(site, latitude, longitude) %>%
#   filter(n > 1) %>%
#   arrange(site, latitude, longitude, sample_date) %>%
#   group_by(site, latitude, longitude) %>%
#   mutate(
#     next_sample_date = lead(sample_date),
#     n_days_diff = next_sample_date - sample_date,
#     next_sample_date_within_days = n_days_diff <= n_days,
#   ) %>%
#   ungroup() %>%
#   filter(next_sample_date_within_days)

ses_overlap <- ses %>%
  group_by(latitude, longitude) %>%
  arrange(sample_date) %>%
  mutate(
    prev_date = lag(sample_date),
    diff = sample_date - prev_date,
    over_n_days = diff > n_days,
    start_new = coalesce(over_n_days, TRUE), # So that the first row is filled in
    group = case_when(start_new ~ sample_date)
  ) %>%
  fill(group, .direction = "down") %>%
  group_by(latitude, longitude, group) %>%
  mutate(
    ...start_date = min(sample_date),
    ...end_date = max(sample_date)
  ) %>%
  ungroup() %>%
    mutate(...id = glue::glue("{latitude}_{longitude}_{...start_date}_{...end_date}"))

calls_dedupe <- ses_overlap %>%
  distinct(latitude, longitude, ...start_date, ...end_date) %>%
  mutate(
    ...interval_start = ...start_date - days(n_days),
    total_days = ...end_date - ...interval_start + 1 # +1 to account for including sample date
  ) %>%
  pull(total_days) %>%
  sum()

calls_orig <- ses %>%
  distinct(latitude, longitude, sample_date) %>%
  mutate(
    ...interval_start = sample_date - days(n_days),
    total_days = sample_date - ...interval_start + 1 # +1 to account for including sample date
  ) %>%
  pull(total_days) %>%
  sum()

as.numeric(calls_orig) - as.numeric(calls_dedupe)

# ~850,000
