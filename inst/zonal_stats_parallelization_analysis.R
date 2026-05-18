library(dplyr)
library(lubridate)
library(purrr)
devtools::load_all()

ses <- readRDS(here::here("inst/extdata/test_sse.rds"))

# Testing cases where there are multiple sample events within a year
# To see how de-duplicating fetches sent to the zonal stats API performs

# Get a sample of sites with multiple surveys within X days

n_days <- 365

set.seed(1234)

sample_ses <- ses %>%
  distinct(site, latitude, longitude, sample_date) %>%
  add_count(site, latitude, longitude) %>%
  filter(n > 1) %>%
  arrange(site, latitude, longitude, sample_date) %>%
  group_by(site, latitude, longitude) %>%
  mutate(
    next_sample_date = lead(sample_date),
    next_sample_date_within_year = next_sample_date <= sample_date + days(n_days)
  ) %>%
  ungroup() %>%
  filter(next_sample_date_within_year) %>%
  distinct(site, latitude, longitude) %>%
  sample_n(5) %>%
  left_join(ses, by = c("site", "latitude", "longitude")) %>%
  add_id_for_iteration()

sample_ses_list <- sample_ses %>%
  split(.$...id)

covariate_id <- "50b810fb-5f17-4cdb-b34b-c377837e2a29"

ses_items <- map_dfr(sample_ses_list,
  \(x) get_items_for_zonal_stats(x, covariate_id, n_days = n_days),
  .id = "...id"
)

items_with_se_info <- sample_ses %>%
  left_join(ses_items, by = "...id")

bench::mark(
  items_with_se_info %>%
    hit_zonal_stats_api(100, dedupe_items = TRUE),
  items_with_se_info %>%
      hit_zonal_stats_api(150, dedupe_items = TRUE)
)

# when n_days = 10, 25 & 50 at once are clearly best
# A tibble: 4 × 13
# expression     min median
# <bch:expr>     <bch:> <bch:>
# 1 10 at once…  11.5s  11.5s
# 2 15 at once…  5.85s  5.85s
# 3 25 at once…  2.21s  2.21s
# 4 50 at once…  1.29s  1.29s

# Try again now with n_days = 60

# A tibble: 4 × 13
# expression      min median
# <bch:expr>    <bch:> <bch:>
# 1 10 at once…  1.74m  1.74m
# 2 15 at once…  1.13m  1.13m
# 3 25 at once… 40.05s 40.05s
# 4 50 at once… 17.78s 17.78s

# Once up to 100ish, improvements are negligible
# So this seems to be the cap

# A tibble: 4 × 13
# expression        min median
# <bch:expr>     <bch:> <bch:>
# 1 50 at once…. 17.06s 17.06s
# 2 100 at once…  8.44s  8.44s
# 3 200 at once…  8.39s  8.39s
# 4 300 at once…  8.74s  8.74s

# 100 is best

# Try with n_days = 365 now
# n = 3 distinct sites
# (approx ~5000 items?)

# A tibble: 2 × 13
# expression        min median
# <bch:expr>      <bch> <bch:>
# 1 100 at once… 4.06m  4.06m
# 2 150 at once… 3.15m  3.15m

# with n = 5 sites
# (7665 items)
