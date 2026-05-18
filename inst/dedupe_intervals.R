library(dplyr)

n_days <- 365
n <- 200

ses <- readRDS(here::here("inst/extdata/test_sse.rds"))

set.seed(1234)

sample_ses <- ses %>%
    sample_n(500)

sample_ses <- ses %>%
  filter(
    site %in% c("BK miad", "1000"),
    sample_date %in% c("2025-02-12", "2025-02-22", "2014-03-13")
  )

# sample_ses <- ses %>%
# sample_n(n)

sample_ses %>%
  add_id_for_iteration(n_days, dedupe_items = TRUE) %>%
  distinct(site, ...id) %>%
  nrow() < nrow(sample_ses)

covariate_id <- "50b810fb-5f17-4cdb-b34b-c377837e2a29"

n_days <- 5

no_dedupe_res <- sample_ses %>%
  get_summary_zonal_statistics(covariate_id,
    spatial_stats = "mean", temporal_stats = "mean",
    n_days = n_days
  )

saveRDS(no_dedupe_res, "inst/no_dedupe_res.rds")

dedupe_res <- sample_ses %>%
  get_summary_zonal_statistics(covariate_id,
    spatial_stats = "mean",
    temporal_stats = "mean", dedupe_items = TRUE,
    n_days = n_days
  )

saveRDS(dedupe_res, "inst/dedupe_res.rds")
expect_equal(names(dedupe_res), names(no_dedupe_res), ignore_attr = TRUE)

n_days <- 365

bench::mark(
  check = FALSE,
  sample_ses %>%
    get_summary_zonal_statistics(covariate_id,
      spatial_stats = "mean", temporal_stats = "mean",
      n_days = n_days
    ),
  sample_ses %>%
    get_summary_zonal_statistics(covariate_id,
      spatial_stats = "mean",
      temporal_stats = "mean", dedupe_items = TRUE,
      n_days = n_days
    )
)
