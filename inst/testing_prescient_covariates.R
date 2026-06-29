library(mermaidr)
library(dplyr)
library(tidyr)

se_full <- mermaid_get_summary_sampleevents()

# covariate <- "50 Reefs+ prioritization"
# covariate <- "ACA Benthic Habitat"
# covariate <- "ACA Reef Extent"
covariate <- "Daily Sea Surface Temperature"
# covariate <- "GPW Global Sediment Exposure"
# covariate <- "GPW Global Sediment Load"
# covariate <- "GPW Land Use and Land Cover"

set.seed(1234)
if (covariate == "Daily Sea Surface Temperature") {
  se <- se_full %>%
    sample_n(50)
} else {
  se <- se_full %>%
    sample_n(5000)
}

res_full <- se %>%
  mutate(...row = row_number()) %>%
  # head(10) %>%
  # slice(c(1,6276)) %>%
  # head(1000) %>%
  get_zonal_statistics(covariate, spatial_stats = "mean", n_days = 5, bands = 1)

res_full %>%
  select(covariates) %>%
  unnest(covariates) %>%
  nrow() == nrow(se)

# all_na_test <- res_full %>%
#   select(...row, covariates) %>%
#   unnest(covariates) %>%
#   filter(is.na(date)) %>% head(20) %>% pull(...row)
#
# res_test <- se %>%
#     mutate(...row = row_number()) %>%
#   slice(all_na_test) %>%
#   get_zonal_statistics(covariate, spatial_stats = "mean", n_days = 5, bands = 1)
