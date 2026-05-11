library(mermaidr)
library(dplyr)

se <- mermaid_get_summary_sampleevents()

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

bench::mark(
    vary_chunk_threshold(25),
    vary_chunk_threshold(50),
    vary_chunk_threshold(100),
    vary_chunk_threshold(200),
    vary_chunk_threshold(500),
    vary_chunk_threshold(1000)
)
