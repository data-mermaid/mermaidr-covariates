# Get summary zonal statistics

Get zonal statistics. Unlike `get_zonal_statistics`, this summarises the
data for \*all \`n_days\`\* prior to the sample event date, using
\`spatial_stats\` to determine how to spatially summarise the data, and
\`temporal_stats\` to summarise the resulting data over time.

## Usage

``` r
get_summary_zonal_statistics(
  se,
  covariate,
  n_days = 365,
  radius = 1000,
  spatial_stats = c("min", "max", "mean"),
  temporal_stats = c("min", "max", "mean")
)
```

## Arguments

- se:

  Sample events from `mermaidr`

- covariate:

  Covariate to get statistics for. Both covariate title or ID are
  permitted.

- n_days:

  Number of days to get statistics for. Includes the sample date itself,
  and days prior to it – e.g., 365 days would include the sample date
  and the 364 days prior. Defaults to 365.

- radius:

  Radius around site location, in metres. Defaults to 1000.

- spatial_stats:

  Spatial statistics – used to summarise all data around the site
  location, according to the `radius` set.

- temporal_stats:

  Temporal statistics – used to summarise the data over time.
