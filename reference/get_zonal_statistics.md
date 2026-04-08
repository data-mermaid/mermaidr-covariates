# Get zonal statistics

Get zonal statistics. For \*each\* of the \`n_days\` prior to the sample
event date, returns summarised values of covariate data for \`radius\`
metres around each site location, using \`spatial_stats\` to determine
how to spatially summarise the data.

## Usage

``` r
get_zonal_statistics(
  se,
  covariate,
  n_days = 365,
  radius = 1000,
  spatial_stats = c("min", "max", "mean"),
  date_col = "sample_date"
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

- date_col:

  Date back from (using `n_days`). Defaults to "sample_date".
