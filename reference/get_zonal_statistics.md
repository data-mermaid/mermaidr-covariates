# Get zonal statistics

Get zonal statistics for covariate data that is in a raster format..
Using `spatial_stats` to spatially summarise covariates data, for each
relevant date prior to the sample event date. For example, for a daily
covariate like "Daily Global 5km Satellite Sea Surface Temperature
(CoralTemp)", the function returns SST for \*each\* of the `n_days`
prior to the sample event date. If the covariate is periodic, e.g.
occurring every 5 years, it returns the most recent value. If the
covariate only occurs once (e.g. 50 Reefs+ prioritization), it returns
that data.

## Usage

``` r
get_zonal_statistics(
  se,
  covariate,
  spatial_stats = "mean",
  radius = 100,
  n_days = NULL,
  dataset = NULL,
  bands = NULL,
  date_col = "sample_date",
  .progress = TRUE
)
```

## Arguments

- se:

  Sample events from `mermaidr`

- covariate:

  Covariate to get statistics for. Both covariate title or ID are
  permitted. Run
  [`list_covariates`](https://data-mermaid.github.io/mermaidr-covariates/reference/list_covariates.md)
  to see available covariates.

- spatial_stats:

  Spatial statistics – used to summarise all data around the site
  location, according to the `radius` set. If `radius` is 0, then
  `spatial_stats` is not relevant; it is just the value itself.

- radius:

  Radius around site location, in metres. Defaults to 100.

- n_days:

  Number of days to get statistics for. Includes the sample date itself,
  and days prior to it – e.g., 365 days would include the sample date
  and the 364 days prior. Only relevant for covariates that are daily;
  otherwise ignored.

- dataset:

  Dataset within the covariate. Not required in most cases, when there
  is only one dataset. Run
  [`list_datasets_for_covariate`](https://data-mermaid.github.io/mermaidr-covariates/reference/list_datasets_for_covariate.md)
  to see datasets and bands.

- bands:

  Bands within the dataset. Not required in most cases, when there is
  only one band. When required, can be numeric or named band. Run
  [`list_datasets_for_covariate`](https://data-mermaid.github.io/mermaidr-covariates/reference/list_datasets_for_covariate.md)
  to see datasets and bands.

- date_col:

  Sample date column – used for date-dependent covariates (e.g. daily or
  periodic). Defaults to "sample_date".

- .progress:

  Whether to show progress bar and time remaining. Defaults to TRUE.
