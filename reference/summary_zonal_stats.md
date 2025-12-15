# Get summary of zonal statistics

Get summary of zonal statistics

## Usage

``` r
summary_zonal_stats(
  df,
  collection,
  n_days = 365,
  buffer = 1000,
  stats = c("min", "max", "mean")
)
```

## Arguments

- df:

  Sample events from `mermaidr`

- collection:

  Covariates to get statistics for

- n_days:

  Number of days prior to sample date to get statistics for. Defaults to
  365.

- buffer:

  Buffer around site location, in metres. Defaults to 1000.

- stats:

  Summary statistics. One of: min, max, or mean.

## Examples

``` r
# TODO
```
