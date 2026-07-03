# Summarise zonal statistics

Summarise zonal statistics. Using the results from
`get_zonal_statistics`, this summarises the data for \*all \`n_days\`\*
prior to the sample event date, using \`temporal_stats\` to summarise
the resulting data over time.

## Usage

``` r
summarise_zonal_statistics(
  zonal_statistics,
  temporal_stats = c("min", "max", "mean")
)
```

## Arguments

- zonal_statistics:

  Sample events with zonal statistics, from `get_zonal_statistics`.

- temporal_stats:

  Temporal statistics – used to summarise the data over time.
