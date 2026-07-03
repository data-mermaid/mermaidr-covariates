# Attach covariate data

Attach covariate data that is in a vector format. Not date dependent –
only attach based on sample event location.

## Usage

``` r
attach_covariate_data(
  se,
  covariate,
  dataset = NULL,
  columns = NULL,
  date_col = "sample_date"
)
```

## Arguments

- se:

  Sample events from `mermaidr`

- covariate:

  Covariate to attach. Both covariate title or ID are permitted. Run
  [`list_covariates()`](https://data-mermaid.github.io/mermaidr-covariates/reference/list_covariates.md)
  to see available covariates.

- dataset:

  Dataset within the covariate. Not required when there is only one
  dataset. Run
  [`list_datasets_for_covariate`](https://data-mermaid.github.io/mermaidr-covariates/reference/list_datasets_for_covariate.md)
  to see datasets and columns.

- columns:

  Columns within the dataset. When NULL (the default), returns all
  columns. Run
  [`list_datasets_for_covariate`](https://data-mermaid.github.io/mermaidr-covariates/reference/list_datasets_for_covariate.md)
  to see datasets and bands.

- date_col:

  Date column in data. Defaults to "sample_date".
