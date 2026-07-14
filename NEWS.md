# mermaidrcovariates 1.0.0

* Added `attach_covariate_data()` for accessing vector covariates
* Added `covariate_helper()` for understanding which of `attach_covariate_data()` or `get_zonal_statistics()` to use
* Added `list_datasets_for_covariate()` for listing datasets and bands/columns within a covariate
* Improved parallelization

Large rewrite of zonal statistics API:

* Deprecated `get_summary_zonal_statistics()` in favour of using `get_zonal_statistics()` and then` summarise_zonal_statistics()`
* Renamed `covariates` column in `get_zonal_statistics()` to `zonal_statistics`
* Renamed `covariates` column from `summarise_zonal_statistics()` to `summary_zonal_statistics` -- original `zonal_statistics()` column is retained
* Removed columns `start_date`, `end_date`, and `n_dates` from `get_zonal_statistics()` output -- only appear in output from `summarise_zonal_statistics()` 
* Added support for multiple bands, specifying dataset

# mermaidrcovariates 0.1.2

* Add `search_covariates()` to search by title or description
* Add `date_col` to `get_zonal_statistics()` and `get_summary_zonal_statistics()`,
which allows for using a column other than `"sample_date"`.

# mermaidrcovariates 0.1.1

* Fix bug with calculating `n_dates` returned from `get_summary_zonal_statistics()`.
* Fix bug where `get_zonal_statistics()` and `get_summary_zonal_statistics()` were
stripping columns from `se` in returned data.

# mermaidrcovariates 0.1.0

* `list_covariates()` returns a data frame by default, and the print method when
it is a list (with `as_data_frame = FALSE`) is improved.
* `get_zonal_statistics()` takes the covariate name *or* ID (previously just ID). 
The argument name is `covariate` instead of `covariate_id`. 
* `get_zonal_statistics()` returns `n_dates` in `covariates` column, gets stats 
for `n_days - 1` prior to sample date and sample date.
* `get_zonal_statistics()` now returns zonal statistics for *each* of the `n_days`, 
while `get_summary_zonal_statistics()` summarises the zonal statistics over time.
* `get_summary_zonal_statistics()` requires both `spatial_stats` -- how to 
summarise spatially -- and `temporal_stats` -- how to summarise over time. 
It returns both of these columns under `covariates`, along with `value`.
* `get_zonal_statistics()` requires only `spatial_stats`. It returns this column 
under `covariates`, along with `value`.
* The argument `buffer` has been renamed to `radius`.

# mermaidrcovariates 0.0.1

Initial MVP.
