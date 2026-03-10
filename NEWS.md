# mermaidrcovariates 0.1.1

* Fix bug with calculating `n_dates` returned from `get_summary_zonal_statistics()`.

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
