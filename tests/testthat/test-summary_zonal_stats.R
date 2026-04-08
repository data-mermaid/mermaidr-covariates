test_that("get_summary_zonal_statistics returns spatial_stats and temporal_stats columns", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  se <- mermaidr::mermaid_get_project_data(
    "4d23d2a1-774f-4ccf-b567-69f95e4ff572",
    "fishbelt",
    "sampleevents",
    limit = 1
  )

  covariates <- get_summary_zonal_statistics(
    se,
    "Daily Sea Surface Temperature",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean",
    temporal_stats = c("min", "max")
  )

  covariates <- covariates[["covariates"]][[1]]

  expect_named(
    covariates,
    c(
      "covariate", "start_date", "end_date", "n_dates",
      "band", "temporal_stat", "spatial_stat", "value"
    )
  )

  expect_equal(covariates[["temporal_stat"]], c("min", "max"))
})

test_that("summary_zonal_stats works with sample_date renamed", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  se <- mermaidr::mermaid_get_project_data(
    "4d23d2a1-774f-4ccf-b567-69f95e4ff572",
    "fishbelt",
    "sampleevents",
    limit = 1
  )

  orig_covariates <- get_summary_zonal_statistics(
    se,
    "Daily Sea Surface Temperature",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean",
    temporal_stats = c("min", "max")
  )

  se <- se %>%
    dplyr::rename(date = sample_date)

  rename_date_covariates <- get_summary_zonal_statistics(
    se,
    "Daily Sea Surface Temperature",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean",
    temporal_stats = c("min", "max"),
    date_col = "date"
  )

  expect_identical(orig_covariates["covariates"], rename_date_covariates["covariates"])
})

test_that("summary_zonal_stats works with different date_col, retains both cols -- same date", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  se <- mermaidr::mermaid_get_project_data(
    "4d23d2a1-774f-4ccf-b567-69f95e4ff572",
    "fishbelt",
    "sampleevents",
    limit = 1
  )

  orig_covariates <- get_summary_zonal_statistics(
    se,
    "Daily Sea Surface Temperature",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean",
    temporal_stats = c("min", "max")
  )

  se <- se %>%
    dplyr::mutate(date = sample_date)

  rename_date_covariates <- get_summary_zonal_statistics(
    se,
    "Daily Sea Surface Temperature",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean",
    temporal_stats = c("min", "max"),
    date_col = "date"
  )

  expect_identical(orig_covariates["covariates"], rename_date_covariates["covariates"])

  expect_true("sample_date" %in% names(rename_date_covariates))
})

test_that("summary_zonal_stats works with different date_col, retains both cols -- different date", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  se <- mermaidr::mermaid_get_project_data(
    "4d23d2a1-774f-4ccf-b567-69f95e4ff572",
    "fishbelt",
    "sampleevents",
    limit = 1
  )

  orig_covariates <- get_summary_zonal_statistics(
    se,
    "Daily Sea Surface Temperature",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean",
    temporal_stats = c("min", "max")
  )

  se <- se %>%
    dplyr::mutate(date = "2026-01-01")

  new_date_covariates <- get_summary_zonal_statistics(
    se,
    "Daily Sea Surface Temperature",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean",
    temporal_stats = c("min", "max"),
    date_col = "date"
  )

  expect_false(identical(orig_covariates["covariates"], new_date_covariates["covariates"]))

  expect_true(
    new_date_covariates %>%
      dplyr::select(covariates) %>%
      tidyr::unnest(covariates) %>%
      dplyr::pull(end_date) %>% unique() ==
      "2026-01-01"
  )
})
