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
