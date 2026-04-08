test_that("get_zonal_statistics allows using covariate name or ID", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  se <- mermaidr::mermaid_get_project_data(
    "4d23d2a1-774f-4ccf-b567-69f95e4ff572",
    "fishbelt",
    "sampleevents",
    limit = 1
  )

  zs_name <- get_zonal_statistics(
    se,
    "Daily Sea Surface Temperature",
    n_days = 1,
    radius = 10,
    spatial_stats = "mean"
  )

  zs_id <- get_zonal_statistics(
    se,
    "50b810fb-5f17-4cdb-b34b-c377837e2a29",
    n_days = 1,
    radius = 10,
    spatial_stats = "mean"
  )

  expect_identical(zs_name, zs_id)
})

test_that("NA returned for value ONLY when there is data on date, but not within radius", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  # No data within 1000m of this site, known
  se <- mermaidr::mermaid_get_project_data(
    "75ef7a5a-c770-4ca6-b9f8-830cab74e425",
    "benthicpit",
    "sampleevents",
    token = NULL
  ) %>%
    dplyr::filter(
      site == "MAP28",
      sample_date == "2010-05-07"
    )

  covariates <- get_zonal_statistics(se,
    "Daily Sea Surface Temperature",
    n_days = 1,
    radius = 1000,
    spatial_stats = "mean"
  )

  expect_true(covariates %>%
    dplyr::pull(covariates) %>%
    purrr::map_dbl("value") %>%
    is.na())

  expect_false(covariates %>%
    dplyr::pull(covariates) %>%
    purrr::map_chr("date") %>%
    is.na())
})

test_that("NA returned for all columns when there is no data within date range", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  se <- mermaidr::mermaid_get_project_data(
    "4d23d2a1-774f-4ccf-b567-69f95e4ff572",
    "fishbelt",
    "sampleevents",
    limit = 1
  )

  se <- se %>%
    dplyr::mutate(sample_date = as.Date("1980-01-01"))

  covariates <- get_zonal_statistics(se,
    "Daily Sea Surface Temperature",
    n_days = 1,
    radius = 10,
    spatial_stats = "mean"
  )

  covariates <- covariates %>%
    dplyr::pull(covariates) %>%
    purrr::pluck(1)

  expect_true(is.na(covariates[["start_date"]]))
  expect_true(is.na(covariates[["end_date"]]))
  expect_true(covariates[["n_dates"]] == 0)
  expect_true(is.na(covariates[["date"]]))
  expect_true(covariates[["spatial_stat"]] == "mean")
  expect_true(is.na(covariates[["start_date"]]))
  expect_true(is.na(covariates[["value"]]))
})

test_that("get_zonal_statistics works with sample_date renamed", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  se <- mermaidr::mermaid_get_project_data(
    "4d23d2a1-774f-4ccf-b567-69f95e4ff572",
    "fishbelt",
    "sampleevents",
    limit = 1
  )

  orig_covariates <- get_zonal_statistics(
    se,
    "Daily Sea Surface Temperature",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean"
  )

  se <- se %>%
    dplyr::rename(date = sample_date)

  rename_date_covariates <- get_zonal_statistics(
    se,
    "Daily Sea Surface Temperature",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean",
    date_col = "date"
  )

  expect_identical(orig_covariates["covariates"], rename_date_covariates["covariates"])
})

test_that("get_zonal_statistics works with different date_col, retains both cols -- same date", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  se <- mermaidr::mermaid_get_project_data(
    "4d23d2a1-774f-4ccf-b567-69f95e4ff572",
    "fishbelt",
    "sampleevents",
    limit = 1
  )

  orig_covariates <- get_zonal_statistics(
    se,
    "Daily Sea Surface Temperature",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean"
  )

  se <- se %>%
    dplyr::mutate(date = sample_date)

  rename_date_covariates <- get_zonal_statistics(
    se,
    "Daily Sea Surface Temperature",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean",
    date_col = "date"
  )

  expect_identical(orig_covariates["covariates"], rename_date_covariates["covariates"])

  expect_true("sample_date" %in% names(rename_date_covariates))
})

test_that("get_zonal_statistics works with different date_col, retains both cols -- different date", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()
  se <- mermaidr::mermaid_get_project_data(
    "4d23d2a1-774f-4ccf-b567-69f95e4ff572",
    "fishbelt",
    "sampleevents",
    limit = 1
  )

  orig_covariates <- get_zonal_statistics(
    se,
    "Daily Sea Surface Temperature",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean"
  )

  se <- se %>%
    dplyr::mutate(date = "2026-01-01")

  new_date_covariates <- get_zonal_statistics(
    se,
    "Daily Sea Surface Temperature",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean",
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
