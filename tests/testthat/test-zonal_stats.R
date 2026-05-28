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

  expect_true(covariates[["n_dates"]] == 0)
  expect_true(is.na(covariates[["date"]]))
  expect_true(covariates[["spatial_stat"]] == "mean")
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

test_that("start_date and end_date are the actual dates of covariate data,
not the start/end date based on the date_col and n_days", {
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
    dplyr::mutate(date = as.Date("1985-01-05")) # 1985-01-01 is the first date of data

  covariates <- se %>%
    dplyr::select(project, site, latitude, longitude, sample_date, date) %>%
    get_zonal_statistics(
      "Daily Sea Surface Temperature",
      n_days = 10,
      radius = 10,
      spatial_stats = "mean",
      date_col = "date"
    )

  covariates <- covariates[["covariates"]][[1]]

  expect_true(covariates[["start_date"]][[1]] == min(covariates[["date"]]))
  expect_true(covariates[["end_date"]][[1]] == max(covariates[["date"]]))
})

test_that("Parellelization produces results identical to prior method", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  covariates_to_test_parallelization_against <- readRDS(
    test_path(
      "covariates_to_test_parallelization_against.rds"
    )
  ) %>%
    dplyr::arrange(project_id, site, sample_date) %>%
    dplyr::select(-...start_date, -...end_date)

  ses <- covariates_to_test_parallelization_against %>%
    dplyr::select(-covariates)

  new_covariates <- ses %>%
    get_zonal_statistics("Daily Sea Surface Temperature",
      n_days = 10,
      spatial_stats = "mean", radius = 100,
      .progress = FALSE
    ) %>%
    dplyr::arrange(project_id, site, sample_date)

  expect_equal(
    new_covariates,
    covariates_to_test_parallelization_against,
    ignore_attr = TRUE
  )
})
