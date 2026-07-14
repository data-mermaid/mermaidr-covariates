test_that("summarise_zonal_statistics returns zonal_statistics and summary_zonal_statistics columns", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  se <- dplyr::tribble(
    ~site, ~latitude, ~longitude, ~sample_date,
    "NosyKarabo_06", -12.996556, 48.562083, "2020-12-10"
  )


  zs <- get_zonal_statistics(
    se,
    "Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean"
  )

  zs_summary <- zs %>%
    summarise_zonal_statistics("mean")

  expect_named(zs_summary, c(names(zs), "summary_zonal_statistics"))

  expect_equal(
    zs_summary[["summary_zonal_statistics"]][[1]] %>% dplyr::mutate(value = round(value, 2)),
    structure(list(
      covariate = "Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)", start_date = structure(18587, class = "Date"),
      end_date = structure(18606, class = "Date"), n_dates = 20,
      band = 1, spatial_stat = "mean", temporal_stat = "mean",
      value = 28.67
    ), row.names = c(NA, -1L), class = c(
      "tbl_df",
      "tbl", "data.frame"
    ))
  )
})

test_that("summarise_zonal_statistics returns NA start/end date for data without results, 0 for date", {
  zs_empty <- dplyr::tibble(
    covariate = "test", date = NA, band = 1, spatial_stat = "mean", value = NA
  )
  zs <- dplyr::bind_rows(zs_empty, zs_empty) %>% # Two rows of this just to avoid the warning of summary results being identical
    tidyr::nest(zonal_statistics = dplyr::everything())

  zs_summary <- zs %>%
    summarise_zonal_statistics("mean")

  expect_equal(
    zs_summary[["summary_zonal_statistics"]],
    list(structure(list(
      covariate = "test", start_date = NA, end_date = NA,
      n_dates = 0, band = 1, spatial_stat = "mean", temporal_stat = "mean",
      value = NA
    ), row.names = c(NA, -1L), class = c(
      "tbl_df",
      "tbl", "data.frame"
    )))
  )

  zs <- zs_empty %>%
    dplyr::bind_rows(zs_empty %>%
      dplyr::mutate(date = "2026-01-01", value = 5)) %>%
    dplyr::mutate(id = dplyr::row_number())

  zs <- dplyr::bind_rows(zs, zs)

  expect_identical(
    zs %>%
      tidyr::nest(zonal_statistics = dplyr::everything(), .by = "id") %>%
      summarise_zonal_statistics("max") %>%
      dplyr::pull(summary_zonal_statistics),
    list(structure(list(
      covariate = "test", start_date = NA_character_,
      end_date = NA_character_, n_dates = 0, band = 1, spatial_stat = "mean",
      temporal_stat = "max", value = NA_real_
    ), row.names = c(
      NA,
      -1L
    ), class = c("tbl_df", "tbl", "data.frame")), structure(list(
      covariate = "test", start_date = "2026-01-01", end_date = "2026-01-01",
      n_dates = 1, band = 1, spatial_stat = "mean", temporal_stat = "max",
      value = 5
    ), row.names = c(NA, -1L), class = c(
      "tbl_df", "tbl",
      "data.frame"
    )))
  )
})

test_that("start_date and end_date are the actual dates of covariate data,
not the start/end date based on the date_col and n_days", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  se <- dplyr::tribble(
    ~site, ~latitude, ~longitude, ~sample_date,
    "NosyKarabo_06", -12.996556, 48.562083, "2020-12-10"
  )

  se <- se %>%
    dplyr::mutate(date = as.Date("1985-01-05")) # 1985-01-01 is the first date of data

  covariates <- se %>%
    get_zonal_statistics(
      "Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)",
      n_days = 10,
      radius = 10,
      spatial_stats = "mean",
      date_col = "date"
    )

  covariates_summary <- covariates %>%
    summarise_zonal_statistics("mean")

  zonal_statistics <- covariates_summary[["zonal_statistics"]][[1]]
  summary_zonal_statistics <- covariates_summary[["summary_zonal_statistics"]][[1]]

  expect_true(summary_zonal_statistics[["start_date"]] == min(zonal_statistics[["date"]]))
  expect_true(summary_zonal_statistics[["end_date"]][[1]] == max(zonal_statistics[["date"]]))
})
