test_that("get_zonal_statistics allows using covariate name or ID", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  se <- dplyr::tribble(
    ~site, ~latitude, ~longitude, ~sample_date,
    "NosyKarabo_06", -12.996556, 48.562083, "2020-12-10"
  )

  zs_name <- get_zonal_statistics(
    se,
    "Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)",
    n_days = 1,
    radius = 10,
    spatial_stats = "mean"
  )

  zs_id <- get_zonal_statistics(
    se,
    "daily_sst",
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

  se <- dplyr::tribble(
    ~site, ~latitude, ~longitude, ~sample_date,
    "NosyKarabo_06", -12.996556, 48.562083, "2020-12-10"
  )

  se <- se %>%
    dplyr::mutate(sample_date = as.Date("1980-01-01"))

  covariates <- get_zonal_statistics(se,
    "Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)",
    n_days = 1,
    radius = 10,
    spatial_stats = "mean"
  )

  zonal_statistics <- covariates %>%
    dplyr::pull(zonal_statistics) %>%
    purrr::pluck(1)

  expect_true(is.na(zonal_statistics[["date"]]))
  expect_true(zonal_statistics[["spatial_stat"]] == "mean")
  expect_true(is.na(zonal_statistics[["value"]]))
})

test_that("get_zonal_statistics works with sample_date renamed", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  se <- dplyr::tribble(
    ~site, ~latitude, ~longitude, ~sample_date,
    "NosyKarabo_06", -12.996556, 48.562083, "2020-12-10"
  )


  orig_covariates <- get_zonal_statistics(
    se,
    "Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean"
  )

  se <- se %>%
    dplyr::rename(date = sample_date)

  rename_date_covariates <- get_zonal_statistics(
    se,
    "Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean",
    date_col = "date"
  )

  expect_identical(orig_covariates["zonal_statistics"], rename_date_covariates["zonal_statistics"])
})

test_that("get_zonal_statistics works with different date_col, retains both cols -- same date", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  se <- dplyr::tribble(
    ~site, ~latitude, ~longitude, ~sample_date,
    "NosyKarabo_06", -12.996556, 48.562083, "2020-12-10"
  )


  orig_covariates <- get_zonal_statistics(
    se,
    "Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean"
  )

  se <- se %>%
    dplyr::mutate(date = sample_date)

  rename_date_covariates <- get_zonal_statistics(
    se,
    "Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean",
    date_col = "date"
  )

  expect_identical(orig_covariates["zonal_statistics"], rename_date_covariates["zonal_statistics"])

  expect_true("sample_date" %in% names(rename_date_covariates))
})

test_that("get_zonal_statistics works with different date_col, retains both cols -- different date", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  se <- dplyr::tribble(
    ~site, ~latitude, ~longitude, ~sample_date,
    "NosyKarabo_06", -12.996556, 48.562083, "2020-12-10"
  )


  orig_covariates <- get_zonal_statistics(
    se,
    "Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean"
  )

  se <- se %>%
    dplyr::mutate(date = "2026-01-01")

  new_date_covariates <- get_zonal_statistics(
    se,
    "Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)",
    n_days = 20,
    radius = 10,
    spatial_stats = "mean",
    date_col = "date"
  )

  expect_false(identical(orig_covariates["zonal_statistics"], new_date_covariates["zonal_statistics"]))

  expect_true(
    new_date_covariates %>%
      dplyr::select(zonal_statistics) %>%
      tidyr::unnest(zonal_statistics) %>%
      dplyr::pull(date) %>% max() ==
      "2026-01-01"
  )
})

test_that("new test scenarios...", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  covariates <- list_covariates()

  covariates_type <- covariates %>%
    dplyr::select(id) %>%
    dplyr::distinct() %>%
    dplyr::rowwise() %>%
    dplyr::mutate(type = get_collection_type(id)) %>%
    dplyr::ungroup()

  cog_covariates <- covariates_type %>%
    dplyr::filter(stringr::str_detect(type, "raster")) %>%
    dplyr::pull(id)

  se <- dplyr::tribble(
    ~site, ~latitude, ~longitude, ~sample_date,
    "C5", -17.060783, 179.0013, "2011-03-29",
    "Three Buoys shallow", 11.128612, 72.734578, "2016-05-06",
    "BUA25", -16.84262, 178.539783, "2016-07-20",
    "WFR6", 16.83796196, -87.8439228, "2023-07-11",
    "RasGhanada", 24.84759632, 54.69025958, "2011-08-01",
    "Andravona_12", -22.44011, 43.23909, "2016-03-21",
    "Ulenge Kusini", -5.02441, 39.16562, "2023-12-16",
    "Deudap 1", 5.607141944, 95.19258111, "2019-07-07",
    "KB09", -16.96729, 178.99922, "2009-04-23",
    "Kavulik MPA deep", -2.529215, 150.538147, "2021-12-01"
  )

  purrr::walk(
    cog_covariates,
    \(covariate) {
      covariate <- get_covariate_id(covariate)
      cog_assets <- get_covariate_cog_datasets(covariate)

      bands <- get_collection_bands_and_columns(covariate)[[cog_assets]]

      interval <- get_covariate_interval(covariate)

      if (interval %in% c("once", "periodic")) {
        n_days <- NULL
        n_days_counter <- 1
      } else {
        n_days_counter <- n_days <- 5
      }

      multiple_bands <- nrow(bands) > 1

      # Test band not specified
      if (!multiple_bands) {
        zs <- get_zonal_statistics(se, covariate, n_days = n_days)

        expect_named(zs, c(names(se), "zonal_statistics"))
      } else {
        expect_error(get_zonal_statistics(se, covariate, n_days = n_days), "Please specify")
      }

      # Test band specified by number
      zs <- get_zonal_statistics(se, covariate, band = 1, n_days = n_days)

      expect_named(zs, c(names(se), "zonal_statistics"))

      if (multiple_bands) {
        bands_named <- all(!is.na(bands[["name"]]))

        if (bands_named) {
          # Test band specified by name
          zs <- get_zonal_statistics(se, covariate, band = bands[1, ][["name"]], n_days = n_days)
          expect_named(zs, c(names(se), "zonal_statistics"))

          # Invalid band name
          expect_error(
            get_zonal_statistics(se, covariate, band = "invalid"),
            "Invalid band"
          )

          # Multiple bands, name
          zs <- get_zonal_statistics(se, covariate, band = bands[1:2, ][["name"]], n_days = n_days)
          expect_named(zs, c(names(se), "zonal_statistics"))
          zs_unnest <- zs %>%
            dplyr::select(zonal_statistics) %>%
            tidyr::unnest(zonal_statistics)
          expect_true(nrow(zs_unnest) == nrow(se) * 2 * n_days_counter)
          expect_true(all(zs_unnest[["band_name"]] %in% bands[1:2, ][["name"]]))

          # Multiple bands, mixed
          zs <- get_zonal_statistics(se, covariate, band = c(1, bands[2, ][["name"]]))
          expect_named(zs, c(names(se), "zonal_statistics"))
          zs_unnest <- zs %>%
            dplyr::select(zonal_statistics) %>%
            tidyr::unnest(zonal_statistics)
          expect_true(nrow(zs_unnest) == nrow(se) * 2 * n_days_counter)
          expect_true(all(zs_unnest[["band_name"]] %in% bands[1:2, ][["name"]]))
          expect_true(all(zs_unnest[["band"]] %in% c(1, 2)))
        }

        # Multiple bands, number
        zs <- get_zonal_statistics(se, covariate, band = c(1, 2), n_days = n_days)
        expect_named(zs, c(names(se), "zonal_statistics"))
        zs_unnest <- zs %>%
          dplyr::select(zonal_statistics) %>%
          tidyr::unnest(zonal_statistics)
        expect_true(nrow(zs_unnest) == nrow(se) * 2 * n_days_counter)
        expect_true(all(zs_unnest[["band"]] %in% c(1, 2)))
      }

      # Invalid band number
      expect_error(
        get_zonal_statistics(se, covariate, band = 9999),
        "Invalid band"
      )
    }
  )

  # Non-COG dataset
  expect_error(get_zonal_statistics(se, "Country Boundaries"), "cannot get zonal statistics")

  # covariates that are relevant to date:
  purrr::walk(
    c(
      "Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)",
      "Daily Global 5km Satellite Coral Bleaching Alert Area",
      "Daily Global 5km Satellite Coral Bleaching Degree Heating Week",
      "GPW Global Sediment Exposure",
      "GPW Global Sediment Load",
      "GPW Land Use and Land Cover"
    ),
    \(covariate) {
      covariate <- get_covariate_id(covariate)
      interval <- get_covariate_interval(covariate)

      if (interval %in% c("once", "periodic")) {
        n_days <- NULL
      } else {
        n_days <- 5
      }

      # What happens when all NA, with no data on that date?
      # i.e., date is before covariate dates
      zs <- se %>%
        dplyr::mutate(new_date = "1980-01-01") %>%
        get_zonal_statistics(covariate, date_col = "new_date", n_days = n_days, band = 1)

      expect_true(all(zs[["zonal_statistics"]][["date"]] %>% is.na()))
      expect_true(all(zs[["zonal_statistics"]][["value"]] %>% is.na()))

      # Some before relevant date
      zs <- se %>%
        dplyr::mutate(
          new_date = ifelse(sample_date == min(sample_date), "1980-01-01", NA_character_),
          new_date = dplyr::coalesce(new_date, sample_date)
        ) %>%
        get_zonal_statistics(covariate, date_col = "new_date", n_days = n_days, band = 1)

      expect_true(
        zs %>%
          dplyr::filter(new_date == "1980-01-01") %>%
          tidyr::unnest(zonal_statistics) %>%
          dplyr::pull(date) %>%
          is.na() %>%
          all()
      )

      expect_true(
        zs %>%
          dplyr::filter(new_date == "1980-01-01") %>%
          tidyr::unnest(zonal_statistics) %>%
          dplyr::pull(value) %>%
          is.na() %>%
          all()
      )

      expect_false(
        zs %>%
          dplyr::filter(new_date != "1980-01-01") %>%
          tidyr::unnest(zonal_statistics) %>%
          dplyr::pull(date) %>%
          is.na() %>%
          all()
      )
    }
  )
})

test_that("For periodic covariates, get_zonal_statistics() gets the MOST RECENT data only", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  se <- dplyr::tribble(
    ~site, ~latitude, ~longitude, ~sample_date,
    "NT1", -17.3741, 179.4217, "2017-09-09",
    "NF2", -17.37898, 179.41577, "2013-05-13",
    "NAOBT1", -17.622, 178.831, "2019-05-23",
    "DawaC6", -17.6759, 178.61905, "2019-10-16",
    "ME6", -17.31758, 178.16589, "2025-11-05"
  )

  covariate <- "gpw_sediment_load"
  bands <- 1

  zs <- se %>%
    get_zonal_statistics(covariate, radius = 100, bands = bands)

  expect_equal(
    zs %>%
      dplyr::pull(zonal_statistics) %>%
      purrr::map_dbl(nrow) %>%
      unique(),
    length(bands)
  )

  expect_equal(
    zs %>%
      dplyr::pull(zonal_statistics) %>%
      purrr::map("band") %>%
      unlist() %>%
      unique(),
    bands
  )

  item_dates <- get_collection_items(covariate) %>%
    purrr::map_dfr(\(x) {
      dplyr::tibble(item_date = as.Date(x$properties$datetime))
    })

  expect_true(zs %>%
    dplyr::select(sample_date, zonal_statistics) %>%
    dplyr::mutate(sample_date = as.Date(sample_date)) %>%
    tidyr::unnest(zonal_statistics) %>%
    dplyr::left_join(item_dates, by = dplyr::join_by(closest(sample_date >= item_date))) %>%
    dplyr::mutate(date_match = date == item_date) %>%
    dplyr::pull(date_match) %>%
    unique())

  # If the data is before the first item, do not return anything
  se <- se %>%
    dplyr::mutate(sample_date = ifelse(site == "NT1", "1999-01-01", sample_date))

  zs <- se %>%
    get_zonal_statistics(covariate, radius = 100, bands = bands)

  no_data_se <- zs %>%
    dplyr::filter(site == "NT1") %>%
    tidyr::unnest(zonal_statistics)

  expect_true(nrow(no_data_se) == length(bands))
  expect_true(all(is.na(no_data_se[["date"]])))
  expect_true(all(is.na(no_data_se[["value"]])))
  expect_equal(no_data_se[["band"]], bands)

  se <- se %>%
    dplyr::filter(site == "NT1")

  zs <- se %>%
    get_zonal_statistics(covariate, radius = 100, bands = bands)

  no_data_se <- zs %>%
    tidyr::unnest(zonal_statistics)

  expect_true(nrow(no_data_se) == length(bands))
  expect_true(all(is.na(no_data_se[["date"]])))
  expect_true(all(is.na(no_data_se[["value"]])))
  expect_equal(no_data_se[["band"]], bands)
})

test_that("For 'once' covariates, get_zonal_statistics() gets one single data point, even if data is before the covariate date", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  covariate <- "fifty_reefs_prioritization"
  bands <- 1

  se <- dplyr::tribble(
    ~site, ~latitude, ~longitude, ~sample_date,
    "NT1", -17.3741, 179.4217, "2017-09-09",
    "NF2", -17.37898, 179.41577, "2013-05-13",
    "NAOBT1", -17.622, 178.831, "2019-05-23",
    "DawaC6", -17.6759, 178.61905, "2019-10-16",
    "ME6", -17.31758, 178.16589, "2025-11-05"
  )

  zs <- se %>%
    get_zonal_statistics(covariate, radius = 100, bands = bands)

  expect_equal(
    zs %>%
      dplyr::pull(zonal_statistics) %>%
      purrr::map_dbl(nrow) %>%
      unique(),
    length(bands)
  )

  expect_equal(
    zs %>%
      dplyr::pull(zonal_statistics) %>%
      purrr::map("band") %>%
      unlist() %>%
      unique(),
    bands
  )

  item_dates <- get_collection_items(covariate) %>%
    purrr::map_dfr(\(x) {
      dplyr::tibble(item_date = as.Date(x$properties$datetime))
    })

  expect_equal(
    zs %>%
      dplyr::select(zonal_statistics) %>%
      tidyr::unnest(zonal_statistics) %>%
      dplyr::pull(date) %>%
      unique(),
    item_dates[["item_date"]]
  )
})

test_that("For 'daily' covariates, get_zonal_statistics() gets data for n_days (at most, less is possible if no data)", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  covariate <- "Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)"
  n_days <- 5

  se <- dplyr::tribble(
    ~site, ~latitude, ~longitude, ~sample_date,
    "NT1", -17.3741, 179.4217, "2017-09-09",
    "NF2", -17.37898, 179.41577, "2013-05-13",
    "NAOBT1", -17.622, 178.831, "2019-05-23",
    "DawaC6", -17.6759, 178.61905, "2019-10-16",
    "ME6", -17.31758, 178.16589, "2025-11-05"
  )

  zs <- se %>%
    get_zonal_statistics(covariate, radius = 100, n_days = n_days)

  expect_equal(
    zs %>%
      dplyr::pull(zonal_statistics) %>%
      purrr::map_dbl(nrow) %>%
      unique(),
    n_days
  )
})

test_that("get_zonal_statistics gives the same results when SEs have overlapping intervals as it would when the SEs are on their own, without overlapping", {
  se <- dplyr::tribble(
    ~site, ~sample_date, ~latitude, ~longitude,
    "test", "2024-10-03", 29.364309, 34.961792,
    "test", "2024-10-04", 29.364309, 34.961792,
    "test", "2024-10-20", 29.364309, 34.961792,
    "test", "2024-10-21", 29.364309, 34.961792,
    "test", "2025-01-01", 29.364309, 34.961792
  )

  zs_together <- se %>%
    get_zonal_statistics("Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)", n_days = 30, radius = 100, spatial_stats = "mean") %>%
    dplyr::arrange(sample_date)

  zs_separate <- se %>%
    split(.$sample_date) %>%
    purrr::map_dfr(\(x) x %>%
      get_zonal_statistics("Daily Global 5km Satellite Sea Surface Temperature (CoralTemp)", n_days = 30, radius = 100, spatial_stats = "mean")) %>%
    dplyr::arrange(sample_date)

  expect_identical(
    zs_together,
    zs_separate
  )
})
