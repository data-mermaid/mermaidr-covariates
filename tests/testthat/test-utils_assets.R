test_that("check_inputs_covariate_data errors when covariate is date-dependent and date_col is NULL", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(
    check_inputs_covariate_data(get_collection_items("lulc"), covariate = "lulc", date_col = NULL),
    "date-dependent. Please supply a date column"
  )
})

test_that("check_inputs_covariate_data does not error when covariate is not date-dependent and date_col is NULL", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_no_error(
    check_inputs_covariate_data(
      get_collection_items("meow_boundaries"),
      "meow_boundaries",
      col = "ECO_CODE",
      date_col = NULL
    )
  )
})

test_that("check_inputs_covariate_data errors when there is more than one asset and `dataset` is not specified", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(
    check_inputs_covariate_data(get_collection_items("gpw_sediment_exposure"), covariate = "gpw_sediment_exposure"),
    "contains more than one dataset"
  )
})

test_that("check_inputs_covariate_data errors when more than one dataset is specified", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(
    check_inputs_covariate_data(get_collection_items("gpw_sediment_exposure"),
      covariate = "gpw_sediment_exposure",
      dataset = c("one", "two")
    ),
    "one dataset"
  )
})

test_that("check_inputs_covariate_data errors when an invalid dataset is given", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(
    check_inputs_covariate_data(get_collection_items("gpw_sediment_exposure"), covariate = "gpw_sediment_exposure", dataset = "invalid"),
    "does not exist"
  )
})

test_that("check_inputs_covariate_data errors when an invalid col/band is given", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_error(
    check_inputs_covariate_data(get_collection_items("gpw_sediment_exposure"),
      covariate = "gpw_sediment_exposure", dataset = "cog", col = 45
    ),
    "may specify by band number OR by name"
  )

  expect_error(
    check_inputs_covariate_data(get_collection_items("gpw_sediment_exposure"),
      covariate = "gpw_sediment_exposure", dataset = "cog", col = "test"
    ),
    "may specify by band number or by name"
  )

  expect_error(
    check_inputs_covariate_data(get_collection_items("gpw_sediment_exposure"),
      covariate = "gpw_sediment_exposure", dataset = "cog", col = "25"
    ),
    "may specify by band number or by name"
  )

  expect_error(
    check_inputs_covariate_data(
      get_collection_items("meow_boundaries"),
      "meow_boundaries",
      col = "test"
    ),
    "is not valid"
  )
})

test_that("`col` can be a number or a name when it refers to a band in a raster data set", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_no_error(check_inputs_covariate_data(get_collection_items("gpw_sediment_exposure"),
    covariate = "gpw_sediment_exposure", dataset = "cog", col = 1
  ))

  expect_no_error(check_inputs_covariate_data(get_collection_items("gpw_sediment_exposure"),
    covariate = "gpw_sediment_exposure", dataset = "cog", col = "1"
  ))

  expect_no_error(check_inputs_covariate_data(get_collection_items("gpw_sediment_exposure"),
    covariate = "gpw_sediment_exposure", dataset = "cog", col = "total_sed_expos"
  ))
})

test_that("check_inputs_covariate_data returns asset_type and bands_cols when all inputs pass", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  # Multiple datasets, band by number -> includes band, in numeric, too
  expect_equal(
    check_inputs_covariate_data(get_collection_items("gpw_sediment_exposure"),
      covariate = "gpw_sediment_exposure", dataset = "cog", col = 1
    ),
    list(
      type = "cog", url = "https://d2uu99zl9amnvy.cloudfront.net/assets/gpw_sediment_exposure/gpw_sediment_exposure_2020/data_sediment_exposure_2020.tif",
      bands_cols = structure(list(band = 1:9, name = c(
        "total_sed_expos",
        "w1_watershed_id", "w2_watershed_id", "w3_watershed_id",
        "w1_percent", "w2_percent", "w3_percent", "leading_country_id",
        "leading_realm_id"
      )), row.names = c(NA, -9L), class = c(
        "tbl_df",
        "tbl", "data.frame"
      )), band = 1
    )
  )

  # Works with numeric col specified in quotes too
  # Multiple datasets, band by number -> includes band, in numeric, too
  expect_equal(
    check_inputs_covariate_data(get_collection_items("gpw_sediment_exposure"),
      covariate = "gpw_sediment_exposure", dataset = "cog", col = "1"
    ),
    list(
      type = "cog", url = "https://d2uu99zl9amnvy.cloudfront.net/assets/gpw_sediment_exposure/gpw_sediment_exposure_2020/data_sediment_exposure_2020.tif",
      bands_cols = structure(list(band = 1:9, name = c(
        "total_sed_expos",
        "w1_watershed_id", "w2_watershed_id", "w3_watershed_id",
        "w1_percent", "w2_percent", "w3_percent", "leading_country_id",
        "leading_realm_id"
      )), row.names = c(NA, -9L), class = c(
        "tbl_df",
        "tbl", "data.frame"
      )), band = 1
    )
  )

  # Multiple datasets, band by name -> includes band, in numeric, too
  expect_equal(
    check_inputs_covariate_data(get_collection_items("gpw_sediment_exposure"),
      covariate = "gpw_sediment_exposure", dataset = "cog", col = "total_sed_expos"
    ),
    list(
      type = "cog", url = "https://d2uu99zl9amnvy.cloudfront.net/assets/gpw_sediment_exposure/gpw_sediment_exposure_2020/data_sediment_exposure_2020.tif",
      bands_cols = structure(list(band = 1:9, name = c(
        "total_sed_expos",
        "w1_watershed_id", "w2_watershed_id", "w3_watershed_id",
        "w1_percent", "w2_percent", "w3_percent", "leading_country_id",
        "leading_realm_id"
      )), row.names = c(NA, -9L), class = c(
        "tbl_df",
        "tbl", "data.frame"
      )), band = 1L
    )
  )

  # Single data set, band not named (but only one band) -> includes band in numeric
  expect_equal(
    check_inputs_covariate_data(get_collection_items("50b810fb-5f17-4cdb-b34b-c377837e2a29"), "50b810fb-5f17-4cdb-b34b-c377837e2a29"),
    list(
      type = "cog", url = "https://d2uu99zl9amnvy.cloudfront.net/assets/50b810fb-5f17-4cdb-b34b-c377837e2a29/c201cb85-c057-402d-905d-0df3a5e6141b/data_analysed_sst.tif",
      bands_cols = structure(list(band = 1, name = NA_character_), class = c(
        "tbl_df",
        "tbl", "data.frame"
      ), row.names = c(NA, -1L)), band = 1
    )
  )

  # Multiple cols -> single dataset, does not include "band" because parquet
  expect_equal(
    check_inputs_covariate_data(
      get_collection_items("meow_boundaries"),
      "meow_boundaries",
      col = "ECO_CODE"
    ),
    list(
      type = "parquet", url = "https://d2uu99zl9amnvy.cloudfront.net/assets/meow_boundaries/meow_boundaries/data_meow.parquet",
      bands_cols = structure(list(name = c(
        "ECO_CODE", "ECOREGION",
        "PROV_CODE", "PROVINCE", "RLM_CODE", "REALM", "ALT_CODE",
        "ECO_CODE_X", "Shape_Leng", "Lat_Zone", "ORIG_FID", "Shape_Le_1",
        "Shape_Area", "geometry"
      )), row.names = c(NA, -14L), class = c(
        "tbl_df",
        "tbl", "data.frame"
      ))
    )
  )
})
