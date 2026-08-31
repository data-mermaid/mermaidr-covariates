test_that("attach_covariate_data only returns `columns` cols if not NULL, all cols otherwise", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  se <- dplyr::tribble(
    ~site, ~latitude, ~longitude, ~sample_date,
     "AA", -17.97855,   179.2251, "2008-11-25"
    )

  col_named <- attach_covariate_data(se, "meow_boundaries", columns = "REALM")

  expect_named(col_named, c(names(se), "REALM"))

  all_col <- attach_covariate_data(se, "meow_boundaries")

  expect_named(all_col, c(
    names(se), "ECO_CODE", "ECOREGION", "PROV_CODE", "PROVINCE", "RLM_CODE",
    "REALM", "ALT_CODE", "ECO_CODE_X", "Shape_Leng", "Lat_Zone",
    "ORIG_FID", "Shape_Le_1", "Shape_Area"
  ))
})

test_that("attach_covariate_data accepts multiple `columns`", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  se <- dplyr::tribble(
    ~site, ~latitude, ~longitude, ~sample_date,
     "AA", -17.97855,   179.2251, "2008-11-25"
    )

  col_named <- attach_covariate_data(se, "meow_boundaries", columns = c("REALM", "ECOREGION"))
})

test_that("attach_covariate_data errors when columns are invalid -- works for one invalid, one (among multiple) invalid, multiple invalid", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  se <- dplyr::tribble(
    ~site, ~latitude, ~longitude, ~sample_date,
     "AA", -17.97855,   179.2251, "2008-11-25"
    )

  expect_error(attach_covariate_data(se, "meow_boundaries", columns = "REAL"), "is not valid")

  expect_error(attach_covariate_data(se, "meow_boundaries", columns = c("REALM", "ecoregion")), "is not valid")

  expect_error(attach_covariate_data(se, "meow_boundaries", columns = c("REAL", "ecoregion")), "are not valid")

  expect_error(attach_covariate_data(se, "meow_boundaries", columns = c("REALM", "ecoregion", "test")), "are not valid")
})
