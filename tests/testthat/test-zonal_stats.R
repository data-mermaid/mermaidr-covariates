test_that("get_zonal_stats returns error with code and message", {
  test_url <- "https://covariates-data.s3.us-east-1.amazonaws.com/noaa-monthly-max-dhw/dhw_2018_10/dhw_2018_10.tif"

  expect_error(
    get_zonal_stats(177, -17,
      test_url,
      buffer = 10,
      bands = list(2),
      stats = "min"
    ),
    "400 Bad Request"
  )
})
