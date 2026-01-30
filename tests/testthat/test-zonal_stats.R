# test_that("get_zonal_stats returns error with code and message", {
#   test_url <- "https://covariates-data.s3.us-east-1.amazonaws.com/noaa-monthly-max-dhw/dhw_2018_10/dhw_2018_10.tif"
#
#   expect_error(
#     get_zonal_stats(177, -17,
#       test_url,
#       buffer = 10,
#       bands = list(2),
#       stats = "min"
#     ),
#     "400 Bad Request"
#   )
# })

test_that("get_zonal_statistics allows using covariate name or ID", {
  se <- mermaidr::mermaid_get_project_data("4d23d2a1-774f-4ccf-b567-69f95e4ff572", "fishbelt", "sampleevents", limit = 1)

  zs_name <- get_zonal_statistics(se, "Daily Sea Surface Temperature",
    n_days = 1,
    buffer = 10, stats = "mean"
  )
  zs_id <- get_zonal_statistics(se, "50b810fb-5f17-4cdb-b34b-c377837e2a29",
    n_days = 1,
    buffer = 10, stats = "mean"
  )

  expect_identical(zs_name, zs_id)
})
