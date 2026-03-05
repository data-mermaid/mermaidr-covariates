test_that("check_covariate_id returns TRUE when x is an ID, and FALSE when it is not", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_true(check_covariate_id("50b810fb-5f17-4cdb-b34b-c377837e2a29"))

  expect_false(check_covariate_id("test"))
})

test_that("lookup_covariate_id_by_name returns the ID when it is a valid name,
          and NA when it is not", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_identical(
    lookup_covariate_id_by_name("Daily Sea Surface Temperature"),
    "50b810fb-5f17-4cdb-b34b-c377837e2a29"
  )

  expect_true(is.na(lookup_covariate_id_by_name("test")))
})

test_that("get_covariate_id returns the ID when passed an ID, the ID when
          passed a valid name, and errors when passed an invalid ID/name", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  expect_identical(
    get_covariate_id("50b810fb-5f17-4cdb-b34b-c377837e2a29"),
    "50b810fb-5f17-4cdb-b34b-c377837e2a29"
  )

  expect_identical(
    get_covariate_id("Daily Sea Surface Temperature"),
    "50b810fb-5f17-4cdb-b34b-c377837e2a29"
  )

  expect_error(get_covariate_id("test"))
})
