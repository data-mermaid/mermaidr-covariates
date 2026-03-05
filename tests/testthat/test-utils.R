test_that("check_covariate_id returns TRUE when x is an ID, and FALSE when it is not", {
  expect_true(check_covariate_id("ea07abba-06cf-41a8-92a3-b20eaf801ea9"))

  expect_false(check_covariate_id("test"))
})

test_that("lookup_covariate_id_by_name returns the ID when it is a valid name,
          and NA when it is not", {
  expect_identical(
    lookup_covariate_id_by_name("Land Use and Land Cover (LULC) Collection"),
    "ea07abba-06cf-41a8-92a3-b20eaf801ea9"
  )

  expect_true(is.na(lookup_covariate_id_by_name("test")))
})

test_that("get_covariate_id returns the ID when passed an ID, the ID when
          passed a valid name, and errors when passed an invalid ID/name", {
  expect_identical(
    get_covariate_id("ea07abba-06cf-41a8-92a3-b20eaf801ea9"),
    "ea07abba-06cf-41a8-92a3-b20eaf801ea9"
  )

  expect_identical(
    get_covariate_id("Land Use and Land Cover (LULC) Collection"),
    "ea07abba-06cf-41a8-92a3-b20eaf801ea9"
  )

  expect_error(get_covariate_id("test"))
})
