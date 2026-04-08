test_that("search_covariates() allows searching by title, is not case sensitive", {
  res <- search_covariates("MEOW")

  expect_true(all(stringr::str_detect(tolower(res$title), "meow")))
})

test_that("search_covariates() allows searching by description, is not case sensitive", {
  res <- search_covariates(description = "Alan Coral Atlas")

  expect_true(all(stringr::str_detect(tolower(res$description), "alan coral atlas")))
})

test_that("search_covariates() allows searching by title AND description, is not case sensitive", {
  res <- search_covariates("MEOW", "Alan Coral Atlas")

  expect_true(nrow(res) == 0)

  res <- search_covariates("MEOW", "data")

  expect_true(all(stringr::str_detect(tolower(res$title), "meow")))
  expect_true(all(stringr::str_detect(tolower(res$description), "data")))
})

test_that("without title or description, search_covariates() is identical to list_covariates()", {
  s <- search_covariates()
  l <- list_covariates()

  expect_identical(s, l)
})

test_that("search_covariates() argument `as_data_frame` works", {
  res <- search_covariates("MEOW", as_data_frame = FALSE)

  expect_true(all(purrr::map_lgl(res, \(x) "covariate" %in% class(x))))
})
