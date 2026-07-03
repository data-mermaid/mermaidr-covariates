test_that("list_datasets_for_covariate returns a list with the cols or bands in a tibble", {
  skip_if_offline()
  skip_on_ci()
  skip_on_cran()

  covariates <- list_covariates()

  purrr::walk(
    covariates[["id"]],
    \(covariate) {
      datasets <- list_datasets_for_covariate(covariate)
      expect_true(is.list(datasets))
      purrr::walk(
        datasets,
        \(x)
        expect_true(is.data.frame(x))
      )
    }
  )
})
