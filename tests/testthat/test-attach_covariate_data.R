test_that("attach_covariate_data only returns `columns` cols if not NULL, all cols otherwise", {
  se <- mermaidr::mermaid_get_summary_sampleevents(limit = 1)

  col_named <- attach_covariate_data(se, "meow_boundaries", columns = "REALM")

  expect_named(col_named, c(names(se), "REALM"))

  all_col <- attach_covariate_data(se, "meow_boundaries")

  expect_named(all_col, c(
    names(se), "ECO_CODE", "ECOREGION", "PROV_CODE", "PROVINCE", "RLM_CODE",
    "REALM", "ALT_CODE", "ECO_CODE_X", "Shape_Leng", "Lat_Zone",
    "ORIG_FID", "Shape_Le_1", "Shape_Area"
  ))
})
