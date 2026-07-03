#' Covariate helper
#'
#' Tells you what function to use to get covariates data! Also returns information on datasets, bands or columns, and more.
#'
#' @param covariate Covariate name or ID
#'
#' @export
#'
#' @examples
#' # covariate_helper("Daily Sea Surface Temperature")
#' # covariate_helper("GPW MEOW Realms")
covariate_helper <- function(covariate) {
  covariate <- get_covariate_id(covariate)
  covariate_name <- get_covariate_name_from_id(covariate)

  type <- get_collection_type(covariate)

  format_msg <- ifelse(length(type) == 1,
    glue::glue("Covariate contains *{type}* data."),
    glue::glue("Covariate contains both *{type}* data.")
  )

  datasets <- list_datasets_for_covariate(covariate)

  fxn <- get_fxn_from_type(type)

  fxn_msg <- glue::glue("Use {fxn}.")

  # Go through each "type" the data has (even if just one), and report on datasets/bands
  original_type <- type
  if (type == "raster + vector") {
    type <- c("raster", "vector")
  }

  datasets_bands_cols_msg <- purrr::map(
    type,
    \(type) {
      relevant_datasets <- switch(type,
        "raster" = get_covariate_cog_datasets(covariate),
        "vector" = get_covariate_parquet_datasets(covariate)
      )

      datasets_msg <- ifelse(length(relevant_datasets) == 1,
        "There is only one dataset, so you do not need to specify.", glue::glue("You need to specify the dataset. Dataset options: {comma_sep_quoted(relevant_datasets)}.")
      )
      relevant_datasets <- datasets[relevant_datasets]

      # Go through datasets and report on bands/cols
      if (type == "raster") {
        bands_cols_msg <- purrr::map_chr(
          relevant_datasets,
          \(bands) {
            if (nrow(bands) == 1) {
              glue::glue("There is only one band of data, so you do not need to specify.")
            } else {
              named_bands <- !all(is.na(bands[["name"]]))
              if (named_bands) {
                bands <- paste(capture.output(print(bands)), collapse = "\n")
                glue::glue("You must specify band(s), either by number or name.\nBand options:\n{bands}")
              } else {
                glue::glue("You must specify band(s). Options: {paste0(bands[['band']], collapse = ', ')}")
              }
            }
          }
        )
      } else if (type == "vector") {
        bands_cols_msg <- purrr::map(
          relevant_datasets,
          \(columns) {
            glue::glue("By default, all columns will be returned. You can limit to specific colunmns in `columns` argument.\nColumn options: {comma_sep_quoted(columns[['name']])}")
          }
        )
      }

      if (length(bands_cols_msg) > 1) {
        dataset_msg <- purrr::map(names(relevant_datasets), \(dataset) glue::glue('For dataset "{dataset}":'))
        bands_cols_msg <- purrr::map2(
          dataset_msg, bands_cols_msg,
          \(dataset, msg) {
            glue::glue("\n\n{dataset}\n{msg}")
          }
        ) %>%
          paste0(collapse = "\n")
      }

      glue::glue("{datasets_msg}\n{bands_cols_msg}")
    }
  )

  if (length(datasets_bands_cols_msg) > 1) {
    type_msg <- purrr::map(type, \(type) glue::glue("Using {get_fxn_from_type(type)}:"))
    datasets_bands_cols_msg <- purrr::map2(
      type_msg, datasets_bands_cols_msg,
      \(type, msg) {
        glue::glue("\n\n{type}\n{msg}")
      }
    ) %>%
      paste0(collapse = "\n")
  }

  msg <- glue::glue("{format_msg} {fxn_msg}\n{datasets_bands_cols_msg}")

  usethis::ui_info(msg)

  res <- list(
    type = original_type,
    "function" = fxn,
    datasets = datasets
  )

  invisible(res)
}

get_fxn_from_type <- function(type) {
  switch(type,
    "raster" = "get_zonal_statistics()",
    "vector" = "attach_covariates()",
    "raster + vector" = "get_zonal_statistics() OR attach_covariates(), depending on the dataset"
  )
}
