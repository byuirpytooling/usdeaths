#' Get Available Years for a Data Type
#'
#' @description
#' Queries the `usdeaths` package data index to identify years for which metadata 
#' objects are available for a specific data category.
#'
#' @param type A character string specifying the data category. 
#'   Must be one of "mortality_multiple", "births", "birth_cohort", or "fetal_deaths".
#'
#' @return An integer vector of sorted years.
#' @export
get_available_years <- function(type = c("mortality_multiple", "births", "birth_cohort", "fetal_deaths")) {
  
  type <- match.arg(type)
  
  # data()$results is the "source of truth" for .rda files in a package
  pkg_data <- as.data.frame(utils::data(package = "usdeaths")$results)
  all_objects <- pkg_data$Item
  
  # Handle singular/plural mismatch for fetal_death vs fetal_deaths
  search_type <- if (type == "fetal_deaths") "fetal_death" else type
  
  # Find objects matching the pattern (e.g., data_births_1968)
  pattern <- paste0("^data_", search_type, "_\\d{4}$")
  matching <- all_objects[stringr::str_detect(all_objects, pattern)]
  
  years <- matching |>
    stringr::str_extract("\\d{4}") |>
    as.integer() |>
    sort()
  
  if (length(years) == 0) {
    cli::cli_abort("No metadata objects found for type {.val {type}}.")
  }
  
  years
}