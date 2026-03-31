#' Get and Compare Available Fields Across Years
#'
#' @description
#' Returns a summary of available fields (columns) for specific data types and years. 
#' If multiple years are provided, it identifies which fields are common across 
#' all years and which are unique to specific ones.
#'
#' @param ... Optional: Explicit metadata objects to inspect.
#' @param type A character string specifying the data category. 
#' @param years An optional integer vector of years to inspect.
#'
#' @return A tibble containing field names, descriptions, and availability notes.
#' @importFrom dplyr filter select mutate group_by summarise n left_join case_when bind_rows distinct if_else any_of
#' @importFrom purrr map2_dfr map_dfr
#' @importFrom stringr str_detect str_extract
#' @importFrom tibble tibble
#' @importFrom utils data
#' @export
get_fields <- function(..., 
                       type = c("mortality_multiple", "births", "birth_cohort", "fetal_deaths"),
                       years = NULL) {
  
  type <- match.arg(type)
  
  if (!is.null(years)) {
    search_type <- if (type == "fetal_deaths") "fetal_death" else type
    
    meta_list <- lapply(years, \(yr) {
      obj_name <- paste0("data_", search_type, "_", yr)
      temp_env <- new.env()
      utils::data(list = obj_name, package = "usdeaths", envir = temp_env)
      get(obj_name, envir = temp_env)
    })
  } else {
    meta_list <- list(...)
  }
  
  year_labels <- if (!is.null(years)) years else seq_along(meta_list)
  
  # Helper to identify "junk" rows
  is_junk <- function(x) stringr::str_detect(x, "(?i)filler|reserved")

  field_presence <- purrr::map2_dfr(meta_list, year_labels, \(m, yr) {
    tibble::tibble(name = m$name, year = as.character(yr))
  }) |> 
    dplyr::filter(!is_junk(name))

  # Base result with filter for junk names
  result <- meta_list[[1]] |>
    dplyr::filter(!is_junk(name)) |>
    dplyr::select(dplyr::any_of(c("name", "description", "codes")))
  
  all_names <- unique(field_presence$name)
  missing_from_first <- setdiff(all_names, result$name)
  
  if (length(missing_from_first) > 0) {
    extra <- purrr::map_dfr(meta_list[-1], \(m) {
      m |>
        dplyr::filter(name %in% missing_from_first, !is_junk(name)) |>
        dplyr::select(dplyr::any_of(c("name", "description", "codes")))
    }) |>
      dplyr::distinct(name, .keep_all = TRUE)
    
    result <- dplyr::bind_rows(result, extra)
  }
  
  year_coverage <- field_presence |>
    dplyr::group_by(name) |>
    dplyr::summarise(
      available_years = paste(year, collapse = ", "),
      in_all_years    = dplyr::n() == length(meta_list),
      .groups = "drop"
    )
  
  result <- result |>
    dplyr::left_join(year_coverage, by = "name") |>
    dplyr::mutate(
      has_codes = dplyr::if_else(nzchar(as.character(codes)), "yes", ""),
      note = dplyr::case_when(
        length(meta_list) == 1 ~ "",
        !in_all_years          ~ paste0("only in: ", available_years),
        TRUE                   ~ ""
      )
    ) |>
    dplyr::select(name, description, has_codes, note)
  
  print(result, n = Inf)
  invisible(result)
}