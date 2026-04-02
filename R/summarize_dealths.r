#' Summarize deaths by one or more grouping columns
#'
#' Groups a CDC mortality dataset by the specified column(s) and returns a
#' tibble with death counts for each group, sorted in descending order.
#' Optionally accepts multiple named datasets to compare across years.
#'
#' @param ... Either a single data frame, or multiple named data frames
#'   e.g. \code{mort1969 = mort1969, mort1970 = mort1970}.
#' @param by A character vector of column names to group by,
#'   e.g. \code{"sex"} or \code{c("sex", "race_recode3")}.
#'
#' @return A tibble with the grouping columns, a \code{year} column (when
#'   multiple datasets are provided), and a \code{n} column containing death
#'   counts, sorted descending by \code{n}.
#'
#' @examples
#' \dontrun{
#' # Single year
#' summarize_deaths(mort1969, by = "sex")
#'
#' # Multiple years
#' summarize_deaths(mort1969 = mort1969, mort1970 = mort1970, by = "sex")
#' }
#'
#' @export
summarize_deaths <- function(..., by) {
    datasets <- list(...)

    if (length(datasets) == 0) {
        stop("At least one data frame must be provided.")
    }

    # Single dataset — original behavior
    if (length(datasets) == 1) {
        df <- datasets[[1]]
        missing_cols <- setdiff(by, colnames(df))
        if (length(missing_cols) > 0) {
            stop(
                "The following columns were not found in the data: ",
                paste(missing_cols, collapse = ", ")
            )
        }

        return(
            df |>
                dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
                dplyr::arrange(dplyr::desc(n))
        )
    }

    # Multiple datasets — compare across years
    if (is.null(names(datasets)) || any(names(datasets) == "")) {
        stop("When providing multiple datasets, all must be named, e.g. mort1969 = mort1969.")
    }

    combined <- purrr::map_dfr(names(datasets), function(nm) {
        df <- datasets[[nm]]

        missing_cols <- setdiff(by, colnames(df))
        if (length(missing_cols) > 0) {
            stop(
                "The following columns were not found in '", nm, "': ",
                paste(missing_cols, collapse = ", ")
            )
        }

        df |>
            dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
            dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
            dplyr::mutate(year = nm) |>
            dplyr::relocate(year)
    })

    combined |> dplyr::arrange(year, dplyr::desc(n))
}