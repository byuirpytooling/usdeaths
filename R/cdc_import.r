#' Import and decode a CDC vital statistics dataset
#'
#' A full pipeline wrapper that downloads, reads, and decodes a CDC vital
#' statistics dataset in a single call. Automatically resolves the download
#' URL and metadata for the given section and year.
#'
#' @param section A string identifying the data section,
#'   e.g. \code{"mortality_multiple"}.
#' @param year An integer or numeric year, e.g. \code{1969}.
#'
#' @return A fully decoded tibble with all columns in metadata order and
#'   coded values replaced by human-readable labels.
#'
#' @examples
#' \dontrun{
#' mort1969 <- cdc_import("mortality_multiple", 1969)
#' }
#'
#' @export
cdc_import <- function(section, year) {
    url <- get_cdc_url(section, year)
    meta_name <- paste("data", section, year, sep = "_")
    if (!exists(meta_name)) {
        stop(
            "No metadata found for section '", section, "' and year ", year,
            ". Expected object: ", meta_name
        )
    }

    meta <- get(meta_name)
    temp <- download_cdc(url)
    unzipped <- load_data(temp, meta)
    message("Decoding ", nrow(unzipped), " rows...")
    decoded <- decode_all(unzipped, meta)
    .enrich_causes(decoded, meta, year = year)
}
