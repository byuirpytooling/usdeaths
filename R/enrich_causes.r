# Internal function: Enrich Cause of Death Descriptions
# This is called automatically during the import process and is NOT exported.

.enrich_causes <- function(df, meta = NULL, year = NULL) {
  # 1. Detect the best available column (Hierarchy: 130 -> 113 -> 61 -> 72 -> Raw)
  cause_col <- .detect_cause_col(df)
  
  if (is.null(cause_col)) {
    return(df) # Return silently if no match is found
  }

  # 2. Determine revision based on year (single-year datasets; cdc_import passes year explicitly)
  years <- NULL
  if (!is.null(year)) {
    years <- stats::na.omit(c(year))
  } else if ("year" %in% names(df)) {
    years <- unique(stats::na.omit(df$year))
  } else if ("current_data_year" %in% names(df)) {
    years <- unique(stats::na.omit(df$current_data_year))
  }
  if (length(years) == 0) {
    stop("`.enrich_causes()` could not determine year from `year`, `current_data_year`, or argument.")
  }
  revision <- if (years[1] >= 1999) "icd10" else "icd9"
  
  # 3. Retrieve the specific lookup table from package internal data
  lookup_tab <- .get_lookup(revision, cause_col)
  
  # 4. Standardize for join (3-digit padding)
  # lookup_tab is internal data, so we access it directly
  lookup_tab$code <- stringr::str_pad(as.character(lookup_tab$code), 3, pad = "0")
  df[[cause_col]]  <- stringr::str_pad(as.character(df[[cause_col]]), 3, pad = "0")
  
  # 5. Execute Join
  df %>%
    dplyr::left_join(lookup_tab, by = stats::setNames("code", cause_col)) %>%
    dplyr::rename(cause_description = description) %>%
    dplyr::mutate(cause_description = tidyr::replace_na(cause_description, "Other/Unknown"))
}

# --- Helper Logic for .enrich_causes ---

.detect_cause_col <- function(df) {
  candidates <- c(
    "ucodr130", "cause_recode_130",      # ICD-10 Infant
    "ucodr113", "cause_recode_113",      # ICD-10 General
    "cause_recode_61_infant", "icr61",   # ICD-9 Infant
    "cause_recode_72", "ucodr72",        # ICD-9 General
    "icd_code", "ucod"                   # Raw Fallback
  )
  
  match <- candidates[candidates %in% names(df)]
  if (length(match) == 0) return(NULL)
  return(match[1]) 
}

.get_lookup <- function(revision, cause_col) {
  if (grepl("130", cause_col)) return(infant_cause_130)
  if (grepl("113", cause_col)) return(cause_113)
  if (grepl("61", cause_col))  return(infant_cause_61)
  if (grepl("72", cause_col))  return(cause_72)
  
  if (revision == "icd10") return(icd10_codes)
  return(icd9_codes)
}