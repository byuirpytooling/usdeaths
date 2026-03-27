# data-raw/build_icd_codes.R
# This script builds the comprehensive lookup tables for the usdeaths package.

library(usethis)
library(dplyr)
library(stringr)

# Download plain-text recode table from URL.
download_recode_text <- function(url) {
  tmp <- tempfile(fileext = ".txt")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  readLines(tmp, warn = FALSE, encoding = "latin1")
}

# Parse "001 = Label (codes)" style recode lines.
parse_recode_text <- function(lines, max_code) {
  lines <- iconv(lines, from = "latin1", to = "UTF-8", sub = " ")
  lines <- gsub("\u2013|\u2014", "-", lines, perl = TRUE)
  lines <- stringr::str_squish(lines)
  lines <- lines[nzchar(lines)]

  entries <- character()
  current <- NULL
  for (line in lines) {
    if (grepl("^[0-9]{3}\\s*=", line)) {
      if (!is.null(current)) entries <- c(entries, current)
      current <- line
    } else if (!is.null(current)) {
      current <- paste(current, line)
    }
  }
  if (!is.null(current)) entries <- c(entries, current)

  tibble::tibble(raw = entries) %>%
    dplyr::mutate(
      code = stringr::str_match(raw, "^([0-9]{3})\\s*=")[, 2],
      description = stringr::str_match(raw, "^[0-9]{3}\\s*=\\s*(.*)$")[, 2]
    ) %>%
    dplyr::filter(!is.na(code)) %>%
    dplyr::mutate(
      code_num = as.integer(code),
      description = stringr::str_squish(description),
      description = stringr::str_remove(description, "\\s*\\([^\\)]*\\)\\s*$")
    ) %>%
    dplyr::filter(code_num <= max_code) %>%
    dplyr::arrange(code_num) %>%
    dplyr::distinct(code, .keep_all = TRUE) %>%
    dplyr::select(code, description)
}

# Build a full code range and apply known labels as overrides.
expand_recode <- function(known, min_code, max_code, unknown_prefix) {
  full_codes <- tibble::tibble(
    code = stringr::str_pad(
      as.character(seq.int(min_code, max_code)),
      width = 3,
      pad = "0"
    )
  )

  full_codes %>%
    dplyr::left_join(known, by = "code") %>%
    dplyr::mutate(
      description = dplyr::coalesce(
        description,
        paste0(unknown_prefix, " ", code)
      )
    )
}

# --- 1. ICD-10-CM (2026 Master List) ----------------------------------------
icd10_url  <- "https://ftp.cdc.gov/pub/health_statistics/nchs/publications/ICD10CM/2026/icd10cm-Code%20Descriptions-2026.zip"
icd10_tmp  <- tempfile(fileext = ".zip")

message("Downloading ICD-10-CM 2026 Master List...")
download.file(icd10_url, icd10_tmp, mode = "wb", quiet = TRUE)

icd10_dir  <- file.path(tempdir(), "icd10_build")
if (dir.exists(icd10_dir)) unlink(icd10_dir, recursive = TRUE)
dir.create(icd10_dir)
unzip(icd10_tmp, exdir = icd10_dir)

all_files  <- list.files(icd10_dir, recursive = TRUE, full.names = TRUE, pattern = "\\.txt$")
file_info  <- file.info(all_files)
icd10_file <- rownames(file_info)[which.max(file_info$size)]

icd10_codes <- data.frame(
  code = gsub("\\.", "", str_trim(substr(readLines(icd10_file, encoding = "latin1"), 7, 13))),
  description = str_trim(substr(readLines(icd10_file, encoding = "latin1"), 78, 500)),
  stringsAsFactors = FALSE
) %>% 
  filter(nchar(code) > 0) %>%
  distinct(code, .keep_all = TRUE)

# --- 2. Infant Cause Recode 130 (ICD-10 Era: 1999-Present) ------------------
infant_130_url <- "https://resdac.org/sites/datadocumentation.resdac.org/files/130%20ICD-10%20Cause%20of%20Death%20Recodes%20Code%20Table%20%28MBSF-NDI%29.txt"
infant_cause_130 <- parse_recode_text(
  lines = download_recode_text(infant_130_url),
  max_code = 158
)
infant_cause_130 <- expand_recode(
  known = infant_cause_130,
  min_code = 1,
  max_code = 158,
  unknown_prefix = "Infant cause recode"
)

# --- 3. General Cause Recode 113 (ICD-10 Era: 1999-Present) -----------------
cause_113_url <- "https://resdac.org/sites/datadocumentation.resdac.org/files/113%20ICD-10%20Cause%20of%20Death%20Recodes%20Code%20Table%20%28MBSF-NDI%29.txt"
cause_113 <- parse_recode_text(
  lines = download_recode_text(cause_113_url),
  max_code = 135
)
cause_113 <- expand_recode(
  known = cause_113,
  min_code = 1,
  max_code = 135,
  unknown_prefix = "Cause recode"
)

# --- 4. Vintage Recodes (ICD-9 Era: 1979-1998) ------------------------------
infant_cause_61 <- tibble::tribble(
  ~code, ~description,
  "010", "Infectious and parasitic diseases",
  "020", "Septicemia",
  "160", "Anencephaly and similar anomalies",
  "170", "Spina bifida",
  "200", "Anomalies of the heart",
  "310", "Respiratory distress syndrome",
  "380", "Short gestation and low birth weight",
  "450", "Sudden Infant Death Syndrome (SIDS)",
  "610", "All other causes"
)
infant_cause_61 <- expand_recode(
  known = infant_cause_61,
  min_code = 10,
  max_code = 610,
  unknown_prefix = "Infant cause recode (ICD-9 era)"
)

cause_72 <- tibble::tribble(
  ~code, ~description,
  "010", "Shigellosis and amebiasis",
  "020", "Certain other intestinal infections",
  "030", "Tuberculosis",
  "040", "Tuberculosis of respiratory system",
  "050", "Other tuberculosis",
  "060", "Whooping cough",
  "070", "Streptococcal sore throat, scarlatina, and erysipelas",
  "080", "Meningococcal infection",
  "090", "Septicemia",
  "100", "Acute poliomyelitis",
  "110", "Measles",
  "120", "Viral hepatitis",
  "130", "Syphilis",
  "140", "All other infectious and parasitic diseases",
  "150", "Malignant neoplasms, including neoplasms of lymphatic and hematopoietic tissues",
  "160", "Malignant neoplasms of lip, oral cavity, and pharynx",
  "170", "Malignant neoplasms of digestive organs and peritoneum",
  "180", "Malignant neoplasms of respiratory and intrathoracic organs",
  "190", "Malignant neoplasm of breast",
  "200", "Malignant neoplasms of genital organs",
  "210", "Malignant neoplasms of urinary organs",
  "220", "Malignant neoplasms of all other and unspecified sites",
  "230", "Leukemia",
  "240", "Other malignant neoplasms of lymphatic and hematopoietic tissues",
  "250", "Benign neoplasms, carcinoma in situ, and neoplasms of uncertain behavior and of unspecified nature",
  "260", "Diabetes mellitus",
  "270", "Nutritional deficiencies",
  "280", "Anemias",
  "290", "Meningitis",
  "300", "Major cardiovascular diseases",
  "310", "Diseases of heart",
  "320", "Rheumatic fever and rheumatic heart disease",
  "330", "Hypertensive heart disease",
  "340", "Hypertensive heart and renal disease",
  "350", "Ischemic heart disease",
  "360", "Acute myocardial infarction",
  "370", "Other acute and subacute forms of ischemic heart disease",
  "380", "Angina pectoris",
  "390", "Old myocardial infarction and other forms of chronic ischemic heart disease",
  "400", "Other diseases of endocardium",
  "410", "All other forms of heart disease",
  "420", "Hypertension with or without renal disease",
  "430", "Cerebrovascular diseases",
  "440", "Intracerebral and other intracranial hemorrhage",
  "450", "Cerebral thrombosis and unspecified occlusion of cerebral arteries",
  "460", "Cerebral embolism",
  "470", "All other and late effects of cerebrovascular diseases",
  "480", "Atherosclerosis",
  "490", "Other diseases of arteries, arterioles, and capillaries",
  "500", "Acute bronchitis and bronchiolitis",
  "510", "Pneumonia and influenza",
  "520", "Pneumonia",
  "530", "Influenza",
  "540", "Chronic obstructive pulmonary diseases and allied conditions",
  "550", "Bronchitis, chronic and unspecified",
  "560", "Emphysema",
  "570", "Asthma",
  "580", "Other chronic obstructive pulmonary diseases and allied conditions",
  "590", "Ulcer of stomach and duodenum",
  "600", "Appendicitis",
  "610", "Hernia of abdominal cavity and intestinal obstruction without mention of hernia",
  "620", "Chronic liver disease and cirrhosis",
  "630", "Cholelithiasis and other disorders of gallbladder",
  "640", "Nephritis, nephrotic syndrome, and nephrosis",
  "650", "Acute glomerulonephritis and nephrotic syndrome",
  "660", "Chronic glomerulonephritis, nephritis and nephropathy not specified as acute or chronic, and renal sclerosis unspecified",
  "670", "Renal failure, disorders resulting from impaired renal function, and small kidney of unknown cause",
  "680", "Infections of kidney",
  "690", "Hyperplasia of prostate",
  "700", "Complications of pregnancy, childbirth, and the puerperium",
  "710", "Pregnancy with abortive outcome",
  "720", "Other complications of pregnancy, childbirth, and the puerperium",
  "730", "Congenital anomalies",
  "740", "Certain conditions originating in the perinatal period",
  "750", "Birth trauma, intrauterine hypoxia, birth asphyxia, and respiratory distress syndrome",
  "760", "Other conditions originating in the perinatal period",
  "770", "Symptoms, signs, and ill-defined conditions",
  "780", "All other diseases (Residual)",
  "790", "Accidents and adverse effects",
  "800", "Motor vehicle accidents",
  "810", "All other accidents and adverse effects",
  "820", "Suicide",
  "830", "Homicide and legal intervention",
  "840", "All other external causes"
)
cause_72 <- expand_recode(
  known = cause_72,
  min_code = 10,
  max_code = 840,
  unknown_prefix = "Cause recode (ICD-9 era)"
)

# Fallback ICD-9 lookup for raw ICD-era joins (legacy years)
# This is a coarse map built from the package's ICD-9 recode dictionaries.
icd9_codes <- dplyr::bind_rows(infant_cause_61, cause_72) %>%
  dplyr::distinct(code, .keep_all = TRUE)

# --- 5. Save Internal Data --------------------------------------------------
usethis::use_data(
  icd10_codes, infant_cause_130, cause_113,
  infant_cause_61, cause_72, icd9_codes,
  overwrite = TRUE, internal = TRUE
)

message("SUCCESS: All 4 major NCHS recode lists and ICD-10 master built.")