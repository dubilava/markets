#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(stringr)
})

raw_dir <- "data/raw"
processed_dir <- "data/processed"
output_long <- file.path(processed_dir, "pink_sheet_indices.csv")
output_wide <- file.path(processed_dir, "pink_sheet_indices_wide.csv")

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

required_series <- c("Energy", "Cereals", "Oils and Meals", "Fertilizers")
canonical_series <- c(
  "energy" = "Energy",
  "cereals" = "Cereals",
  "oils and meals" = "Oils and Meals",
  "fertilizers" = "Fertilizers"
)

normalize_name <- function(x) {
  x |>
    str_trim() |>
    str_replace_all("[_./-]+", " ") |>
    str_replace_all("\\s+", " ") |>
    str_to_lower()
}

find_latest_raw_file <- function(path) {
  files <- list.files(path, pattern = "^CMO-Historical-Data-Monthly_\\d{4}-\\d{2}-\\d{2}\\.xlsx$", full.names = TRUE)
  if (!length(files)) {
    stop(sprintf("No raw monthly workbook found in %s.", path), call. = FALSE)
  }

  file_dates <- as.IDate(str_match(basename(files), "_(\\d{4}-\\d{2}-\\d{2})\\.xlsx$")[, 2])
  files[order(file_dates, decreasing = TRUE)][1]
}

find_header_row <- function(dt) {
  max_rows <- min(nrow(dt), 25L)
  targets <- normalize_name(required_series)

  for (i in seq_len(max_rows)) {
    row_values <- normalize_name(unlist(dt[i], use.names = FALSE))
    score <- sum(targets %in% row_values)
    if (score >= 2L) {
      return(i)
    }
  }

  stop("Could not detect a header row containing the expected Market Indices series names.", call. = FALSE)
}

coerce_excel_date <- function(x) {
  if (inherits(x, c("Date", "POSIXct", "POSIXt"))) {
    return(as.IDate(x))
  }

  if (is.numeric(x)) {
    return(as.IDate(as.Date(x, origin = "1899-12-30")))
  }

  x_chr <- str_trim(as.character(x))
  x_chr[x_chr %in% c("", "NA", "N/A", "..") ] <- NA_character_

  parsed <- suppressWarnings(as.IDate(x_chr))
  missing_idx <- which(is.na(parsed) & !is.na(x_chr))
  if (length(missing_idx)) {
    parsed[missing_idx] <- suppressWarnings(as.IDate(as.Date(x_chr[missing_idx], format = "%b %Y")))
  }
  missing_idx <- which(is.na(parsed) & !is.na(x_chr))
  if (length(missing_idx)) {
    parsed[missing_idx] <- suppressWarnings(as.IDate(as.Date(x_chr[missing_idx], format = "%Y-%m")))
  }
  missing_idx <- which(is.na(parsed) & !is.na(x_chr))
  if (length(missing_idx)) {
    parsed[missing_idx] <- suppressWarnings(as.IDate(as.Date(x_chr[missing_idx], format = "%m/%d/%Y")))
  }

  parsed
}

latest_file <- find_latest_raw_file(raw_dir)
message(sprintf("Processing workbook: %s", latest_file))

sheets <- excel_sheets(latest_file)
if (!("Market Indices" %in% sheets)) {
  stop(sprintf("Workbook %s does not contain required sheet 'Market Indices'. Found sheets: %s", basename(latest_file), paste(sheets, collapse = ", ")), call. = FALSE)
}

raw_sheet <- as.data.table(read_excel(latest_file, sheet = "Market Indices", col_names = FALSE))
if (!nrow(raw_sheet) || !ncol(raw_sheet)) {
  stop("The 'Market Indices' sheet is empty.", call. = FALSE)
}

header_row <- find_header_row(raw_sheet)
header_values <- as.character(unlist(raw_sheet[header_row], use.names = FALSE))
header_values[is.na(header_values) | header_values == ""] <- paste0("col_", seq_along(header_values))[is.na(header_values) | header_values == ""]
setnames(raw_sheet, make.unique(header_values, sep = "_"))

market_dt <- raw_sheet[(header_row + 1):nrow(raw_sheet)]
if (!nrow(market_dt)) {
  stop("No data rows were found below the detected header row in 'Market Indices'.", call. = FALSE)
}

normalized_names <- normalize_name(names(market_dt))
date_col_idx <- which(normalized_names %in% c("monthly", "month", "date", "period", "time"))[1]
if (is.na(date_col_idx)) {
  date_col_idx <- 1L
}

desired_map <- setNames(rep(NA_integer_, length(required_series)), required_series)
for (col_idx in seq_along(normalized_names)) {
  match_name <- canonical_series[[normalized_names[col_idx]]]
  if (!is.null(match_name) && is.na(desired_map[[match_name]])) {
    desired_map[[match_name]] <- col_idx
  }
}

if (anyNA(desired_map)) {
  missing_cols <- names(desired_map)[is.na(desired_map)]
  stop(sprintf(
    "Could not locate all required series columns in 'Market Indices'. Missing: %s. Available columns: %s",
    paste(missing_cols, collapse = ", "),
    paste(names(market_dt), collapse = ", ")
  ), call. = FALSE)
}

selected <- market_dt[, c(date_col_idx, unname(desired_map)), with = FALSE]
setnames(selected, c("Date", names(desired_map)))
selected[, Date := coerce_excel_date(Date)]

for (series in required_series) {
  selected[, (series) := suppressWarnings(as.numeric(str_replace_all(as.character(get(series)), ",", "")))]
}

selected <- selected[!is.na(Date)]
if (!nrow(selected)) {
  stop("No valid dated observations were extracted from the 'Market Indices' sheet.", call. = FALSE)
}

selected <- unique(selected, by = "Date")
setorder(selected, Date)

long_dt <- melt(
  selected,
  id.vars = "Date",
  measure.vars = required_series,
  variable.name = "Series",
  value.name = "Value"
)
long_dt <- long_dt[!is.na(Value)]

if (!nrow(long_dt)) {
  stop("All extracted series values are missing after processing.", call. = FALSE)
}

wide_dt <- copy(selected)
setnames(wide_dt, old = c("Oils and Meals"), new = c("Oils_and_Meals"))

fwrite(long_dt, output_long)
fwrite(wide_dt, output_wide)
message(sprintf("Wrote tidy output to %s", output_long))
message(sprintf("Wrote wide output to %s", output_wide))
