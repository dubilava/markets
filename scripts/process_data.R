#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(stringr)
})

raw_dir <- "data/raw"
processed_dir <- "data/processed"

output_long <- file.path(processed_dir, "pink_sheet_combined.csv")
output_wide <- file.path(processed_dir, "pink_sheet_combined_wide.csv")

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

find_latest_raw_file <- function(path) {
  files <- list.files(
    path,
    pattern = "^CMO-Historical-Data-Monthly_\\d{4}-\\d{2}-\\d{2}\\.xlsx$",
    full.names = TRUE
  )
  
  if (!length(files)) {
    stop(sprintf("No raw monthly workbook found in %s.", path), call. = FALSE)
  }
  
  file_dates <- as.IDate(str_match(basename(files), "_(\\d{4}-\\d{2}-\\d{2})\\.xlsx$")[, 2])
  files[order(file_dates, decreasing = TRUE)][1]
}

clean_name <- function(name) {
  name <- gsub("\\*+", "", name)
  name <- gsub("[^A-Za-z0-9]+", "_", name)
  name <- gsub("_+", "_", name)     # collapse here
  name <- gsub("^_|_$", "", name)
  name
}

parse_ym <- function(x) {
  as.Date(paste0(substr(x, 1, 4), "-", substr(x, 6, 7), "-01"))
}

parse_unit_currency <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("^\\(|\\)$", "", x)
  
  parts <- strsplit(x, "/", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  
  if (length(parts) == 2) {
    num <- parts[1]
    den <- parts[2]
    
    # map numerator → currency
    currency <- if (num %in% c("$", "US$")) {
      "dollar"
    } else if (num == "c") {
      "cent"
    } else {
      clean_name(num)
    }
    
    unit <- clean_name(den)
    
    list(
      Currency = currency,
      Unit = unit
    )
    
  } else {
    list(
      Currency = clean_name(x),
      Unit = NA_character_
    )
  }
}

process_monthly_indices <- function(latest_file) {
  sheet_name <- "Monthly Indices"
  
  raw_sheet <- as.data.table(
    read_excel(latest_file, sheet = sheet_name, col_names = FALSE)
  )
  
  data_start <- which(grepl("^\\d{4}M\\d{2}$", raw_sheet[[1]]))[1]
  if (is.na(data_start)) {
    stop(sprintf("Could not detect start of data in sheet '%s'.", sheet_name), call. = FALSE)
  }
  
  header_rows <- raw_sheet[1:(data_start - 1)]
  
  get_name_first <- function(x) {
    val <- x[!is.na(x) & x != ""]
    if (length(val) == 0) return(NA_character_)
    clean_name(val[1])
  }
  
  new_names <- sapply(header_rows, get_name_first)
  new_names[1] <- "Date"
  new_names <- make.unique(new_names, sep = "_")
  
  setnames(raw_sheet, new_names)
  dt <- raw_sheet[data_start:.N]
  dt <- dt[!is.na(Date)]
  dt[, Date := parse_ym(as.character(Date))]
  
  value_cols <- setdiff(names(dt), "Date")
  dt[, (value_cols) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = value_cols]
  
  long_dt <- melt(
    dt,
    id.vars = "Date",
    variable.name = "Series",
    value.name = "Value"
  )
  
  long_dt <- long_dt[!is.na(Value)]
  long_dt[, `:=`(
    Unit = "Index",
    Currency = "2010_100"
  )]
  
  setcolorder(long_dt, c("Date", "Series", "Unit", "Currency", "Value"))
  setorder(long_dt, Date, Series)
  
  long_dt[]
}

process_monthly_prices <- function(latest_file) {
  sheet_name <- "Monthly Prices"
  
  raw_sheet <- as.data.table(
    read_excel(latest_file, sheet = sheet_name, col_names = FALSE)
  )
  
  data_start <- which(grepl("^\\d{4}M\\d{2}$", raw_sheet[[1]]))[1]
  if (is.na(data_start)) {
    stop(sprintf("Could not detect start of data in sheet '%s'.", sheet_name), call. = FALSE)
  }
  
  header_rows <- raw_sheet[1:(data_start - 1)]
  
  # For Monthly Prices:
  # - series names are the second-last non-missing entry in each column
  # - unit/currency is the last non-missing entry in each column
  get_series_name <- function(x) {
    val <- x[!is.na(x) & x != ""]
    if (length(val) == 0) return(NA_character_)
    if (length(val) == 1) return(clean_name(val[1]))
    clean_name(val[length(val) - 1])
  }
  
  get_unit_currency_raw <- function(x) {
    val <- x[!is.na(x) & x != ""]
    if (length(val) < 2) return(NA_character_)
    as.character(val[length(val)])
  }
  
  series_names <- sapply(header_rows, get_series_name)
  unit_currency_raw <- sapply(header_rows, get_unit_currency_raw)
  
  series_names[1] <- "Date"
  series_names <- make.unique(series_names, sep = "_")
  
  setnames(raw_sheet, series_names)
  dt <- raw_sheet[data_start:.N]
  dt <- dt[!is.na(Date)]
  dt[, Date := parse_ym(as.character(Date))]
  
  value_cols <- setdiff(names(dt), "Date")
  dt[, (value_cols) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = value_cols]
  
  # Build metadata table from original headers
  meta <- data.table(
    Series = value_cols,
    unit_currency_raw = unit_currency_raw[match(value_cols, series_names)]
  )
  
  meta[, c("Currency", "Unit") := {
    parsed <- lapply(unit_currency_raw, parse_unit_currency)
    list(
      vapply(parsed, `[[`, character(1), "Currency"),
      vapply(parsed, `[[`, character(1), "Unit")
    )
  }]
  
  meta[, unit_currency_raw := NULL]
  
  long_dt <- melt(
    dt,
    id.vars = "Date",
    variable.name = "Series",
    value.name = "Value"
  )
  
  long_dt <- long_dt[!is.na(Value)]
  long_dt <- merge(long_dt, meta, by = "Series", all.x = TRUE, sort = FALSE)
  
  setcolorder(long_dt, c("Date", "Series", "Unit", "Currency", "Value"))
  setorder(long_dt, Date, Series)
  
  long_dt[]
}

latest_file <- find_latest_raw_file(raw_dir)

indices_dt <- process_monthly_indices(latest_file)
prices_dt  <- process_monthly_prices(latest_file)

combined_long <- rbindlist(
  list(indices_dt, prices_dt),
  use.names = TRUE,
  fill = TRUE
)

setorder(combined_long, Date, Series)

combined_long[, Series_Wide := paste(Series, Unit, Currency, sep = "_")]

combined_wide <- dcast(
  combined_long,
  Date ~ Series_Wide,
  value.var = "Value"
)

fwrite(combined_long, output_long)
fwrite(combined_wide, output_wide)

message(sprintf("Processed workbook: %s", basename(latest_file)))
message(sprintf("Saved combined long data to: %s", output_long))
message(sprintf("Saved combined wide data to: %s", output_wide))