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

latest_file <- find_latest_raw_file(raw_dir)

sheet_name <- "Monthly Indices"

raw_sheet <- as.data.table(
  read_excel(
    latest_file,
    sheet = sheet_name,
    col_names = FALSE
  )
)

if (nrow(raw_sheet) < 10L) {
  stop(sprintf("Sheet '%s' does not contain expected data rows.", sheet_name), call. = FALSE)
}


# find where actual data starts (e.g. "1960M01")
data_start <- which(grepl("^\\d{4}M\\d{2}$", raw_sheet[[1]]))[1]

# header rows = everything above that
header_rows <- raw_sheet[1:(data_start - 1)]

get_name <- function(x) {
  val <- x[!is.na(x) & x != ""]
  if (length(val) == 0) return(NA_character_)
  
  name <- val[1]
  name <- gsub("\\*+", "", name)            # remove asterisks
  name <- gsub("[^A-Za-z0-9]+", "_", name)  # replace non-alphanumeric with _
  name <- gsub("_+", "_", name)             # collapse multiple _
  name <- gsub("^_|_$", "", name)           # trim leading/trailing _
  
  name
}

# generate names
new_names <- sapply(header_rows, get_name)

new_names[1] <- "Date"

# assign names
setnames(raw_sheet, new_names)

# drop header rows
raw_sheet <- raw_sheet[data_start:.N]

# clean up the data
dt <- copy(raw_sheet)

# Drop fully empty rows if any
dt <- dt[!is.na(Date)]

# Parse dates like 1960M01
dt[, Date := as.Date(paste0(substr(Date, 1, 4), "-", substr(Date, 6, 7), "-01"))]

# Convert numeric columns
value_cols <- setdiff(names(raw_sheet), "Date")
dt[, (value_cols) := lapply(.SD, function(x) as.numeric(as.character(x))), .SDcols = value_cols]

# Long format
long_dt <- melt(
  dt,
  id.vars = "Date",
  variable.name = "Series",
  value.name = "Value"
)

long_dt <- long_dt[!is.na(Date) & !is.na(Value)]
setorder(long_dt, Date, Series)

# Wide format
wide_dt <- dcast(long_dt, Date ~ Series, value.var = "Value")
setnames(wide_dt, old = "Oils and Meals", new = "Oils_and_Meals", skip_absent = TRUE)

fwrite(long_dt, output_long)
fwrite(wide_dt, output_wide)

message(sprintf("Processed workbook: %s", basename(latest_file)))
message(sprintf("Saved long data to: %s", output_long))
message(sprintf("Saved wide data to: %s", output_wide))
