#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(httr2)
  library(rvest)
  library(stringr)
})

raw_dir <- "data/raw"
log_dir <- "logs"
log_file <- file.path(log_dir, "fetch_log.txt")
source_url <- "https://www.worldbank.org/en/research/commodity-markets"
max_attempts <- 3L
retry_backoff <- c(2, 5, 10)

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

log_message <- function(level, message) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  line <- sprintf("[%s] [%s] %s", timestamp, level, message)
  cat(line, "\n", sep = "")
  cat(line, "\n", sep = "", file = log_file, append = TRUE)
}

retry <- function(expr_fun, action) {
  last_error <- NULL
  for (attempt in seq_len(max_attempts)) {
    log_message("INFO", sprintf("%s (attempt %s of %s)", action, attempt, max_attempts))
    result <- tryCatch(expr_fun(), error = function(e) {
      last_error <<- e
      NULL
    })

    if (!is.null(result)) {
      return(result)
    }

    log_message("WARN", sprintf("%s failed: %s", action, conditionMessage(last_error)))
    if (attempt < max_attempts) {
      Sys.sleep(retry_backoff[pmin(attempt, length(retry_backoff))])
    }
  }

  stop(sprintf("%s failed after %s attempts: %s", action, max_attempts, conditionMessage(last_error)), call. = FALSE)
}

normalize_href <- function(href, base_url) {
  if (is.na(href) || !nzchar(href)) {
    return(NA_character_)
  }

  if (str_detect(href, "^https?://")) {
    return(href)
  }

  if (str_starts(href, "//")) {
    return(paste0("https:", href))
  }

  origin <- str_match(base_url, "^(https?://[^/]+)")[, 2]
  base_path <- sub("^(https?://[^/]+)", "", base_url)

  if (str_starts(href, "/")) {
    return(paste0(origin, href))
  }

  base_dir <- sub("/[^/]*$", "/", base_path)
  paste0(origin, base_dir, href)
}

extract_monthly_prices_url <- function(page_url) {
  page <- retry(
    function() {
      req(page_url) |>
        req_user_agent("markets-pink-sheet-pipeline/1.0") |>
        req_timeout(60) |>
        req_perform() |>
        resp_body_html()
    },
    sprintf("Fetch source page %s", page_url)
  )

  anchors <- html_elements(page, "a")
  hrefs <- html_attr(anchors, "href")
  texts <- html_text2(anchors)
  normalized_texts <- str_squish(texts)

  idx <- which(
    str_detect(str_to_lower(normalized_texts), "monthly\\s+prices") |
      str_detect(str_to_lower(coalesce(hrefs, "")), "cmo.*historical.*data.*monthly.*xlsx")
  )

  if (length(idx) == 0L) {
    stop("Could not find an anchor containing 'Monthly prices' on the World Bank commodity markets page.", call. = FALSE)
  }

  candidates <- unique(na.omit(vapply(hrefs[idx], normalize_href, character(1), base_url = page_url)))
  candidates <- candidates[str_detect(str_to_lower(candidates), "xlsx|monthly")]

  if (length(candidates) == 0L) {
    stop("Found 'Monthly prices' anchor text, but no usable href attribute was available.", call. = FALSE)
  }

  candidates[[1]]
}

coalesce <- function(x, y) ifelse(is.na(x), y, x)

resolve_remote_date <- function(file_url) {
  response <- retry(
    function() {
      req(file_url) |>
        req_user_agent("markets-pink-sheet-pipeline/1.0") |>
        req_method("HEAD") |>
        req_timeout(60) |>
        req_perform()
    },
    sprintf("Inspect remote workbook metadata %s", file_url)
  )

  header_candidates <- c(
    resp_header(response, "last-modified"),
    resp_header(response, "date")
  )
  header_candidates <- header_candidates[!is.na(header_candidates) & nzchar(header_candidates)]

  for (candidate in header_candidates) {
    parsed <- suppressWarnings(as.Date(strptime(candidate, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")))
    if (!is.na(parsed)) {
      return(parsed)
    }
  }

  log_message("WARN", "Remote workbook did not expose a parseable Last-Modified/Date header; falling back to today's date.")
  Sys.Date()
}

monthly_url <- extract_monthly_prices_url(source_url)
log_message("INFO", sprintf("Resolved monthly prices URL: %s", monthly_url))

file_date <- resolve_remote_date(monthly_url)
out_file <- file.path(raw_dir, sprintf("CMO-Historical-Data-Monthly_%s.xlsx", file_date))

if (file.exists(out_file)) {
  log_message("INFO", sprintf("A raw workbook for remote date %s already exists; skipping download: %s", file_date, out_file))
  quit(save = "no", status = 0)
}

temp_file <- tempfile(fileext = ".xlsx")
on.exit(unlink(temp_file), add = TRUE)

raw_content <- retry(
  function() {
    response <- req(monthly_url) |>
      req_user_agent("markets-pink-sheet-pipeline/1.0") |>
      req_timeout(120) |>
      req_retry(max_tries = 2) |>
      req_perform()

    resp_check_status(response)
    resp_body_raw(response)
  },
  sprintf("Download workbook %s", monthly_url)
)

writeBin(raw_content, temp_file)

file_info <- file.info(temp_file)
if (is.na(file_info$size) || file_info$size <= 0) {
  stop(sprintf("Downloaded file is empty: %s", monthly_url), call. = FALSE)
}

file.copy(temp_file, out_file, overwrite = FALSE)
log_message("INFO", sprintf("Saved workbook to %s (%s bytes)", out_file, file_info$size))
