# R/utils/csv_parser.R — Bank statement CSV import logic

library(readr)
library(stringr)
library(lubridate)

#' Parse a bank statement CSV file
#' Attempts to detect common bank export formats automatically.
#' Returns a data frame with at minimum: date, amount, description columns
parse_bank_csv <- function(filepath) {
  # Read raw CSV — try different separators
  raw <- tryCatch(
    readr::read_csv(filepath, show_col_types = FALSE, name_repair = "minimal"),
    error = function(e) {
      readr::read_delim(filepath, delim = ";", show_col_types = FALSE, name_repair = "minimal")
    }
  )
  if (nrow(raw) == 0) stop("CSV file is empty.")

  # Normalise column names
  names(raw) <- stringr::str_to_lower(stringr::str_replace_all(names(raw), "\\s+", "_"))

  raw
}

#' Detect which column is most likely the date column
detect_date_col <- function(df) {
  date_patterns <- c("date", "trans_date", "transaction_date",
                      "posted_date", "value_date", "posting_date")
  cols <- tolower(names(df))
  match <- cols[cols %in% date_patterns]
  if (length(match) > 0) return(match[1])
  # Heuristic: column whose values look like dates
  for (col in names(df)) {
    vals <- as.character(df[[col]])
    parsed <- tryCatch(as.Date(vals[!is.na(vals)][1:5]), error = function(e) NULL)
    if (!is.null(parsed) && !all(is.na(parsed))) return(col)
  }
  names(df)[1]
}

#' Detect which column is most likely the amount column
detect_amount_col <- function(df) {
  amount_patterns <- c("amount", "debit", "credit", "transaction_amount",
                        "trans_amount", "value", "sum")
  cols <- tolower(names(df))
  match <- cols[cols %in% amount_patterns]
  if (length(match) > 0) return(match[1])
  # Heuristic: first numeric-looking column
  for (col in names(df)) {
    vals <- as.numeric(gsub("[^0-9.\\-]", "", as.character(df[[col]])))
    if (!all(is.na(vals))) return(col)
  }
  names(df)[2]
}

#' Detect which column is most likely the description column
detect_description_col <- function(df) {
  desc_patterns <- c("description", "memo", "narrative", "details",
                      "merchant", "payee", "reference", "remarks")
  cols <- tolower(names(df))
  match <- cols[cols %in% desc_patterns]
  if (length(match) > 0) return(match[1])
  ""
}

#' Parse a dollar amount string to numeric, handling ($) format, commas, etc.
parse_amount <- function(x) {
  x <- as.character(x)
  x <- gsub(",", "",  x)          # remove thousands separator
  x <- gsub("\\$", "", x)         # remove dollar sign
  x <- gsub("\\(([0-9.]+)\\)", "-\\1", x)  # (123.45) -> -123.45
  x <- stringr::str_trim(x)
  suppressWarnings(as.numeric(x))
}
