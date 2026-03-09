# R/utils/formatters.R — Currency and date formatters

#' Format a number as currency string
fmt_currency <- function(x, symbol = "$", digits = 2) {
  if (is.null(x) || is.na(x)) return(paste0(symbol, "0.00"))
  formatted <- formatC(abs(x), format = "f", digits = digits, big.mark = ",")
  if (x < 0) paste0("-", symbol, formatted) else paste0(symbol, formatted)
}

#' Format a date with the configured format
fmt_date <- function(d, fmt = "%Y-%m-%d") {
  if (is.null(d) || is.na(d)) return("")
  format(as.Date(d), fmt)
}

#' Format a percentage
fmt_pct <- function(x, digits = 1) {
  if (is.null(x) || is.na(x)) return("0%")
  paste0(round(x, digits), "%")
}

#' Truncate a string with ellipsis
trunc_str <- function(s, n = 30) {
  if (is.null(s) || is.na(s)) return("")
  if (nchar(s) <= n) return(s)
  paste0(substr(s, 1, n - 3), "...")
}

#' Null-coalescing operator (re-exported for utils scope)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b
