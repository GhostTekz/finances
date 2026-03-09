#!/usr/bin/env Rscript
# install_packages.R — One-time dependency installer for Finance Platform

pkgs <- c(
  "shiny", "bslib", "DBI", "RSQLite", "openssl", "bcrypt",
  "dplyr", "tidyr", "lubridate", "ggplot2", "plotly", "DT",
  "shinyalert", "shinycssloaders", "rmarkdown", "knitr",
  "readr", "stringr", "purrr", "htmltools", "shinyjs",
  "scales", "glue", "digest", "shinyWidgets", "fontawesome"
)

installed <- rownames(installed.packages())
to_install <- pkgs[!pkgs %in% installed]

if (length(to_install) > 0) {
  cat("Installing missing packages:", paste(to_install, collapse = ", "), "\n")
  install.packages(to_install, repos = "https://cloud.r-project.org", dependencies = TRUE)
} else {
  cat("All required packages are already installed.\n")
}

# Verify all installed
still_missing <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(still_missing) > 0) {
  cat("WARNING: Failed to install:", paste(still_missing, collapse = ", "), "\n")
  quit(status = 1)
} else {
  cat("All packages verified successfully.\n")
}
