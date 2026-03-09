#!/usr/bin/env Rscript
# install_packages.R — One-time dependency installer for Finance Platform

# Ensure a user-writable library exists and is on the path
user_lib <- Sys.getenv("R_LIBS_USER")
if (nchar(user_lib) == 0) {
  user_lib <- path.expand("~/.local/lib/R/library")
}
dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)

# Prepend user lib so installs and lookups go there first
if (!user_lib %in% .libPaths()) {
  .libPaths(c(user_lib, .libPaths()))
}
cat("R library path:", .libPaths()[1], "\n")

pkgs <- c(
  "shiny", "bslib", "DBI", "RSQLite", "openssl", "bcrypt",
  "dplyr", "tidyr", "lubridate", "ggplot2", "plotly", "DT",
  "shinyalert", "shinycssloaders", "rmarkdown", "knitr",
  "readr", "stringr", "purrr", "htmltools", "shinyjs",
  "scales", "glue", "digest", "shinyWidgets", "fontawesome"
)

installed   <- rownames(installed.packages(lib.loc = .libPaths()))
to_install  <- pkgs[!pkgs %in% installed]

if (length(to_install) > 0) {
  cat("Installing missing packages:", paste(to_install, collapse = ", "), "\n")
  install.packages(
    to_install,
    repos        = "https://cloud.r-project.org",
    lib          = user_lib,
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
} else {
  cat("All required packages are already installed.\n")
}

# Verify
still_missing <- pkgs[!pkgs %in% rownames(installed.packages(lib.loc = .libPaths()))]
if (length(still_missing) > 0) {
  cat("WARNING: Failed to install:", paste(still_missing, collapse = ", "), "\n")
  quit(status = 1)
} else {
  cat("All packages verified successfully.\n")
}
