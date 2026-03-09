# app.R — Main entry point for R Personal Finance Platform
# Runs as a Shiny app at localhost:7777

library(shiny)
library(bslib)
library(DBI)
library(RSQLite)
library(openssl)
library(bcrypt)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(shinyalert)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(scales)
library(glue)
library(stringr)
library(readr)
library(purrr)
library(fontawesome)

# ── Source modules ─────────────────────────────────────────────────────────────
source("R/auth.R")
source("R/database.R")
source("R/utils/formatters.R")
source("R/utils/charts.R")
source("R/utils/csv_parser.R")

# UI files
source("R/ui/dashboard_ui.R")
source("R/ui/transactions_ui.R")
source("R/ui/budget_ui.R")
source("R/ui/bills_ui.R")
source("R/ui/accounts_ui.R")
source("R/ui/vault_ui.R")
source("R/ui/goals_ui.R")
source("R/ui/reports_ui.R")
source("R/ui/settings_ui.R")

# Server files
source("R/server/dashboard_server.R")
source("R/server/transactions_server.R")
source("R/server/budget_server.R")
source("R/server/bills_server.R")
source("R/server/accounts_server.R")
source("R/server/vault_server.R")
source("R/server/goals_server.R")
source("R/server/reports_server.R")
source("R/server/settings_server.R")

# ── Initialise database ────────────────────────────────────────────────────────
DB_PATH <- "data/finance.db"
dir.create("data", showWarnings = FALSE)
init_database(DB_PATH)

# ── Theme ──────────────────────────────────────────────────────────────────────
finance_theme <- bs_theme(
  version = 5,
  bootswatch = "flatly",
  primary   = "#2C7BE5",
  secondary = "#6E84A3",
  success   = "#00D97E",
  danger    = "#E63757",
  warning   = "#F6C343",
  base_font = font_google("Inter"),
  code_font = font_google("JetBrains Mono")
)

dark_theme <- bs_theme(
  version = 5,
  bootswatch = "darkly",
  primary   = "#2C7BE5",
  success   = "#00D97E",
  danger    = "#E63757",
  warning   = "#F6C343",
  base_font = font_google("Inter"),
  code_font = font_google("JetBrains Mono")
)

# ── Auth UI (shown before main app) ──────────────────────────────────────────
auth_ui <- function() {
  fluidPage(
    theme = finance_theme,
    tags$head(
      tags$link(rel = "stylesheet", href = "custom.css"),
      tags$title("Finance Platform — Login")
    ),
    useShinyjs(),
    useShinyalert(),
    div(
      class = "auth-container",
      div(
        class = "auth-card card shadow",
        div(
          class = "card-body p-5",
          div(class = "text-center mb-4",
            tags$i(class = "fas fa-coins fa-3x text-primary"),
            h2("Finance Platform", class = "mt-2 fw-bold"),
            p("Personal Finance & Audit System", class = "text-muted")
          ),
          div(id = "login_panel",
            h5("Enter Master Password", class = "text-center mb-3"),
            passwordInput("master_password", "Master Password", placeholder = "Enter your master password"),
            div(class = "d-grid mt-3",
              actionButton("login_btn", "Unlock", class = "btn-primary btn-lg", icon = icon("lock-open"))
            ),
            div(class = "text-center mt-3",
              tags$small(
                class = "text-muted",
                "First time? A new password will be created. ",
                tags$br(),
                tags$strong("Remember it — it cannot be recovered.")
              )
            )
          ),
          hidden(div(id = "setup_panel",
            h5("Create Master Password", class = "text-center mb-3"),
            p(class = "text-warning text-center",
              icon("triangle-exclamation"), " Store this password safely. It cannot be recovered."),
            passwordInput("new_password", "New Master Password", placeholder = "Choose a strong password"),
            passwordInput("confirm_password", "Confirm Password", placeholder = "Repeat password"),
            div(class = "d-grid mt-3",
              actionButton("create_btn", "Create & Enter", class = "btn-success btn-lg",
                           icon = icon("check-circle"))
            )
          ))
        )
      )
    )
  )
}

# ── Main App UI ────────────────────────────────────────────────────────────────
main_ui <- function() {
  page_navbar(
    id = "main_nav",
    title = span(icon("coins"), " Finance"),
    theme = finance_theme,
    fillable = FALSE,
    header = tagList(
      tags$head(
        tags$link(rel = "stylesheet", href = "custom.css"),
        tags$link(rel = "stylesheet",
          href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css")
      ),
      useShinyjs(),
      useShinyalert(),
      # Inactivity timer — 15 min
      tags$script(HTML("
        var idleTimer;
        function resetIdle() {
          clearTimeout(idleTimer);
          idleTimer = setTimeout(function() {
            Shiny.setInputValue('session_idle', Date.now());
          }, 15 * 60 * 1000);
        }
        document.addEventListener('mousemove', resetIdle);
        document.addEventListener('keypress', resetIdle);
        resetIdle();
      ")),
      div(
        class = "d-flex justify-content-end align-items-center px-3 py-1 border-bottom",
        style = "background: var(--bs-body-bg); gap: 10px;",
        uiOutput("top_bar_info"),
        actionButton("lock_app", "", icon = icon("lock"),
                      class = "btn-sm btn-outline-secondary",
                      title = "Lock application"),
        materialSwitch("dark_mode_toggle", label = "Dark", value = FALSE,
                        status = "primary", inline = TRUE)
      )
    ),
    nav_panel("Dashboard",   icon = icon("gauge-high"),   dashboard_ui()),
    nav_panel("Transactions",icon = icon("arrow-right-arrow-left"), transactions_ui()),
    nav_panel("Budget",      icon = icon("bullseye"),      budget_ui()),
    nav_panel("Bills",       icon = icon("file-invoice"),  bills_ui()),
    nav_panel("Accounts",    icon = icon("building-columns"), accounts_ui()),
    nav_panel("Vault",       icon = icon("shield-halved"), vault_ui()),
    nav_panel("Goals",       icon = icon("flag"),          goals_ui()),
    nav_panel("Reports",     icon = icon("chart-bar"),     reports_ui()),
    nav_panel("Settings",    icon = icon("gear"),          settings_ui())
  )
}

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- uiOutput("app_ui")

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  # ── Reactive state ──────────────────────────────────────────────────────────
  rv <- reactiveValues(
    authenticated = FALSE,
    enc_key       = NULL,   # Raw AES key derived from master password
    dark_mode     = FALSE,
    db            = NULL
  )

  # Check if master password has been set
  pw_set <- reactive({ auth_password_exists(DB_PATH) })

  # ── Auth rendering ──────────────────────────────────────────────────────────
  output$app_ui <- renderUI({
    if (rv$authenticated) main_ui() else auth_ui()
  })

  # Show setup panel if no password set yet
  observe({
    if (!pw_set()) {
      shinyjs::hide("login_panel")
      shinyjs::show("setup_panel")
    }
  })

  # ── Login ───────────────────────────────────────────────────────────────────
  observeEvent(input$login_btn, {
    req(input$master_password)
    pwd <- input$master_password
    if (nchar(trimws(pwd)) == 0) {
      shinyalert("Error", "Password cannot be empty.", type = "error")
      return()
    }
    result <- verify_master_password(DB_PATH, pwd)
    if (result$success) {
      rv$enc_key       <- result$key
      rv$authenticated <- TRUE
      rv$db            <- dbConnect(SQLite(), DB_PATH)
    } else {
      shinyalert("Access Denied", "Incorrect master password.", type = "error")
      updatePasswordInput(session, "master_password", value = "")
    }
  })

  # ── Create password ─────────────────────────────────────────────────────────
  observeEvent(input$create_btn, {
    pwd  <- input$new_password
    cpwd <- input$confirm_password
    if (nchar(trimws(pwd)) < 8) {
      shinyalert("Error", "Password must be at least 8 characters.", type = "error")
      return()
    }
    if (pwd != cpwd) {
      shinyalert("Error", "Passwords do not match.", type = "error")
      return()
    }
    result <- create_master_password(DB_PATH, pwd)
    if (result$success) {
      rv$enc_key       <- result$key
      rv$authenticated <- TRUE
      rv$db            <- dbConnect(SQLite(), DB_PATH)
    } else {
      shinyalert("Error", paste("Setup failed:", result$message), type = "error")
    }
  })

  # ── Lock ────────────────────────────────────────────────────────────────────
  observeEvent(input$lock_app, {
    if (!is.null(rv$db)) { try(dbDisconnect(rv$db), silent = TRUE) }
    rv$authenticated <- FALSE
    rv$enc_key       <- NULL
    rv$db            <- NULL
    session$reload()
  })

  # Idle timeout
  observeEvent(input$session_idle, {
    shinyalert("Session Locked", "Locked due to inactivity.", type = "warning",
               callbackR = function(x) session$reload())
  })

  # ── Dark mode ───────────────────────────────────────────────────────────────
  observeEvent(input$dark_mode_toggle, {
    rv$dark_mode <- input$dark_mode_toggle
    if (input$dark_mode_toggle) {
      session$setCurrentTheme(dark_theme)
    } else {
      session$setCurrentTheme(finance_theme)
    }
  })

  # ── Top bar ─────────────────────────────────────────────────────────────────
  output$top_bar_info <- renderUI({
    req(rv$authenticated)
    db <- rv$db
    net_worth <- tryCatch({
      accts <- dbGetQuery(db, "SELECT SUM(balance) as nw FROM accounts")
      fmt_currency(accts$nw[1] %||% 0)
    }, error = function(e) "$0.00")
    tagList(
      span(class = "badge bg-success me-2", icon("wallet"), " Net Worth: ", net_worth),
      span(class = "text-muted small", format(Sys.time(), "%b %d, %Y"))
    )
  })

  # ── Cleanup on disconnect ────────────────────────────────────────────────────
  session$onSessionEnded(function() {
    if (!is.null(rv$db)) { try(dbDisconnect(rv$db), silent = TRUE) }
  })

  # ── Delegate to module servers ───────────────────────────────────────────────
  observe({
    req(rv$authenticated, rv$db)
    dashboard_server(input, output, session, rv)
    transactions_server(input, output, session, rv)
    budget_server(input, output, session, rv)
    bills_server(input, output, session, rv)
    accounts_server(input, output, session, rv)
    vault_server(input, output, session, rv)
    goals_server(input, output, session, rv)
    reports_server(input, output, session, rv)
    settings_server(input, output, session, rv)
  })
}

# ── Run ─────────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
