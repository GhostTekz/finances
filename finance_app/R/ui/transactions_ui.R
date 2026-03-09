# R/ui/transactions_ui.R

transactions_ui <- function() {
  tagList(
    div(class = "container-fluid py-3",
      fluidRow(
        column(12,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              div(class = "d-flex justify-content-between align-items-center",
                h5(class = "mb-0 fw-semibold", icon("arrow-right-arrow-left"), " Transactions"),
                div(class = "btn-group",
                  actionButton("tx_add_btn",    "Add Transaction",
                               icon = icon("plus"),    class = "btn-sm btn-primary"),
                  actionButton("tx_import_btn", "Import CSV",
                               icon = icon("upload"),  class = "btn-sm btn-outline-secondary")
                )
              )
            ),
            div(class = "card-body",
              # Filter bar
              fluidRow(
                column(3,
                  dateRangeInput("tx_date_range", "Date Range",
                                  start = floor_date(Sys.Date(), "month"),
                                  end   = Sys.Date(), separator = "to")
                ),
                column(2,
                  selectInput("tx_filter_category", "Category",
                              choices = c("All" = ""), width = "100%")
                ),
                column(2,
                  selectInput("tx_filter_account", "Account",
                              choices = c("All" = ""), width = "100%")
                ),
                column(3,
                  textInput("tx_search", "Search", placeholder = "Description or tags…")
                ),
                column(2,
                  div(class = "mt-4",
                    actionButton("tx_filter_btn", "Filter",
                                 icon = icon("filter"), class = "btn-sm btn-secondary"),
                    actionButton("tx_clear_filter", "Clear",
                                 icon = icon("x"), class = "btn-sm btn-outline-secondary ms-1")
                  )
                )
              ),
              # Summary row
              uiOutput("tx_summary_row"),
              hr(),
              withSpinner(DTOutput("tx_table"), type = 4)
            )
          )
        )
      )
    ),

    # ── Add/Edit modal ──────────────────────────────────────────────────────
    hidden(div(id = "tx_modal_div",
      absolutePanel(
        class = "modal-overlay",
        div(class = "card shadow-lg modal-card",
          div(class = "card-header",
            div(class = "d-flex justify-content-between",
              h6(class = "mb-0", uiOutput("tx_modal_title")),
              actionButton("tx_modal_close", "", icon = icon("x"),
                           class = "btn-sm btn-outline-secondary")
            )
          ),
          div(class = "card-body",
            hidden(numericInput("tx_edit_id", NULL, value = NA)),
            fluidRow(
              column(6,
                dateInput("tx_date", "Date", value = Sys.Date())
              ),
              column(6,
                numericInput("tx_amount", "Amount ($)", value = 0, min = 0, step = 0.01)
              )
            ),
            fluidRow(
              column(6,
                selectInput("tx_category", "Category", choices = NULL)
              ),
              column(6,
                selectInput("tx_account", "Account", choices = NULL)
              )
            ),
            textInput("tx_description", "Description", placeholder = "What was this for?"),
            textInput("tx_tags", "Tags", placeholder = "comma, separated, tags"),
            fluidRow(
              column(6,
                checkboxInput("tx_recurring", "Recurring transaction", FALSE)
              ),
              column(6,
                checkboxInput("tx_tax_deductible", "Tax deductible", FALSE)
              )
            )
          ),
          div(class = "card-footer d-flex justify-content-end gap-2",
            actionButton("tx_save_btn", "Save", icon = icon("save"),
                         class = "btn-primary"),
            actionButton("tx_cancel_btn", "Cancel", icon = icon("times"),
                         class = "btn-outline-secondary")
          )
        )
      )
    )),

    # ── CSV Import modal ────────────────────────────────────────────────────
    hidden(div(id = "tx_import_modal_div",
      absolutePanel(
        class = "modal-overlay",
        div(class = "card shadow-lg modal-card",
          div(class = "card-header",
            div(class = "d-flex justify-content-between",
              h6(class = "mb-0", icon("upload"), " Import Bank CSV"),
              actionButton("tx_import_modal_close", "", icon = icon("x"),
                           class = "btn-sm btn-outline-secondary")
            )
          ),
          div(class = "card-body",
            fileInput("tx_csv_file", "Select CSV file",
                       accept = c(".csv", "text/csv")),
            uiOutput("tx_csv_preview"),
            selectInput("tx_import_account", "Import to Account", choices = NULL),
            uiOutput("tx_csv_mapping_ui")
          ),
          div(class = "card-footer d-flex justify-content-end gap-2",
            actionButton("tx_import_confirm", "Import",
                         icon = icon("file-import"), class = "btn-success"),
            actionButton("tx_import_cancel", "Cancel",
                         icon = icon("times"), class = "btn-outline-secondary")
          )
        )
      )
    ))
  )
}
