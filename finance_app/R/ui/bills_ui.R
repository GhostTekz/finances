# R/ui/bills_ui.R

bills_ui <- function() {
  tagList(
    div(class = "container-fluid py-3",
      fluidRow(
        column(8,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              div(class = "d-flex justify-content-between align-items-center",
                h5(class = "mb-0 fw-semibold", icon("file-invoice"), " Bills Tracker"),
                actionButton("bill_add_btn", "Add Bill",
                             icon = icon("plus"), class = "btn-sm btn-primary")
              )
            ),
            div(class = "card-body p-0",
              withSpinner(DTOutput("bills_table"), type = 4)
            )
          )
        ),
        column(4,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              h6(class = "mb-0 fw-semibold", "Monthly Obligations")
            ),
            div(class = "card-body",
              withSpinner(uiOutput("bills_summary_panel"), type = 4)
            )
          ),
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              h6(class = "mb-0 fw-semibold", "Upcoming (30 days)")
            ),
            div(class = "card-body p-2",
              withSpinner(uiOutput("bills_upcoming_panel"), type = 4)
            )
          )
        )
      ),

      # Calendar-style bill view
      fluidRow(
        column(12,
          div(class = "card border-0 shadow-sm",
            div(class = "card-header bg-transparent border-0",
              h6(class = "mb-0 fw-semibold", "Bill Calendar — Current Month")
            ),
            div(class = "card-body",
              withSpinner(uiOutput("bills_calendar"), type = 4)
            )
          )
        )
      )
    ),

    # ── Add/Edit Bill modal ─────────────────────────────────────────────────
    hidden(div(id = "bill_modal_div",
      absolutePanel(
        class = "modal-overlay",
        div(class = "card shadow-lg modal-card",
          div(class = "card-header",
            div(class = "d-flex justify-content-between",
              h6(class = "mb-0", uiOutput("bill_modal_title")),
              actionButton("bill_modal_close", "", icon = icon("x"),
                           class = "btn-sm btn-outline-secondary")
            )
          ),
          div(class = "card-body",
            hidden(numericInput("bill_edit_id", NULL, value = NA)),
            fluidRow(
              column(6, textInput("bill_name_inp", "Bill Name", placeholder = "e.g. Electricity")),
              column(6, numericInput("bill_amount_inp", "Amount ($)", value = 0, min = 0, step = 0.01))
            ),
            fluidRow(
              column(6, dateInput("bill_due_date_inp", "Due Date", value = Sys.Date())),
              column(6,
                selectInput("bill_frequency_inp", "Frequency",
                  choices = c("Weekly" = "weekly", "Monthly" = "monthly",
                              "Yearly" = "yearly", "One-time" = "once"))
              )
            ),
            fluidRow(
              column(6, selectInput("bill_account_inp", "Pay from Account", choices = NULL)),
              column(6,
                div(class = "mt-4",
                  checkboxInput("bill_autopay_inp", "Autopay enabled", FALSE)
                )
              )
            ),
            textAreaInput("bill_notes_inp", "Notes", rows = 2, placeholder = "Optional notes…")
          ),
          div(class = "card-footer d-flex justify-content-end gap-2",
            actionButton("bill_save_btn", "Save", icon = icon("save"), class = "btn-primary"),
            actionButton("bill_cancel_btn", "Cancel", icon = icon("times"),
                         class = "btn-outline-secondary")
          )
        )
      )
    ))
  )
}
