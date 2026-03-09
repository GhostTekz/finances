# R/ui/accounts_ui.R

accounts_ui <- function() {
  tagList(
    div(class = "container-fluid py-3",
      # Net worth banner
      fluidRow(
        column(12,
          div(class = "card border-0 shadow-sm mb-3 bg-primary text-white",
            div(class = "card-body py-2",
              div(class = "d-flex justify-content-between align-items-center",
                div(
                  div(class = "small opacity-75", "Total Net Worth"),
                  h2(class = "mb-0 fw-bold", withSpinner(uiOutput("acct_net_worth_banner"), type = 4, size = 0.5))
                ),
                icon("building-columns", class = "fa-3x opacity-25")
              )
            )
          )
        )
      ),

      fluidRow(
        column(8,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              div(class = "d-flex justify-content-between align-items-center",
                h5(class = "mb-0 fw-semibold", icon("building-columns"), " Accounts"),
                actionButton("acct_add_btn", "Add Account",
                             icon = icon("plus"), class = "btn-sm btn-primary")
              )
            ),
            div(class = "card-body p-0",
              withSpinner(uiOutput("accounts_cards"), type = 4)
            )
          )
        ),
        column(4,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              h6(class = "mb-0 fw-semibold", "Account Breakdown")
            ),
            div(class = "card-body",
              withSpinner(plotlyOutput("acct_type_chart", height = "250px"), type = 4)
            )
          ),
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              h6(class = "mb-0 fw-semibold", "Net Worth Over Time")
            ),
            div(class = "card-body",
              withSpinner(plotlyOutput("acct_networth_chart", height = "200px"), type = 4)
            )
          )
        )
      )
    ),

    # ── Add/Edit Account modal ──────────────────────────────────────────────
    hidden(div(id = "acct_modal_div",
      absolutePanel(
        class = "modal-overlay",
        div(class = "card shadow-lg modal-card",
          div(class = "card-header",
            div(class = "d-flex justify-content-between",
              h6(class = "mb-0", uiOutput("acct_modal_title")),
              actionButton("acct_modal_close", "", icon = icon("x"),
                           class = "btn-sm btn-outline-secondary")
            )
          ),
          div(class = "card-body",
            hidden(numericInput("acct_edit_id", NULL, value = NA)),
            fluidRow(
              column(6, textInput("acct_name_inp", "Account Name", placeholder = "e.g. Chase Checking")),
              column(6,
                selectInput("acct_type_inp", "Type",
                  choices = c("Checking" = "checking", "Savings" = "savings",
                              "Credit Card" = "credit", "Investment" = "investment",
                              "Loan" = "loan", "Cash" = "cash"))
              )
            ),
            fluidRow(
              column(6, numericInput("acct_balance_inp", "Current Balance ($)", value = 0, step = 0.01)),
              column(6, textInput("acct_currency_inp", "Currency", value = "USD"))
            ),
            fluidRow(
              column(6, numericInput("acct_rate_inp", "Interest Rate (%)", value = 0, min = 0, max = 100, step = 0.01)),
              column(6)
            ),
            textAreaInput("acct_notes_inp", "Notes", rows = 2, placeholder = "Optional…")
          ),
          div(class = "card-footer d-flex justify-content-end gap-2",
            actionButton("acct_save_btn", "Save", icon = icon("save"), class = "btn-primary"),
            actionButton("acct_cancel_btn", "Cancel", icon = icon("times"),
                         class = "btn-outline-secondary")
          )
        )
      )
    ))
  )
}
