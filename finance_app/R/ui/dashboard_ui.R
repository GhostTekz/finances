# R/ui/dashboard_ui.R

dashboard_ui <- function() {
  tagList(
    div(class = "container-fluid py-3",
      # ── Row 1: KPI cards ──────────────────────────────────────────────────
      fluidRow(
        col_4(
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-body",
              div(class = "d-flex justify-content-between align-items-start",
                div(
                  div(class = "text-muted small", "Net Worth"),
                  h3(class = "mb-0 fw-bold text-success",
                     withSpinner(uiOutput("dash_net_worth"), type = 4, size = 0.5))
                ),
                div(class = "bg-success bg-opacity-10 rounded p-2",
                    icon("wallet", class = "text-success fa-lg"))
              )
            )
          )
        ),
        col_4(
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-body",
              div(class = "d-flex justify-content-between align-items-start",
                div(
                  div(class = "text-muted small", "Monthly Income"),
                  h3(class = "mb-0 fw-bold text-primary",
                     withSpinner(uiOutput("dash_monthly_income"), type = 4, size = 0.5))
                ),
                div(class = "bg-primary bg-opacity-10 rounded p-2",
                    icon("arrow-down", class = "text-primary fa-lg"))
              )
            )
          )
        ),
        col_4(
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-body",
              div(class = "d-flex justify-content-between align-items-start",
                div(
                  div(class = "text-muted small", "Monthly Expenses"),
                  h3(class = "mb-0 fw-bold text-danger",
                     withSpinner(uiOutput("dash_monthly_expenses"), type = 4, size = 0.5))
                ),
                div(class = "bg-danger bg-opacity-10 rounded p-2",
                    icon("arrow-up", class = "text-danger fa-lg"))
              )
            )
          )
        )
      ),

      fluidRow(
        col_4(
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-body",
              div(class = "d-flex justify-content-between align-items-start",
                div(
                  div(class = "text-muted small", "Savings Rate"),
                  h3(class = "mb-0 fw-bold text-info",
                     withSpinner(uiOutput("dash_savings_rate"), type = 4, size = 0.5))
                ),
                div(class = "bg-info bg-opacity-10 rounded p-2",
                    icon("piggy-bank", class = "text-info fa-lg"))
              )
            )
          )
        ),
        col_4(
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-body",
              div(class = "d-flex justify-content-between align-items-start",
                div(
                  div(class = "text-muted small", "Bills Due (7 days)"),
                  h3(class = "mb-0 fw-bold text-warning",
                     withSpinner(uiOutput("dash_bills_due"), type = 4, size = 0.5))
                ),
                div(class = "bg-warning bg-opacity-10 rounded p-2",
                    icon("calendar", class = "text-warning fa-lg"))
              )
            )
          )
        ),
        col_4(
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-body",
              div(class = "d-flex justify-content-between align-items-start",
                div(
                  div(class = "text-muted small", "Active Goals"),
                  h3(class = "mb-0 fw-bold text-secondary",
                     withSpinner(uiOutput("dash_active_goals"), type = 4, size = 0.5))
                ),
                div(class = "bg-secondary bg-opacity-10 rounded p-2",
                    icon("flag", class = "text-secondary fa-lg"))
              )
            )
          )
        )
      ),

      # ── Row 2: Charts ────────────────────────────────────────────────────
      fluidRow(
        column(8,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              h6(class = "mb-0 fw-semibold", "Income vs. Expenses — Last 6 Months")
            ),
            div(class = "card-body",
              withSpinner(plotlyOutput("dash_income_expense_chart", height = "280px"), type = 4)
            )
          )
        ),
        column(4,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              h6(class = "mb-0 fw-semibold", "Spending by Category")
            ),
            div(class = "card-body",
              withSpinner(plotlyOutput("dash_category_pie", height = "280px"), type = 4)
            )
          )
        )
      ),

      # ── Row 3: Budget gauges + Upcoming bills + Recent tx ────────────────
      fluidRow(
        column(4,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              h6(class = "mb-0 fw-semibold", "Budget Utilization")
            ),
            div(class = "card-body",
              withSpinner(uiOutput("dash_budget_gauges"), type = 4)
            )
          )
        ),
        column(4,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              h6(class = "mb-0 fw-semibold", "Upcoming Bills (7 Days)")
            ),
            div(class = "card-body p-2",
              withSpinner(uiOutput("dash_upcoming_bills"), type = 4)
            )
          )
        ),
        column(4,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              h6(class = "mb-0 fw-semibold", "Recent Transactions")
            ),
            div(class = "card-body p-2",
              withSpinner(uiOutput("dash_recent_transactions"), type = 4)
            )
          )
        )
      )
    )
  )
}
