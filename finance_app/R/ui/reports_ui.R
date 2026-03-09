# R/ui/reports_ui.R

reports_ui <- function() {
  tagList(
    div(class = "container-fluid py-3",
      # Filter bar
      div(class = "card border-0 shadow-sm mb-3",
        div(class = "card-body",
          fluidRow(
            column(3,
              dateRangeInput("rpt_date_range", "Date Range",
                              start = floor_date(Sys.Date() - 365, "month"),
                              end   = Sys.Date(), separator = "to")
            ),
            column(2,
              selectInput("rpt_account", "Account",
                           choices = c("All Accounts" = ""))
            ),
            column(2,
              selectInput("rpt_type", "Report Type",
                choices = c(
                  "Monthly Summary"   = "monthly",
                  "Category Breakdown"= "category",
                  "Income vs Expense" = "income_expense",
                  "Net Worth Trend"   = "net_worth",
                  "Tax Report"        = "tax",
                  "Annual Summary"    = "annual"
                ))
            ),
            column(3,
              div(class = "mt-4 d-flex gap-2",
                actionButton("rpt_generate", "Generate Report",
                             icon = icon("chart-bar"), class = "btn-primary"),
                actionButton("rpt_export_pdf", "Export PDF",
                             icon = icon("file-pdf"), class = "btn-outline-danger"),
                actionButton("rpt_export_csv", "Export CSV",
                             icon = icon("file-csv"),  class = "btn-outline-success")
              )
            )
          )
        )
      ),

      # Report output
      withSpinner(uiOutput("rpt_output"), type = 4),

      # Charts
      fluidRow(
        column(8,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              h6(class = "mb-0 fw-semibold", uiOutput("rpt_chart_title"))
            ),
            div(class = "card-body",
              withSpinner(plotlyOutput("rpt_main_chart", height = "350px"), type = 4)
            )
          )
        ),
        column(4,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              h6(class = "mb-0 fw-semibold", "Category Distribution")
            ),
            div(class = "card-body",
              withSpinner(plotlyOutput("rpt_pie_chart", height = "350px"), type = 4)
            )
          )
        )
      ),

      # Data table
      div(class = "card border-0 shadow-sm",
        div(class = "card-header bg-transparent border-0",
          h6(class = "mb-0 fw-semibold", "Detailed Data")
        ),
        div(class = "card-body p-0",
          withSpinner(DTOutput("rpt_data_table"), type = 4)
        )
      )
    ),

    # PDF download handler placeholder
    downloadButton("rpt_pdf_download", "Download PDF", style = "display:none;"),
    downloadButton("rpt_csv_download", "Download CSV", style = "display:none;")
  )
}
