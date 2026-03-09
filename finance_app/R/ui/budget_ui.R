# R/ui/budget_ui.R

budget_ui <- function() {
  tagList(
    div(class = "container-fluid py-3",
      # Month selector + actions
      fluidRow(
        column(12,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              div(class = "d-flex justify-content-between align-items-center",
                h5(class = "mb-0 fw-semibold", icon("bullseye"), " Budget Manager"),
                div(class = "d-flex gap-2 align-items-center",
                  div(class = "input-group input-group-sm",
                    tags$select(
                      id = "budget_month",
                      class = "form-select form-select-sm",
                      lapply(1:12, function(m)
                        tags$option(value = m,
                          selected = if (m == as.integer(format(Sys.Date(), "%m"))) "selected" else NULL,
                          format(as.Date(paste0("2000-", sprintf("%02d", m), "-01")), "%B"))
                      )
                    ),
                    tags$select(
                      id = "budget_year",
                      class = "form-select form-select-sm",
                      lapply(seq(year(Sys.Date()) - 2, year(Sys.Date()) + 1), function(y)
                        tags$option(value = y,
                          selected = if (y == year(Sys.Date())) "selected" else NULL,
                          y)
                      )
                    )
                  ),
                  actionButton("budget_copy_last", "Copy Last Month",
                               icon = icon("copy"), class = "btn-sm btn-outline-secondary"),
                  actionButton("budget_add_btn", "Add Budget",
                               icon = icon("plus"), class = "btn-sm btn-primary")
                )
              )
            ),
            div(class = "card-body",
              # Summary bar
              uiOutput("budget_summary_cards"),
              hr(),
              # Progress bars
              withSpinner(uiOutput("budget_progress_list"), type = 4)
            )
          )
        )
      ),

      # Annual view
      fluidRow(
        column(12,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              div(class = "d-flex justify-content-between",
                h6(class = "mb-0 fw-semibold", "Annual Budget Overview"),
                numericInput("budget_annual_year", NULL, value = year(Sys.Date()),
                             min = 2000, max = 2100, width = "100px")
              )
            ),
            div(class = "card-body p-0",
              withSpinner(DTOutput("budget_annual_table"), type = 4)
            )
          )
        )
      )
    ),

    # ── Add/Edit budget modal ───────────────────────────────────────────────
    hidden(div(id = "budget_modal_div",
      absolutePanel(
        class = "modal-overlay",
        div(class = "card shadow-lg modal-card",
          div(class = "card-header",
            div(class = "d-flex justify-content-between",
              h6(class = "mb-0", uiOutput("budget_modal_title")),
              actionButton("budget_modal_close", "", icon = icon("x"),
                           class = "btn-sm btn-outline-secondary")
            )
          ),
          div(class = "card-body",
            hidden(numericInput("budget_edit_id", NULL, value = NA)),
            selectInput("budget_category_sel", "Category", choices = NULL),
            numericInput("budget_amount_inp", "Budget Amount ($)",
                          value = 100, min = 0, step = 10),
            checkboxInput("budget_rollover_chk", "Roll over unused budget to next month", FALSE)
          ),
          div(class = "card-footer d-flex justify-content-end gap-2",
            actionButton("budget_save_btn", "Save", icon = icon("save"),
                         class = "btn-primary"),
            actionButton("budget_cancel_btn", "Cancel", icon = icon("times"),
                         class = "btn-outline-secondary")
          )
        )
      )
    ))
  )
}
