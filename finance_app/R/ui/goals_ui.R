# R/ui/goals_ui.R

goals_ui <- function() {
  tagList(
    div(class = "container-fluid py-3",
      fluidRow(
        column(12,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              div(class = "d-flex justify-content-between align-items-center",
                h5(class = "mb-0 fw-semibold", icon("flag"), " Financial Goals"),
                actionButton("goal_add_btn", "Add Goal",
                             icon = icon("plus"), class = "btn-sm btn-primary")
              )
            ),
            div(class = "card-body",
              withSpinner(uiOutput("goals_cards"), type = 4)
            )
          )
        )
      )
    ),

    # ── Add/Edit Goal modal ─────────────────────────────────────────────────
    hidden(div(id = "goal_modal_div",
      absolutePanel(
        class = "modal-overlay",
        div(class = "card shadow-lg modal-card",
          div(class = "card-header",
            div(class = "d-flex justify-content-between",
              h6(class = "mb-0", uiOutput("goal_modal_title")),
              actionButton("goal_modal_close", "", icon = icon("x"),
                           class = "btn-sm btn-outline-secondary")
            )
          ),
          div(class = "card-body",
            hidden(numericInput("goal_edit_id", NULL, value = NA)),
            fluidRow(
              column(6, textInput("goal_name_inp", "Goal Name",
                                   placeholder = "e.g. Emergency Fund")),
              column(6,
                selectInput("goal_cat_inp", "Category",
                  choices = c("Emergency Fund" = "emergency",
                              "Vacation"        = "vacation",
                              "Home"            = "home",
                              "Retirement"      = "retirement",
                              "Debt Payoff"     = "debt",
                              "Other"           = "other"))
              )
            ),
            fluidRow(
              column(6, numericInput("goal_target_inp", "Target Amount ($)",
                                      value = 1000, min = 0, step = 100)),
              column(6, numericInput("goal_current_inp", "Current Amount ($)",
                                      value = 0, min = 0, step = 0.01))
            ),
            fluidRow(
              column(6, dateInput("goal_date_inp", "Target Date",
                                   value = Sys.Date() + 365)),
              column(6, selectInput("goal_account_inp", "Linked Account", choices = NULL))
            ),
            textAreaInput("goal_notes_inp", "Notes", rows = 2)
          ),
          div(class = "card-footer d-flex justify-content-end gap-2",
            actionButton("goal_save_btn", "Save", icon = icon("save"), class = "btn-primary"),
            actionButton("goal_cancel_btn", "Cancel", icon = icon("times"),
                         class = "btn-outline-secondary")
          )
        )
      )
    )),

    # ── Add Contribution modal ─────────────────────────────────────────────
    hidden(div(id = "goal_contrib_div",
      absolutePanel(
        class = "modal-overlay",
        div(class = "card shadow-lg modal-card",
          div(class = "card-header",
            div(class = "d-flex justify-content-between",
              h6(class = "mb-0", icon("circle-plus"), " Add Contribution"),
              actionButton("goal_contrib_close", "", icon = icon("x"),
                           class = "btn-sm btn-outline-secondary")
            )
          ),
          div(class = "card-body",
            hidden(numericInput("goal_contrib_goal_id", NULL, value = NA)),
            numericInput("goal_contrib_amount", "Amount ($)", value = 0, min = 0.01, step = 0.01),
            dateInput("goal_contrib_date", "Date", value = Sys.Date()),
            textInput("goal_contrib_notes", "Notes", placeholder = "Optional…")
          ),
          div(class = "card-footer d-flex justify-content-end gap-2",
            actionButton("goal_contrib_save", "Add Contribution",
                         icon = icon("circle-plus"), class = "btn-success"),
            actionButton("goal_contrib_cancel", "Cancel",
                         icon = icon("times"), class = "btn-outline-secondary")
          )
        )
      )
    ))
  )
}
