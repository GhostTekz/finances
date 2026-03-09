# R/ui/settings_ui.R

settings_ui <- function() {
  tagList(
    div(class = "container-fluid py-3",
      tabsetPanel(id = "settings_tabs", type = "pills",
        tabPanel("Categories",
          div(class = "card border-0 shadow-sm mt-3",
            div(class = "card-header bg-transparent border-0",
              div(class = "d-flex justify-content-between align-items-center",
                h6(class = "mb-0 fw-semibold", "Transaction Categories"),
                actionButton("cat_add_btn", "Add Category",
                             icon = icon("plus"), class = "btn-sm btn-primary")
              )
            ),
            div(class = "card-body p-0",
              withSpinner(DTOutput("categories_table"), type = 4)
            )
          )
        ),

        tabPanel("Keyword Rules",
          div(class = "card border-0 shadow-sm mt-3",
            div(class = "card-header bg-transparent border-0",
              div(class = "d-flex justify-content-between",
                h6(class = "mb-0 fw-semibold", "Auto-Categorization Rules"),
                actionButton("rule_add_btn", "Add Rule",
                             icon = icon("plus"), class = "btn-sm btn-primary")
              )
            ),
            div(class = "card-body",
              p(class = "text-muted small",
                "When importing CSV transactions, descriptions are matched against these keywords
                 to automatically assign a category."),
              withSpinner(DTOutput("keyword_rules_table"), type = 4)
            )
          )
        ),

        tabPanel("Security",
          div(class = "card border-0 shadow-sm mt-3",
            div(class = "card-header bg-transparent border-0",
              h6(class = "mb-0 fw-semibold", "Change Master Password")
            ),
            div(class = "card-body",
              div(class = "col-md-5",
                passwordInput("sec_old_pwd",  "Current Password"),
                passwordInput("sec_new_pwd",  "New Password"),
                passwordInput("sec_conf_pwd", "Confirm New Password"),
                actionButton("sec_change_pwd_btn", "Change Password",
                             icon = icon("key"), class = "btn-warning mt-2")
              )
            )
          )
        ),

        tabPanel("Data",
          div(class = "card border-0 shadow-sm mt-3",
            div(class = "card-header bg-transparent border-0",
              h6(class = "mb-0 fw-semibold", "Backup & Restore")
            ),
            div(class = "card-body",
              fluidRow(
                column(6,
                  h6("Export"),
                  p(class = "text-muted small", "Download a full copy of your SQLite database."),
                  downloadButton("data_export_db", "Export Database (.db)",
                                  class = "btn-primary btn-sm"),
                  br(), br(),
                  downloadButton("data_export_csv_all", "Export All Transactions (.csv)",
                                  class = "btn-outline-secondary btn-sm")
                ),
                column(6,
                  h6("Restore"),
                  p(class = "text-muted small",
                    tags$strong(class = "text-danger", "Warning: "),
                    "Restoring will overwrite your current data."),
                  fileInput("data_restore_file", "Select database file (.db)",
                             accept = ".db"),
                  actionButton("data_restore_btn", "Restore Database",
                               icon = icon("upload"), class = "btn-danger btn-sm")
                )
              )
            )
          )
        ),

        tabPanel("Preferences",
          div(class = "card border-0 shadow-sm mt-3",
            div(class = "card-header bg-transparent border-0",
              h6(class = "mb-0 fw-semibold", "Application Preferences")
            ),
            div(class = "card-body",
              div(class = "col-md-4",
                textInput("pref_currency", "Currency Symbol", value = "$"),
                selectInput("pref_date_format", "Date Format",
                  choices = c("YYYY-MM-DD" = "%Y-%m-%d",
                              "MM/DD/YYYY" = "%m/%d/%Y",
                              "DD/MM/YYYY" = "%d/%m/%Y")),
                actionButton("pref_save_btn", "Save Preferences",
                             icon = icon("save"), class = "btn-primary mt-2")
              )
            )
          )
        )
      )
    ),

    # ── Category modal ──────────────────────────────────────────────────────
    hidden(div(id = "cat_modal_div",
      absolutePanel(class = "modal-overlay",
        div(class = "card shadow-lg modal-card",
          div(class = "card-header",
            div(class = "d-flex justify-content-between",
              h6(class = "mb-0", uiOutput("cat_modal_title")),
              actionButton("cat_modal_close", "", icon = icon("x"),
                           class = "btn-sm btn-outline-secondary")
            )
          ),
          div(class = "card-body",
            hidden(numericInput("cat_edit_id", NULL, value = NA)),
            fluidRow(
              column(6, textInput("cat_name_inp", "Name", placeholder = "e.g. Groceries")),
              column(6,
                selectInput("cat_type_inp", "Type",
                  choices = c("Expense" = "expense", "Income" = "income"))
              )
            ),
            fluidRow(
              column(6, textInput("cat_color_inp", "Color (hex)", value = "#6E84A3")),
              column(6, textInput("cat_icon_inp",  "Icon (FA name)", value = "tag"))
            ),
            numericInput("cat_budget_inp", "Default Budget Limit ($)", value = 0, min = 0)
          ),
          div(class = "card-footer d-flex justify-content-end gap-2",
            actionButton("cat_save_btn", "Save", icon = icon("save"), class = "btn-primary"),
            actionButton("cat_cancel_btn", "Cancel", icon = icon("times"),
                         class = "btn-outline-secondary")
          )
        )
      )
    )),

    # ── Keyword rule modal ──────────────────────────────────────────────────
    hidden(div(id = "rule_modal_div",
      absolutePanel(class = "modal-overlay",
        div(class = "card shadow-lg modal-card",
          div(class = "card-header",
            div(class = "d-flex justify-content-between",
              h6(class = "mb-0", "Add Keyword Rule"),
              actionButton("rule_modal_close", "", icon = icon("x"),
                           class = "btn-sm btn-outline-secondary")
            )
          ),
          div(class = "card-body",
            textInput("rule_keyword_inp", "Keyword", placeholder = "e.g. AMAZON, Netflix"),
            selectInput("rule_category_inp", "Assign to Category", choices = NULL),
            selectInput("rule_match_inp", "Match Type",
              choices = c("Contains" = "contains",
                          "Starts With" = "starts_with",
                          "Exact Match" = "exact"))
          ),
          div(class = "card-footer d-flex justify-content-end gap-2",
            actionButton("rule_save_btn", "Save", icon = icon("save"), class = "btn-primary"),
            actionButton("rule_cancel_btn", "Cancel", icon = icon("times"),
                         class = "btn-outline-secondary")
          )
        )
      )
    ))
  )
}
