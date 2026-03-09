# R/ui/vault_ui.R

vault_ui <- function() {
  tagList(
    div(class = "container-fluid py-3",
      # Security banner
      div(class = "alert alert-info d-flex align-items-center gap-2 mb-3",
        icon("shield-halved"),
        div("All passwords are encrypted with AES-256-CBC. The encryption key is derived
             from your master password and never written to disk.")
      ),

      fluidRow(
        column(12,
          div(class = "card border-0 shadow-sm mb-3",
            div(class = "card-header bg-transparent border-0",
              div(class = "d-flex justify-content-between align-items-center",
                h5(class = "mb-0 fw-semibold", icon("shield-halved"), " Password Vault"),
                div(class = "d-flex gap-2",
                  textInput("vault_search", NULL, placeholder = "Search vault…",
                             width = "200px"),
                  selectInput("vault_filter_cat", NULL,
                               choices = c("All Categories" = "",
                                           "Banking" = "banking",
                                           "Investment" = "investment",
                                           "Insurance" = "insurance",
                                           "Utilities" = "utilities",
                                           "Other" = "other"),
                               width = "150px"),
                  actionButton("vault_add_btn", "Add Entry",
                               icon = icon("plus"), class = "btn-sm btn-primary")
                )
              )
            ),
            div(class = "card-body p-0",
              withSpinner(uiOutput("vault_entries_list"), type = 4)
            )
          )
        )
      )
    ),

    # ── Add/Edit Vault Entry modal ──────────────────────────────────────────
    hidden(div(id = "vault_modal_div",
      absolutePanel(
        class = "modal-overlay",
        div(class = "card shadow-lg modal-card-wide",
          div(class = "card-header",
            div(class = "d-flex justify-content-between",
              h6(class = "mb-0", icon("lock"), " ", uiOutput("vault_modal_title")),
              actionButton("vault_modal_close", "", icon = icon("x"),
                           class = "btn-sm btn-outline-secondary")
            )
          ),
          div(class = "card-body",
            hidden(numericInput("vault_edit_id", NULL, value = NA)),
            fluidRow(
              column(6, textInput("vault_institution_inp", "Institution / Site Name",
                                   placeholder = "e.g. Chase Bank")),
              column(6,
                selectInput("vault_category_inp", "Category",
                  choices = c("Banking" = "banking", "Investment" = "investment",
                              "Insurance" = "insurance", "Utilities" = "utilities",
                              "Other" = "other"))
              )
            ),
            fluidRow(
              column(6, textInput("vault_username_inp", "Username / Email",
                                   placeholder = "your@email.com")),
              column(6, textInput("vault_url_inp", "URL",
                                   placeholder = "https://bank.com/login"))
            ),
            div(class = "mb-3",
              label("Password"),
              div(class = "input-group",
                passwordInput("vault_password_inp", NULL,
                               placeholder = "Enter or generate password"),
                actionButton("vault_show_pwd", "", icon = icon("eye"),
                             class = "btn btn-outline-secondary"),
                actionButton("vault_gen_btn", "Generate",
                             icon = icon("dice"), class = "btn btn-outline-primary")
              )
            ),
            # Password generator options
            hidden(div(id = "vault_gen_options",
              class = "border rounded p-2 mb-2 bg-light",
              fluidRow(
                column(3, numericInput("vault_gen_length", "Length",
                                        value = 20, min = 8, max = 64, step = 1)),
                column(2, checkboxInput("vault_gen_upper",   "A-Z",    TRUE)),
                column(2, checkboxInput("vault_gen_lower",   "a-z",    TRUE)),
                column(2, checkboxInput("vault_gen_digits",  "0-9",    TRUE)),
                column(3, checkboxInput("vault_gen_symbols", "Symbols",TRUE))
              )
            )),
            textAreaInput("vault_notes_inp", "Notes (encrypted)",
                           rows = 2, placeholder = "Account #, PIN, security Q&A…")
          ),
          div(class = "card-footer d-flex justify-content-end gap-2",
            actionButton("vault_save_btn", "Save Encrypted",
                         icon = icon("lock"), class = "btn-success"),
            actionButton("vault_cancel_btn", "Cancel",
                         icon = icon("times"), class = "btn-outline-secondary")
          )
        )
      )
    )),

    # ── View password modal (read-only) ─────────────────────────────────────
    hidden(div(id = "vault_view_div",
      absolutePanel(
        class = "modal-overlay",
        div(class = "card shadow-lg modal-card",
          div(class = "card-header",
            div(class = "d-flex justify-content-between",
              h6(class = "mb-0", icon("eye"), " View Entry"),
              actionButton("vault_view_close", "", icon = icon("x"),
                           class = "btn-sm btn-outline-secondary")
            )
          ),
          div(class = "card-body",
            uiOutput("vault_view_content")
          )
        )
      )
    ))
  )
}
