# R/server/accounts_server.R

accounts_server <- function(input, output, session, rv) {
  accounts_data <- reactive({
    input$acct_save_btn
    db_get_accounts(rv$db)
  })

  # ── Net worth banner ───────────────────────────────────────────────────────
  output$acct_net_worth_banner <- renderUI({
    nw <- db_get_net_worth(rv$db)
    fmt_currency(nw)
  })

  # ── Account cards ──────────────────────────────────────────────────────────
  output$accounts_cards <- renderUI({
    df <- accounts_data()
    if (nrow(df) == 0) {
      return(div(class = "p-3 text-muted", "No accounts added yet."))
    }
    type_icons <- list(
      checking   = "building-columns",
      savings    = "piggy-bank",
      credit     = "credit-card",
      investment = "chart-line",
      loan       = "hand-holding-dollar",
      cash       = "money-bill"
    )
    tagList(lapply(seq_len(nrow(df)), function(i) {
      acct  <- df[i, ]
      icon_name <- type_icons[[acct$type]] %||% "wallet"
      bal_cls   <- if (acct$balance >= 0) "text-success" else "text-danger"
      div(class = "d-flex justify-content-between align-items-center p-3 border-bottom",
        div(class = "d-flex align-items-center gap-3",
          div(class = "bg-primary bg-opacity-10 rounded p-2",
            icon(icon_name, class = "text-primary")),
          div(
            div(class = "fw-semibold", acct$name),
            div(class = "text-muted small",
              stringr::str_to_title(acct$type),
              if (!is.na(acct$interest_rate) && acct$interest_rate > 0)
                paste0(" · ", acct$interest_rate, "% APR")),
            if (!is.na(acct$notes) && nchar(acct$notes) > 0)
              div(class = "text-muted", style = "font-size:0.75rem", acct$notes)
          )
        ),
        div(class = "d-flex align-items-center gap-2",
          h5(class = paste("mb-0", bal_cls), fmt_currency(acct$balance)),
          actionButton(paste0("acct_edit_", acct$id), "",
                        icon = icon("pen"), class = "btn-sm btn-outline-primary",
                        onclick = sprintf("Shiny.setInputValue('acct_edit_id_click',%d,{priority:'event'})", acct$id)),
          actionButton(paste0("acct_del_", acct$id), "",
                        icon = icon("trash"), class = "btn-sm btn-outline-danger",
                        onclick = sprintf("Shiny.setInputValue('acct_delete_id',%d,{priority:'event'})", acct$id))
        )
      )
    }))
  })

  # ── Type breakdown chart ───────────────────────────────────────────────────
  output$acct_type_chart <- renderPlotly({
    df <- accounts_data()
    if (nrow(df) == 0) return(plotly_empty_msg("No accounts."))
    by_type <- df %>%
      group_by(type) %>%
      summarise(balance = sum(balance, na.rm = TRUE), .groups = "drop")
    plot_ly(by_type, labels = ~stringr::str_to_title(type),
            values = ~balance, type = "pie",
            textinfo = "label+percent",
            marker = list(colors = c("#2C7BE5","#00D97E","#E63757","#F6C343","#6E84A3","#4ECDC4"))) %>%
      layout(showlegend = FALSE, margin = list(t = 0, b = 0, l = 0, r = 0),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  # ── Net worth over time (approximate from transactions) ────────────────────
  output$acct_networth_chart <- renderPlotly({
    df <- dbGetQuery(rv$db, "
      SELECT strftime('%Y-%m', date) AS month,
             SUM(CASE WHEN c.type='income'  THEN  ABS(t.amount) ELSE 0 END) -
             SUM(CASE WHEN c.type='expense' THEN  ABS(t.amount) ELSE 0 END) AS net
      FROM transactions t
      JOIN categories c ON t.category_id = c.id
      WHERE t.date >= date('now', '-12 months')
      GROUP BY month ORDER BY month")
    if (nrow(df) == 0) return(plotly_empty_msg("No data."))
    df$cumnet <- cumsum(df$net)
    plot_ly(df, x = ~month, y = ~cumnet, type = "scatter", mode = "lines+markers",
            line = list(color = "#2C7BE5", width = 2),
            marker = list(color = "#2C7BE5")) %>%
      layout(xaxis = list(title = ""), yaxis = list(title = ""),
             paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")
  })

  # ── Add modal ──────────────────────────────────────────────────────────────
  observeEvent(input$acct_add_btn, {
    updateNumericInput(session, "acct_edit_id", value = NA)
    updateTextInput(session, "acct_name_inp", value = "")
    updateSelectInput(session, "acct_type_inp", selected = "checking")
    updateNumericInput(session, "acct_balance_inp", value = 0)
    updateTextInput(session, "acct_currency_inp", value = "USD")
    updateNumericInput(session, "acct_rate_inp", value = 0)
    updateTextAreaInput(session, "acct_notes_inp", value = "")
    shinyjs::show("acct_modal_div")
  })
  output$acct_modal_title <- renderUI({
    if (!is.null(input$acct_edit_id) && !is.na(input$acct_edit_id)) "Edit Account" else "Add Account"
  })

  # ── Edit ───────────────────────────────────────────────────────────────────
  observeEvent(input$acct_edit_id_click, {
    id  <- as.integer(input$acct_edit_id_click)
    row <- dbGetQuery(rv$db, sprintf("SELECT * FROM accounts WHERE id=%d", id))
    if (nrow(row) == 0) return()
    updateNumericInput(session, "acct_edit_id",      value = id)
    updateTextInput(session,    "acct_name_inp",      value = row$name[1])
    updateSelectInput(session,  "acct_type_inp",      selected = row$type[1])
    updateNumericInput(session, "acct_balance_inp",   value = row$balance[1])
    updateTextInput(session,    "acct_currency_inp",  value = row$currency[1] %||% "USD")
    updateNumericInput(session, "acct_rate_inp",      value = row$interest_rate[1] %||% 0)
    updateTextAreaInput(session,"acct_notes_inp",     value = row$notes[1] %||% "")
    shinyjs::show("acct_modal_div")
  })

  # ── Save ───────────────────────────────────────────────────────────────────
  observeEvent(input$acct_save_btn, {
    name <- trimws(input$acct_name_inp %||% "")
    if (nchar(name) == 0) {
      shinyalert("Validation", "Account name is required.", type = "warning")
      return()
    }
    edit_id <- input$acct_edit_id
    tryCatch({
      if (!is.null(edit_id) && !is.na(edit_id)) {
        db_update_account(rv$db, as.integer(edit_id), name, input$acct_type_inp,
                           as.numeric(input$acct_balance_inp %||% 0),
                           input$acct_currency_inp %||% "USD",
                           as.numeric(input$acct_rate_inp %||% 0),
                           input$acct_notes_inp %||% "")
      } else {
        db_add_account(rv$db, name, input$acct_type_inp,
                        as.numeric(input$acct_balance_inp %||% 0),
                        input$acct_currency_inp %||% "USD",
                        as.numeric(input$acct_rate_inp %||% 0),
                        input$acct_notes_inp %||% "")
      }
      shinyjs::hide("acct_modal_div")
      shinyalert("Saved", "Account saved.", type = "success")
    }, error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
  })

  # ── Delete ─────────────────────────────────────────────────────────────────
  observeEvent(input$acct_delete_id, {
    id <- as.integer(input$acct_delete_id)
    shinyalert("Delete Account?",
               "This will remove the account. Transactions will remain. Proceed?",
               type = "warning", showCancelButton = TRUE,
               callbackR = function(x) {
                 if (isTRUE(x)) {
                   tryCatch(db_delete_account(rv$db, id),
                            error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
                 }
               })
  })

  observeEvent(input$acct_modal_close, shinyjs::hide("acct_modal_div"))
  observeEvent(input$acct_cancel_btn,  shinyjs::hide("acct_modal_div"))
}
