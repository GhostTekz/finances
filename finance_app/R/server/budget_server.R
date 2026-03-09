# R/server/budget_server.R

budget_server <- function(input, output, session, rv) {
  cur_month <- reactive({ as.integer(input$budget_month %||% format(Sys.Date(), "%m")) })
  cur_year  <- reactive({ as.integer(input$budget_year  %||% format(Sys.Date(), "%Y")) })

  # Populate category selector in modal
  observe({
    cats <- db_get_categories(rv$db, type = "expense")
    updateSelectInput(session, "budget_category_sel",
                       choices = setNames(cats$id, cats$name))
  })

  # ── Budget summary cards ────────────────────────────────────────────────────
  output$budget_summary_cards <- renderUI({
    bva <- db_get_budget_vs_actual(rv$db, cur_month(), cur_year())
    total_bud  <- sum(bva$budgeted, na.rm = TRUE)
    total_spent <- sum(bva$spent,   na.rm = TRUE)
    remaining  <- total_bud - total_spent
    div(class = "d-flex gap-3 mb-3",
      div(class = "card bg-primary text-white p-2 flex-fill text-center",
        div(class = "small", "Total Budgeted"),
        h5(class = "mb-0", fmt_currency(total_bud))
      ),
      div(class = "card bg-danger text-white p-2 flex-fill text-center",
        div(class = "small", "Total Spent"),
        h5(class = "mb-0", fmt_currency(total_spent))
      ),
      div(class = if (remaining >= 0) "card bg-success text-white p-2 flex-fill text-center"
                  else "card bg-warning text-dark p-2 flex-fill text-center",
        div(class = "small", "Remaining"),
        h5(class = "mb-0", fmt_currency(remaining))
      )
    )
  })

  # ── Budget progress list ────────────────────────────────────────────────────
  output$budget_progress_list <- renderUI({
    bva <- db_get_budget_vs_actual(rv$db, cur_month(), cur_year())
    if (nrow(bva) == 0) return(p(class="text-muted","No budgets configured for this month."))
    tagList(lapply(seq_len(nrow(bva)), function(i) {
      pct    <- if (bva$budgeted[i] > 0) min(round(bva$spent[i] / bva$budgeted[i] * 100), 100) else 0
      over   <- bva$spent[i] > bva$budgeted[i]
      color  <- if (over) "danger" else if (pct >= 80) "warning" else "success"
      div(class = "mb-3 p-2 border rounded",
        div(class = "d-flex justify-content-between align-items-center mb-1",
          span(class = "fw-semibold", bva$category[i]),
          div(class = "d-flex gap-2 align-items-center",
            if (over) span(class = "badge bg-danger", "Over budget!") else NULL,
            span(class = "small text-muted",
              fmt_currency(bva$spent[i]), " of ", fmt_currency(bva$budgeted[i])),
            span(class = paste0("badge bg-", color), paste0(pct, "%"))
          )
        ),
        div(class = "progress", style = "height:12px;",
          div(class = paste0("progress-bar bg-", color),
              style = paste0("width:", pct, "%;"),
              role  = "progressbar",
              `aria-valuenow` = pct, `aria-valuemin` = 0, `aria-valuemax` = 100)
        ),
        if (bva$budgeted[i] > 0) {
          remaining <- bva$budgeted[i] - bva$spent[i]
          div(class = "text-muted mt-1", style = "font-size:0.75rem",
            if (remaining >= 0)
              paste0(fmt_currency(remaining), " remaining")
            else
              paste0(fmt_currency(abs(remaining)), " over budget")
          )
        }
      )
    }))
  })

  # ── Annual budget table ────────────────────────────────────────────────────
  output$budget_annual_table <- renderDT({
    yr    <- as.integer(input$budget_annual_year %||% year(Sys.Date()))
    months <- 1:12
    cats  <- db_get_categories(rv$db, type = "expense")
    if (nrow(cats) == 0) return(datatable(data.frame()))

    mat <- do.call(rbind, lapply(months, function(m) {
      bva <- db_get_budget_vs_actual(rv$db, m, yr)
      row <- setNames(rep(0, nrow(cats)), cats$name)
      for (k in seq_len(nrow(bva))) {
        if (bva$category[k] %in% names(row)) row[bva$category[k]] <- bva$spent[k]
      }
      row
    }))
    df <- as.data.frame(mat)
    df <- cbind(Month = month.abb, df)
    datatable(df, options = list(pageLength = 12, dom = "t"), rownames = FALSE)
  })

  # ── Open Add Budget modal ──────────────────────────────────────────────────
  observeEvent(input$budget_add_btn, {
    updateNumericInput(session, "budget_edit_id", value = NA)
    updateNumericInput(session, "budget_amount_inp", value = 100)
    updateCheckboxInput(session, "budget_rollover_chk", value = FALSE)
    shinyjs::show("budget_modal_div")
  })
  output$budget_modal_title <- renderUI({ "Add / Edit Budget" })

  observeEvent(input$budget_save_btn, {
    cat_id  <- as.integer(input$budget_category_sel)
    amount  <- as.numeric(input$budget_amount_inp %||% 0)
    rollover <- as.integer(input$budget_rollover_chk)
    if (is.na(cat_id) || amount < 0) {
      shinyalert("Validation", "Please select a category and valid amount.", type = "warning")
      return()
    }
    tryCatch({
      db_upsert_budget(rv$db, cat_id, cur_month(), cur_year(), amount, rollover)
      shinyjs::hide("budget_modal_div")
      shinyalert("Saved", "Budget saved.", type = "success")
    }, error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
  })

  # ── Copy last month ────────────────────────────────────────────────────────
  observeEvent(input$budget_copy_last, {
    m <- cur_month(); y <- cur_year()
    prev_m <- if (m == 1) 12 else m - 1
    prev_y <- if (m == 1) y - 1 else y
    prev <- dbGetQuery(rv$db, sprintf(
      "SELECT * FROM budgets WHERE month=%d AND year=%d", prev_m, prev_y))
    if (nrow(prev) == 0) {
      shinyalert("Info", "No budgets found for previous month.", type = "info")
      return()
    }
    tryCatch({
      for (i in seq_len(nrow(prev))) {
        db_upsert_budget(rv$db, prev$category_id[i], m, y, prev$amount[i], prev$rollover[i])
      }
      shinyalert("Copied", paste0(nrow(prev), " budgets copied from previous month."),
                  type = "success")
    }, error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
  })

  observeEvent(input$budget_modal_close, shinyjs::hide("budget_modal_div"))
  observeEvent(input$budget_cancel_btn,  shinyjs::hide("budget_modal_div"))
}
