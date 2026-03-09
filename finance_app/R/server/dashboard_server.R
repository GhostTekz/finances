# R/server/dashboard_server.R

dashboard_server <- function(input, output, session, rv) {
  # ── KPI: Net Worth ─────────────────────────────────────────────────────────
  output$dash_net_worth <- renderUI({
    nw <- db_get_net_worth(rv$db)
    span(class = if (nw >= 0) "text-success" else "text-danger", fmt_currency(nw))
  })

  # ── KPI: Monthly Income ────────────────────────────────────────────────────
  output$dash_monthly_income <- renderUI({
    m  <- as.integer(format(Sys.Date(), "%m"))
    y  <- as.integer(format(Sys.Date(), "%Y"))
    df <- dbGetQuery(rv$db, sprintf("
      SELECT COALESCE(SUM(ABS(t.amount)),0) AS total
      FROM transactions t
      JOIN categories c ON t.category_id = c.id
      WHERE c.type = 'income'
        AND strftime('%%m', t.date) = '%02d'
        AND strftime('%%Y', t.date) = '%d'", m, y))
    span(class = "text-primary", fmt_currency(df$total[1]))
  })

  # ── KPI: Monthly Expenses ──────────────────────────────────────────────────
  output$dash_monthly_expenses <- renderUI({
    m  <- as.integer(format(Sys.Date(), "%m"))
    y  <- as.integer(format(Sys.Date(), "%Y"))
    df <- dbGetQuery(rv$db, sprintf("
      SELECT COALESCE(SUM(ABS(t.amount)),0) AS total
      FROM transactions t
      JOIN categories c ON t.category_id = c.id
      WHERE c.type = 'expense'
        AND strftime('%%m', t.date) = '%02d'
        AND strftime('%%Y', t.date) = '%d'", m, y))
    span(class = "text-danger", fmt_currency(df$total[1]))
  })

  # ── KPI: Savings Rate ──────────────────────────────────────────────────────
  output$dash_savings_rate <- renderUI({
    m <- as.integer(format(Sys.Date(), "%m"))
    y <- as.integer(format(Sys.Date(), "%Y"))
    df <- dbGetQuery(rv$db, sprintf("
      SELECT c.type, COALESCE(SUM(ABS(t.amount)),0) AS total
      FROM transactions t
      JOIN categories c ON t.category_id = c.id
      WHERE strftime('%%m', t.date) = '%02d'
        AND strftime('%%Y', t.date) = '%d'
      GROUP BY c.type", m, y))
    inc <- df$total[df$type == "income"]  %||% 0
    exp <- df$total[df$type == "expense"] %||% 0
    rate <- if (inc > 0) round((inc - exp) / inc * 100, 1) else 0
    cls  <- if (rate >= 0) "text-info" else "text-danger"
    span(class = cls, paste0(rate, "%"))
  })

  # ── KPI: Bills Due ─────────────────────────────────────────────────────────
  output$dash_bills_due <- renderUI({
    bills <- db_get_bills(rv$db)
    due   <- bills[!is.na(bills$due_date) &
                   as.Date(bills$due_date) <= Sys.Date() + 7 &
                   as.Date(bills$due_date) >= Sys.Date(), ]
    total <- if (nrow(due) > 0) sum(due$amount, na.rm = TRUE) else 0
    span(class = "text-warning",
      paste0(nrow(due), " (", fmt_currency(total), ")"))
  })

  # ── KPI: Active Goals ─────────────────────────────────────────────────────
  output$dash_active_goals <- renderUI({
    goals <- db_get_goals(rv$db)
    n     <- nrow(goals)
    span(class = "text-secondary", n)
  })

  # ── Chart: Income vs Expenses (6 months) ──────────────────────────────────
  output$dash_income_expense_chart <- renderPlotly({
    df <- dbGetQuery(rv$db, "
      SELECT strftime('%Y-%m', t.date) AS month,
             c.type,
             COALESCE(SUM(ABS(t.amount)),0) AS total
      FROM transactions t
      JOIN categories c ON t.category_id = c.id
      WHERE t.date >= date('now', '-6 months')
      GROUP BY strftime('%Y-%m', t.date), c.type
      ORDER BY month")
    if (nrow(df) == 0) {
      return(plotly_empty_msg("No transaction data yet."))
    }
    chart_income_expense_bar(df)
  })

  # ── Chart: Category Pie ────────────────────────────────────────────────────
  output$dash_category_pie <- renderPlotly({
    m  <- as.integer(format(Sys.Date(), "%m"))
    y  <- as.integer(format(Sys.Date(), "%Y"))
    df <- dbGetQuery(rv$db, sprintf("
      SELECT c.name AS category, c.color,
             COALESCE(SUM(ABS(t.amount)),0) AS total
      FROM transactions t
      JOIN categories c ON t.category_id = c.id
      WHERE c.type = 'expense'
        AND strftime('%%m', t.date) = '%02d'
        AND strftime('%%Y', t.date) = '%d'
      GROUP BY c.name, c.color
      ORDER BY total DESC", m, y))
    if (nrow(df) == 0) return(plotly_empty_msg("No expense data this month."))
    chart_category_pie(df)
  })

  # ── Budget gauges ─────────────────────────────────────────────────────────
  output$dash_budget_gauges <- renderUI({
    m  <- as.integer(format(Sys.Date(), "%m"))
    y  <- as.integer(format(Sys.Date(), "%Y"))
    bva <- db_get_budget_vs_actual(rv$db, m, y)
    bva <- bva[bva$budgeted > 0, ]
    if (nrow(bva) == 0) return(p(class = "text-muted", "No budgets set for this month."))
    tagList(lapply(seq_len(min(nrow(bva), 8)), function(i) {
      pct   <- min(round(bva$spent[i] / bva$budgeted[i] * 100), 100)
      color <- if (pct >= 100) "danger" else if (pct >= 80) "warning" else "success"
      div(class = "mb-2",
        div(class = "d-flex justify-content-between small",
          span(bva$category[i]),
          span(paste0(fmt_currency(bva$spent[i]), " / ", fmt_currency(bva$budgeted[i])))
        ),
        div(class = "progress", style = "height:8px;",
          div(class = paste0("progress-bar bg-", color),
              style = paste0("width:", pct, "%"),
              role = "progressbar")
        )
      )
    }))
  })

  # ── Upcoming bills widget ─────────────────────────────────────────────────
  output$dash_upcoming_bills <- renderUI({
    bills <- db_get_bills(rv$db)
    up    <- bills[!is.na(bills$due_date) &
                   as.Date(bills$due_date) >= Sys.Date() &
                   as.Date(bills$due_date) <= Sys.Date() + 7, ]
    up    <- up[order(as.Date(up$due_date)), ]
    if (nrow(up) == 0) return(p(class = "text-muted small p-2", "No bills due in next 7 days."))
    tagList(lapply(seq_len(nrow(up)), function(i) {
      days_left <- as.integer(as.Date(up$due_date[i]) - Sys.Date())
      badge_cls <- if (days_left == 0) "bg-danger" else if (days_left <= 2) "bg-warning" else "bg-info"
      div(class = "d-flex justify-content-between align-items-center p-2 border-bottom",
        div(
          div(class = "fw-semibold small", up$name[i]),
          div(class = "text-muted", style = "font-size:0.75rem",
            format(as.Date(up$due_date[i]), "%b %d"))
        ),
        div(
          span(class = paste("badge", badge_cls), paste0(days_left, "d")),
          div(class = "fw-bold small text-end", fmt_currency(up$amount[i]))
        )
      )
    }))
  })

  # ── Recent Transactions ───────────────────────────────────────────────────
  output$dash_recent_transactions <- renderUI({
    tx <- db_get_transactions(rv$db, limit = 8)
    if (nrow(tx) == 0) return(p(class = "text-muted small p-2", "No transactions yet."))
    tagList(lapply(seq_len(nrow(tx)), function(i) {
      is_income <- !is.na(tx$category_type[i]) && tx$category_type[i] == "income"
      amt_cls   <- if (is_income) "text-success" else "text-danger"
      amt_sign  <- if (is_income) "+" else "-"
      div(class = "d-flex justify-content-between align-items-center p-2 border-bottom",
        div(
          div(class = "small fw-semibold",
              if (nchar(tx$description[i] %||% "") > 0) tx$description[i] else
                tx$category_name[i] %||% "Unknown"),
          div(class = "text-muted", style = "font-size:0.75rem",
            tx$category_name[i] %||% "", " · ",
            format(as.Date(tx$date[i]), "%b %d"))
        ),
        span(class = paste("fw-bold small", amt_cls),
          paste0(amt_sign, fmt_currency(abs(tx$amount[i]))))
      )
    }))
  })
}
