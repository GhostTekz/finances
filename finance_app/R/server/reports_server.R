# R/server/reports_server.R

reports_server <- function(input, output, session, rv) {
  observe({
    accts <- db_get_accounts(rv$db)
    choices <- c("All Accounts" = "", setNames(accts$id, accts$name))
    updateSelectInput(session, "rpt_account", choices = choices)
  })

  rpt_df <- eventReactive(input$rpt_generate, {
    dfrom <- as.character(input$rpt_date_range[1])
    dto   <- as.character(input$rpt_date_range[2])
    acct  <- if (nchar(input$rpt_account %||% "") > 0)
               as.integer(input$rpt_account) else NULL
    db_get_transactions(rv$db, limit = 9999,
                         date_from = dfrom, date_to = dto, account_id = acct)
  }, ignoreNULL = FALSE)

  # ── Summary output ─────────────────────────────────────────────────────────
  output$rpt_output <- renderUI({
    df   <- rpt_df()
    type <- input$rpt_type %||% "monthly"
    if (nrow(df) == 0) return(div(class = "alert alert-info", "No data for selected range."))

    inc <- sum(abs(df$amount[!is.na(df$category_type) & df$category_type == "income"]), na.rm = TRUE)
    exp <- sum(abs(df$amount[!is.na(df$category_type) & df$category_type == "expense"]), na.rm = TRUE)
    net <- inc - exp
    tax <- sum(abs(df$amount[!is.na(df$tax_deductible) & df$tax_deductible == 1]), na.rm = TRUE)

    div(class = "row mb-3",
      column(3,
        div(class = "card bg-primary text-white p-3 text-center",
          div(class = "small", "Total Income"),
          h4(class = "mb-0", fmt_currency(inc)))),
      column(3,
        div(class = "card bg-danger text-white p-3 text-center",
          div(class = "small", "Total Expenses"),
          h4(class = "mb-0", fmt_currency(exp)))),
      column(3,
        div(class = if (net >= 0) "card bg-success text-white p-3 text-center"
                    else "card bg-warning text-dark p-3 text-center",
          div(class = "small", "Net"),
          h4(class = "mb-0", fmt_currency(net)))),
      column(3,
        div(class = "card bg-info text-white p-3 text-center",
          div(class = "small", "Tax Deductible"),
          h4(class = "mb-0", fmt_currency(tax))))
    )
  })

  output$rpt_chart_title <- renderUI({
    switch(input$rpt_type %||% "monthly",
      monthly        = "Monthly Spending",
      category       = "Spending by Category",
      income_expense = "Income vs. Expenses",
      net_worth      = "Net Worth Trend",
      tax            = "Tax-Deductible Expenses",
      annual         = "Annual Summary",
      "Report"
    )
  })

  # ── Main chart ─────────────────────────────────────────────────────────────
  output$rpt_main_chart <- renderPlotly({
    df   <- rpt_df()
    type <- input$rpt_type %||% "monthly"
    if (nrow(df) == 0) return(plotly_empty_msg("No data."))

    if (type == "monthly") {
      agg <- df %>%
        filter(!is.na(category_type), category_type == "expense") %>%
        mutate(month = format(as.Date(date), "%Y-%m")) %>%
        group_by(month) %>%
        summarise(total = sum(abs(amount), na.rm = TRUE), .groups = "drop")
      plot_ly(agg, x = ~month, y = ~total, type = "bar",
              marker = list(color = "#E63757")) %>%
        layout(xaxis = list(title = "Month"), yaxis = list(title = "Amount ($)"),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")

    } else if (type == "income_expense") {
      agg <- df %>%
        filter(!is.na(category_type)) %>%
        mutate(month = format(as.Date(date), "%Y-%m")) %>%
        group_by(month, category_type) %>%
        summarise(total = sum(abs(amount), na.rm = TRUE), .groups = "drop")
      chart_income_expense_bar(agg)

    } else if (type == "category") {
      agg <- df %>%
        filter(!is.na(category_type), category_type == "expense") %>%
        group_by(category_name) %>%
        summarise(total = sum(abs(amount), na.rm = TRUE), .groups = "drop") %>%
        arrange(desc(total))
      plot_ly(agg, x = ~total, y = ~reorder(category_name, total),
              type = "bar", orientation = "h",
              marker = list(color = "#2C7BE5")) %>%
        layout(xaxis = list(title = "Amount ($)"), yaxis = list(title = ""),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")

    } else if (type == "tax") {
      tax_df <- df %>%
        filter(tax_deductible == 1, !is.na(category_type), category_type == "expense") %>%
        group_by(category_name) %>%
        summarise(total = sum(abs(amount), na.rm = TRUE), .groups = "drop")
      if (nrow(tax_df) == 0) return(plotly_empty_msg("No tax-deductible expenses."))
      plot_ly(tax_df, x = ~total, y = ~reorder(category_name, total),
              type = "bar", orientation = "h",
              marker = list(color = "#F6C343")) %>%
        layout(xaxis = list(title = "Amount ($)"), yaxis = list(title = ""),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")

    } else if (type == "net_worth") {
      agg <- df %>%
        filter(!is.na(category_type)) %>%
        mutate(month = format(as.Date(date), "%Y-%m"),
               signed = ifelse(category_type == "income", abs(amount), -abs(amount))) %>%
        group_by(month) %>%
        summarise(net = sum(signed, na.rm = TRUE), .groups = "drop") %>%
        arrange(month) %>%
        mutate(cumnet = cumsum(net))
      plot_ly(agg, x = ~month, y = ~cumnet, type = "scatter", mode = "lines+markers",
              line = list(color = "#00D97E", width = 2)) %>%
        layout(xaxis = list(title = ""), yaxis = list(title = "Cumulative Net ($)"),
               paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)")

    } else {
      plotly_empty_msg("Select a report type and click Generate.")
    }
  })

  # ── Pie chart ──────────────────────────────────────────────────────────────
  output$rpt_pie_chart <- renderPlotly({
    df <- rpt_df()
    if (nrow(df) == 0) return(plotly_empty_msg("No data."))
    agg <- df %>%
      filter(!is.na(category_type), category_type == "expense") %>%
      group_by(category_name) %>%
      summarise(total = sum(abs(amount), na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total)) %>%
      head(10)
    if (nrow(agg) == 0) return(plotly_empty_msg("No expense data."))
    chart_category_pie(agg %>% rename(category = category_name, color = category_name))
  })

  # ── Data table ─────────────────────────────────────────────────────────────
  output$rpt_data_table <- renderDT({
    df <- rpt_df()
    if (nrow(df) == 0) return(datatable(data.frame()))
    display <- df %>%
      mutate(
        Date        = format(as.Date(date), "%Y-%m-%d"),
        Amount      = ifelse(!is.na(category_type) & category_type == "income",
                             paste0("+", fmt_currency(abs(amount))),
                             paste0("-", fmt_currency(abs(amount)))),
        Category    = category_name %||% "",
        Account     = account_name  %||% "",
        Description = description   %||% "",
        `Tax Ded.`  = ifelse(!is.na(tax_deductible) & tax_deductible == 1, "Yes", "")
      ) %>%
      select(Date, Amount, Category, Account, Description, `Tax Ded.`)
    datatable(display, rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE,
                             dom = "Bfrtip", buttons = c("copy","csv","excel")),
              extensions = "Buttons")
  })

  # ── PDF export ─────────────────────────────────────────────────────────────
  observeEvent(input$rpt_export_pdf, {
    shinyjs::click("rpt_pdf_download")
  })
  output$rpt_pdf_download <- downloadHandler(
    filename = function() paste0("finance_report_", Sys.Date(), ".pdf"),
    content  = function(file) {
      df   <- rpt_df()
      tmp  <- tempfile(fileext = ".Rmd")
      writeLines(c(
        "---",
        paste0("title: 'Finance Report — ", format(Sys.Date(), "%B %Y"), "'"),
        "output: pdf_document",
        "---",
        "",
        "## Summary",
        "",
        paste0("**Period:** ", input$rpt_date_range[1], " to ", input$rpt_date_range[2]),
        "",
        paste0("**Total Income:** ", fmt_currency(sum(abs(df$amount[!is.na(df$category_type) & df$category_type == "income"]), na.rm = TRUE))),
        paste0("**Total Expenses:** ", fmt_currency(sum(abs(df$amount[!is.na(df$category_type) & df$category_type == "expense"]), na.rm = TRUE))),
        "",
        "## Transactions",
        "",
        "```{r echo=FALSE}",
        "knitr::kable(head(df[,c('date','amount','category_name','description')],50))",
        "```"
      ), tmp)
      rmarkdown::render(tmp, output_file = file, quiet = TRUE,
                         envir = list(df = df))
    }
  )

  # ── CSV export ─────────────────────────────────────────────────────────────
  observeEvent(input$rpt_export_csv, {
    shinyjs::click("rpt_csv_download")
  })
  output$rpt_csv_download <- downloadHandler(
    filename = function() paste0("transactions_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- rpt_df()
      readr::write_csv(df, file)
    }
  )
}
