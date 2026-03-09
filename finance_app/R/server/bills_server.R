# R/server/bills_server.R

bills_server <- function(input, output, session, rv) {
  # Populate account dropdown
  observe({
    accts <- db_get_accounts(rv$db)
    choices <- c("None" = "", setNames(accts$id, accts$name))
    updateSelectInput(session, "bill_account_inp", choices = choices)
  })

  bills_data <- reactive({
    input$bill_save_btn
    db_get_bills(rv$db)
  })

  # ── Bills table ────────────────────────────────────────────────────────────
  output$bills_table <- renderDT({
    df <- bills_data()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No bills added yet."),
                        options = list(dom = "t"), rownames = FALSE))
    }
    today <- Sys.Date()
    display <- df %>%
      mutate(
        `Due Date`  = format(as.Date(due_date), "%Y-%m-%d"),
        Amount      = fmt_currency(amount),
        Frequency   = stringr::str_to_title(frequency),
        `Auto Pay`  = ifelse(autopay == 1, "Yes", "No"),
        Status      = ifelse(as.Date(due_date) < today, "Overdue",
                      ifelse(as.Date(due_date) <= today + 3, "Due Soon", "OK")),
        Actions     = as.character(id)
      ) %>%
      select(name, Amount, `Due Date`, Frequency, `Auto Pay`, Status, Actions)

    datatable(
      display,
      escape    = FALSE,
      rownames  = FALSE,
      selection = "none",
      colnames  = c("Name", "Amount", "Due Date", "Frequency", "Autopay", "Status", "Actions"),
      options   = list(
        pageLength = 20, dom = "frtip",
        columnDefs = list(
          list(
            targets = 6,
            render  = JS("function(data,type,row){
              return '<button class=\"btn btn-xs btn-outline-primary\" onclick=\"Shiny.setInputValue(\\\"bill_edit_id_click\\\",'+data+',{priority:\\\"event\\\"})\">Edit</button> ' +
                     '<button class=\"btn btn-xs btn-outline-danger\" onclick=\"Shiny.setInputValue(\\\"bill_delete_id\\\",'+data+',{priority:\\\"event\\\"})\">Del</button>';
            }")
          ),
          list(
            targets = 5,
            render  = JS("function(data){
              var cls = data==='Overdue'?'bg-danger':data==='Due Soon'?'bg-warning text-dark':'bg-success';
              return '<span class=\"badge '+cls+'\">'+data+'</span>';
            }")
          )
        )
      )
    )
  })

  # ── Summary panel ─────────────────────────────────────────────────────────
  output$bills_summary_panel <- renderUI({
    df <- bills_data()
    if (nrow(df) == 0) return(p(class = "text-muted", "No bills."))
    monthly <- sum(df$amount[df$frequency == "monthly"], na.rm = TRUE)
    weekly  <- sum(df$amount[df$frequency == "weekly"],  na.rm = TRUE) * 4.33
    yearly  <- sum(df$amount[df$frequency == "yearly"],  na.rm = TRUE) / 12
    total   <- monthly + weekly + yearly
    tagList(
      div(class = "d-flex justify-content-between mb-1 small",
        span("Monthly bills:"), span(class = "fw-bold", fmt_currency(monthly))),
      div(class = "d-flex justify-content-between mb-1 small",
        span("Weekly (×4.33):"), span(class = "fw-bold", fmt_currency(weekly))),
      div(class = "d-flex justify-content-between mb-1 small",
        span("Annual (÷12):"),   span(class = "fw-bold", fmt_currency(yearly))),
      hr(),
      div(class = "d-flex justify-content-between fw-bold",
        span("Total/month:"), span(class = "text-danger", fmt_currency(total)))
    )
  })

  # ── Upcoming panel ─────────────────────────────────────────────────────────
  output$bills_upcoming_panel <- renderUI({
    df  <- bills_data()
    up  <- df[!is.na(df$due_date) & as.Date(df$due_date) >= Sys.Date() &
              as.Date(df$due_date) <= Sys.Date() + 30, ]
    up  <- up[order(as.Date(up$due_date)), ]
    if (nrow(up) == 0) return(p(class = "text-muted small", "No bills in next 30 days."))
    tagList(lapply(seq_len(nrow(up)), function(i) {
      days   <- as.integer(as.Date(up$due_date[i]) - Sys.Date())
      b_cls  <- if (days <= 0) "bg-danger" else if (days <= 7) "bg-warning" else "bg-secondary"
      div(class = "d-flex justify-content-between align-items-center p-1 border-bottom",
        div(
          div(class = "small fw-semibold", up$name[i]),
          div(class = "text-muted", style = "font-size:0.75rem",
            format(as.Date(up$due_date[i]), "%b %d"))
        ),
        div(class = "text-end",
          div(class = "small fw-bold", fmt_currency(up$amount[i])),
          span(class = paste("badge", b_cls), paste0(days, "d"))
        )
      )
    }))
  })

  # ── Bill calendar ─────────────────────────────────────────────────────────
  output$bills_calendar <- renderUI({
    df    <- bills_data()
    today <- Sys.Date()
    m_start <- floor_date(today, "month")
    m_end   <- ceiling_date(today, "month") - 1
    days_in <- as.integer(m_end - m_start) + 1

    due_this_month <- df[!is.na(df$due_date) &
      as.Date(df$due_date) >= m_start &
      as.Date(df$due_date) <= m_end, ]

    weeks     <- ceiling(days_in / 7)
    first_dow <- as.integer(format(m_start, "%u")) %% 7  # 0=Sun

    cells <- vector("list", weeks * 7)
    for (i in seq_len(weeks * 7)) {
      day_num <- i - first_dow
      if (day_num < 1 || day_num > days_in) {
        cells[[i]] <- div(class = "cal-cell empty")
      } else {
        d        <- m_start + (day_num - 1)
        day_bills <- due_this_month[!is.na(due_this_month$due_date) &
                                    as.Date(due_this_month$due_date) == d, ]
        is_today  <- d == today
        cell_cls  <- paste("cal-cell", if (is_today) "today")
        bills_html <- if (nrow(day_bills) > 0) {
          tagList(lapply(seq_len(nrow(day_bills)), function(j)
            div(class = "cal-bill", day_bills$name[j], " ",
                fmt_currency(day_bills$amount[j]))))
        } else NULL
        cells[[i]] <- div(class = cell_cls,
          span(class = "cal-day-num", day_num),
          bills_html
        )
      }
    }

    tagList(
      tags$style("
        .cal-grid{display:grid;grid-template-columns:repeat(7,1fr);gap:4px}
        .cal-header{text-align:center;font-weight:600;padding:4px;background:#f8f9fa}
        .cal-cell{min-height:60px;border:1px solid #dee2e6;border-radius:4px;padding:4px;font-size:0.75rem}
        .cal-cell.today{background:#e8f4fd;border-color:#2C7BE5}
        .cal-cell.empty{background:#f8f9fa;border:none}
        .cal-day-num{font-weight:600;color:#6E84A3;display:block}
        .cal-bill{background:#E63757;color:white;border-radius:3px;padding:1px 4px;margin-top:2px;white-space:nowrap;overflow:hidden;text-overflow:ellipsis}
      "),
      div(class = "cal-grid",
        lapply(c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"),
               function(d) div(class = "cal-header", d)),
        cells
      )
    )
  })

  # ── Add modal ──────────────────────────────────────────────────────────────
  observeEvent(input$bill_add_btn, {
    updateNumericInput(session, "bill_edit_id", value = NA)
    updateTextInput(session, "bill_name_inp", value = "")
    updateNumericInput(session, "bill_amount_inp", value = 0)
    updateDateInput(session, "bill_due_date_inp", value = Sys.Date() + 1)
    updateSelectInput(session, "bill_frequency_inp", selected = "monthly")
    updateCheckboxInput(session, "bill_autopay_inp", value = FALSE)
    updateTextAreaInput(session, "bill_notes_inp", value = "")
    shinyjs::show("bill_modal_div")
  })
  output$bill_modal_title <- renderUI({
    if (!is.null(input$bill_edit_id) && !is.na(input$bill_edit_id)) "Edit Bill" else "Add Bill"
  })

  # ── Edit modal ─────────────────────────────────────────────────────────────
  observeEvent(input$bill_edit_id_click, {
    id  <- as.integer(input$bill_edit_id_click)
    row <- dbGetQuery(rv$db, sprintf("SELECT * FROM bills WHERE id=%d", id))
    if (nrow(row) == 0) return()
    updateNumericInput(session,  "bill_edit_id",       value = id)
    updateTextInput(session,     "bill_name_inp",       value = row$name[1])
    updateNumericInput(session,  "bill_amount_inp",     value = row$amount[1])
    updateDateInput(session,     "bill_due_date_inp",   value = as.Date(row$due_date[1]))
    updateSelectInput(session,   "bill_frequency_inp",  selected = row$frequency[1])
    updateSelectInput(session,   "bill_account_inp",    selected = row$account_id[1] %||% "")
    updateCheckboxInput(session, "bill_autopay_inp",    value = row$autopay[1] == 1)
    updateTextAreaInput(session, "bill_notes_inp",      value = row$notes[1] %||% "")
    shinyjs::show("bill_modal_div")
  })

  # ── Save ───────────────────────────────────────────────────────────────────
  observeEvent(input$bill_save_btn, {
    name   <- trimws(input$bill_name_inp %||% "")
    amount <- as.numeric(input$bill_amount_inp %||% 0)
    if (nchar(name) == 0 || is.na(amount) || amount <= 0) {
      shinyalert("Validation", "Name and amount are required.", type = "warning")
      return()
    }
    acct_id <- tryCatch(as.integer(input$bill_account_inp), error = function(e) NA)
    edit_id <- input$bill_edit_id
    tryCatch({
      if (!is.null(edit_id) && !is.na(edit_id)) {
        db_update_bill(rv$db, as.integer(edit_id), name, amount,
                        input$bill_due_date_inp, input$bill_frequency_inp,
                        acct_id, as.integer(input$bill_autopay_inp),
                        input$bill_notes_inp %||% "")
      } else {
        db_add_bill(rv$db, name, amount, input$bill_due_date_inp,
                     input$bill_frequency_inp, acct_id,
                     as.integer(input$bill_autopay_inp),
                     input$bill_notes_inp %||% "")
      }
      shinyjs::hide("bill_modal_div")
      shinyalert("Saved", "Bill saved.", type = "success")
    }, error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
  })

  # ── Delete ─────────────────────────────────────────────────────────────────
  observeEvent(input$bill_delete_id, {
    id <- as.integer(input$bill_delete_id)
    shinyalert("Delete Bill?", "This cannot be undone.", type = "warning",
               showCancelButton = TRUE,
               callbackR = function(x) {
                 if (isTRUE(x)) {
                   tryCatch(db_delete_bill(rv$db, id),
                            error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
                 }
               })
  })

  observeEvent(input$bill_modal_close, shinyjs::hide("bill_modal_div"))
  observeEvent(input$bill_cancel_btn,  shinyjs::hide("bill_modal_div"))
}
