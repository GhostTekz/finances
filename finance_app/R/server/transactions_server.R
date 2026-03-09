# R/server/transactions_server.R

transactions_server <- function(input, output, session, rv) {
  # ── Populate filter dropdowns ──────────────────────────────────────────────
  observe({
    cats  <- db_get_categories(rv$db)
    accts <- db_get_accounts(rv$db)
    cat_choices  <- setNames(c("", cats$id),  c("All", cats$name))
    acct_choices <- setNames(c("", accts$id), c("All", accts$name))
    tx_modal_cats  <- setNames(cats$id,  cats$name)
    tx_modal_accts <- setNames(accts$id, accts$name)
    updateSelectInput(session, "tx_filter_category", choices = cat_choices)
    updateSelectInput(session, "tx_filter_account",  choices = acct_choices)
    updateSelectInput(session, "tx_category",  choices = tx_modal_cats)
    updateSelectInput(session, "tx_account",   choices = tx_modal_accts)
    updateSelectInput(session, "tx_import_account", choices = tx_modal_accts)
  })

  # ── Transaction table ──────────────────────────────────────────────────────
  tx_data <- reactive({
    input$tx_filter_btn
    input$tx_clear_filter
    input$tx_save_btn
    input$tx_import_confirm

    cat_id  <- if (!is.null(input$tx_filter_category) &&
                   nchar(input$tx_filter_category) > 0)
                 as.integer(input$tx_filter_category) else NULL
    acct_id <- if (!is.null(input$tx_filter_account) &&
                   nchar(input$tx_filter_account) > 0)
                 as.integer(input$tx_filter_account) else NULL
    search  <- input$tx_search %||% NULL
    dfrom   <- tryCatch(as.character(input$tx_date_range[1]), error = function(e) NULL)
    dto     <- tryCatch(as.character(input$tx_date_range[2]), error = function(e) NULL)

    db_get_transactions(rv$db, limit = 1000, date_from = dfrom, date_to = dto,
                         category_id = cat_id, account_id = acct_id, search = search)
  })

  output$tx_table <- renderDT({
    df <- tx_data()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No transactions found."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    display <- df %>%
      mutate(
        Date        = format(as.Date(date), "%Y-%m-%d"),
        Amount      = ifelse(category_type == "income",
                             paste0("+", fmt_currency(abs(amount))),
                             paste0("-", fmt_currency(abs(amount)))),
        Category    = category_name %||% "Uncategorised",
        Account     = account_name  %||% "Unknown",
        Description = description   %||% "",
        Tags        = tags          %||% "",
        `Tax Ded.`  = ifelse(tax_deductible == 1, "Yes", ""),
        Actions     = as.character(id)
      ) %>%
      select(Date, Amount, Category, Account, Description, Tags, `Tax Ded.`, Actions)

    datatable(
      display,
      escape       = FALSE,
      rownames     = FALSE,
      selection    = "none",
      options      = list(
        pageLength = 25,
        scrollX    = TRUE,
        columnDefs = list(
          list(targets = which(names(display) == "Actions") - 1,
               render  = JS("function(data,type,row){
                 return '<button class=\"btn btn-xs btn-outline-primary\" onclick=\"Shiny.setInputValue(\\\"tx_edit_id_click\\\",'+data+',{priority:\\\"event\\\"})\">Edit</button> ' +
                        '<button class=\"btn btn-xs btn-outline-danger\" onclick=\"Shiny.setInputValue(\\\"tx_delete_id\\\",'+data+',{priority:\\\"event\\\"})\">Del</button>';
               }"))
        ),
        dom = "Bfrtip",
        buttons = c("copy", "csv")
      ),
      extensions = "Buttons"
    ) %>%
      formatStyle("Amount",
        color = styleEqual(
          grep("^\\+", unique(display$Amount), value = TRUE),
          rep("#00D97E", length(grep("^\\+", unique(display$Amount), value = TRUE)))
        )
      )
  })

  # ── Summary row ────────────────────────────────────────────────────────────
  output$tx_summary_row <- renderUI({
    df  <- tx_data()
    inc <- sum(abs(df$amount[!is.na(df$category_type) & df$category_type == "income"]), na.rm = TRUE)
    exp <- sum(abs(df$amount[!is.na(df$category_type) & df$category_type == "expense"]), na.rm = TRUE)
    net <- inc - exp
    div(class = "d-flex gap-3 mb-2 mt-1",
      span(class = "badge bg-success",  "Income: ",  fmt_currency(inc)),
      span(class = "badge bg-danger",   "Expenses: ", fmt_currency(exp)),
      span(class = if (net >= 0) "badge bg-primary" else "badge bg-warning",
           "Net: ", fmt_currency(net)),
      span(class = "badge bg-secondary", nrow(df), " transactions")
    )
  })

  # ── Open Add modal ─────────────────────────────────────────────────────────
  observeEvent(input$tx_add_btn, {
    updateNumericInput(session, "tx_edit_id", value = NA)
    updateDateInput(session, "tx_date", value = Sys.Date())
    updateNumericInput(session, "tx_amount", value = 0)
    updateTextInput(session, "tx_description", value = "")
    updateTextInput(session, "tx_tags", value = "")
    updateCheckboxInput(session, "tx_recurring", value = FALSE)
    updateCheckboxInput(session, "tx_tax_deductible", value = FALSE)
    shinyjs::show("tx_modal_div")
  })

  output$tx_modal_title <- renderUI({
    if (!is.null(input$tx_edit_id) && !is.na(input$tx_edit_id)) "Edit Transaction" else "Add Transaction"
  })

  # ── Open Edit modal ────────────────────────────────────────────────────────
  observeEvent(input$tx_edit_id_click, {
    id  <- as.integer(input$tx_edit_id_click)
    row <- dbGetQuery(rv$db,
      sprintf("SELECT * FROM transactions WHERE id=%d", id))
    if (nrow(row) == 0) return()
    updateNumericInput(session,  "tx_edit_id",        value = id)
    updateDateInput(session,     "tx_date",            value = as.Date(row$date[1]))
    updateNumericInput(session,  "tx_amount",          value = abs(row$amount[1]))
    updateSelectInput(session,   "tx_category",        selected = row$category_id[1])
    updateSelectInput(session,   "tx_account",         selected = row$account_id[1])
    updateTextInput(session,     "tx_description",     value = row$description[1] %||% "")
    updateTextInput(session,     "tx_tags",            value = row$tags[1] %||% "")
    updateCheckboxInput(session, "tx_recurring",       value = row$is_recurring[1] == 1)
    updateCheckboxInput(session, "tx_tax_deductible",  value = row$tax_deductible[1] == 1)
    shinyjs::show("tx_modal_div")
  })

  # ── Save transaction ───────────────────────────────────────────────────────
  observeEvent(input$tx_save_btn, {
    amt     <- abs(as.numeric(input$tx_amount %||% 0))
    cat_id  <- as.integer(input$tx_category)
    acct_id <- as.integer(input$tx_account)
    if (is.na(amt) || amt == 0) {
      shinyalert("Validation", "Amount must be greater than zero.", type = "warning")
      return()
    }
    if (is.na(cat_id) || is.na(acct_id)) {
      shinyalert("Validation", "Please select a category and account.", type = "warning")
      return()
    }
    edit_id <- input$tx_edit_id
    tryCatch({
      if (!is.null(edit_id) && !is.na(edit_id)) {
        db_update_transaction(rv$db, as.integer(edit_id),
          input$tx_date, amt, cat_id, acct_id,
          input$tx_description %||% "", input$tx_tags %||% "",
          as.integer(input$tx_recurring), as.integer(input$tx_tax_deductible))
        shinyalert("Saved", "Transaction updated.", type = "success")
      } else {
        db_add_transaction(rv$db, input$tx_date, amt, cat_id, acct_id,
          input$tx_description %||% "", input$tx_tags %||% "",
          as.integer(input$tx_recurring), as.integer(input$tx_tax_deductible))
        shinyalert("Saved", "Transaction added.", type = "success")
      }
      shinyjs::hide("tx_modal_div")
    }, error = function(e) {
      shinyalert("Error", conditionMessage(e), type = "error")
    })
  })

  # ── Delete transaction ─────────────────────────────────────────────────────
  observeEvent(input$tx_delete_id, {
    id <- as.integer(input$tx_delete_id)
    shinyalert(
      title = "Delete Transaction?",
      text  = "This cannot be undone.",
      type  = "warning",
      showCancelButton = TRUE,
      confirmButtonText = "Delete",
      callbackR = function(x) {
        if (isTRUE(x)) {
          tryCatch({
            db_delete_transaction(rv$db, id)
            shinyalert("Deleted", "Transaction removed.", type = "success")
          }, error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
        }
      }
    )
  })

  # Close modal buttons
  observeEvent(input$tx_modal_close,  shinyjs::hide("tx_modal_div"))
  observeEvent(input$tx_cancel_btn,   shinyjs::hide("tx_modal_div"))
  observeEvent(input$tx_clear_filter, {
    updateTextInput(session, "tx_search", value = "")
    updateSelectInput(session, "tx_filter_category", selected = "")
    updateSelectInput(session, "tx_filter_account",  selected = "")
  })

  # ── CSV Import ─────────────────────────────────────────────────────────────
  observeEvent(input$tx_import_btn, {
    shinyjs::show("tx_import_modal_div")
  })
  observeEvent(input$tx_import_modal_close, shinyjs::hide("tx_import_modal_div"))
  observeEvent(input$tx_import_cancel,      shinyjs::hide("tx_import_modal_div"))

  csv_data <- reactive({
    req(input$tx_csv_file)
    parse_bank_csv(input$tx_csv_file$datapath)
  })

  output$tx_csv_preview <- renderUI({
    df <- tryCatch(csv_data(), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) {
      return(div(class = "alert alert-warning", "Could not parse CSV. Check format."))
    }
    tagList(
      p(class = "text-success small",
        icon("check"), " Detected ", nrow(df), " rows."),
      div(style = "overflow-x:auto; max-height:150px;",
        renderTable(head(df, 5))
      )
    )
  })

  output$tx_csv_mapping_ui <- renderUI({
    df <- tryCatch(csv_data(), error = function(e) NULL)
    if (is.null(df)) return(NULL)
    cols <- names(df)
    tagList(
      h6("Map CSV Columns"),
      fluidRow(
        column(4, selectInput("csv_col_date",   "Date column",   choices = cols)),
        column(4, selectInput("csv_col_amount", "Amount column", choices = cols)),
        column(4, selectInput("csv_col_desc",   "Description",   choices = c("None" = "", cols)))
      )
    )
  })

  observeEvent(input$tx_import_confirm, {
    df      <- tryCatch(csv_data(), error = function(e) NULL)
    acct_id <- as.integer(input$tx_import_account)
    if (is.null(df) || is.na(acct_id)) {
      shinyalert("Error", "No valid CSV or account selected.", type = "error")
      return()
    }
    imported <- 0
    tryCatch({
      for (i in seq_len(nrow(df))) {
        date_val <- tryCatch(as.Date(df[[input$csv_col_date]][i]), error = function(e) Sys.Date())
        amt_val  <- tryCatch(as.numeric(gsub("[^0-9.\\-]", "", df[[input$csv_col_amount]][i])),
                             error = function(e) 0)
        if (is.na(amt_val) || amt_val == 0) next
        desc     <- if (nchar(input$csv_col_desc) > 0)
                      as.character(df[[input$csv_col_desc]][i]) else ""
        cat_id   <- auto_categorize(rv$db, desc)
        if (is.na(cat_id)) {
          # Use "Other" expense category
          fallback <- dbGetQuery(rv$db, "SELECT id FROM categories WHERE name='Other' LIMIT 1")
          cat_id   <- if (nrow(fallback) > 0) fallback$id[1] else 1L
        }
        db_add_transaction(rv$db, date_val, abs(amt_val), cat_id, acct_id,
                            desc, "", 0, 0)
        imported <- imported + 1
      }
      shinyjs::hide("tx_import_modal_div")
      shinyalert("Import Complete",
                  paste0(imported, " transactions imported."), type = "success")
    }, error = function(e) {
      shinyalert("Import Error", conditionMessage(e), type = "error")
    })
  })
}
