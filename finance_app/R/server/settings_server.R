# R/server/settings_server.R

settings_server <- function(input, output, session, rv) {
  # ── Categories table ────────────────────────────────────────────────────────
  output$categories_table <- renderDT({
    input$cat_save_btn
    cats <- db_get_categories(rv$db)
    cats$Actions <- as.character(cats$id)
    datatable(
      cats %>% select(name, type, color, icon, budget_limit, Actions) %>%
        rename(Name = name, Type = type, Color = color, Icon = icon,
               `Budget Limit` = budget_limit),
      escape = FALSE, rownames = FALSE, selection = "none",
      options = list(
        pageLength = 20, dom = "frtip",
        columnDefs = list(list(
          targets = 5,
          render  = JS("function(data){
            return '<button class=\"btn btn-xs btn-outline-primary me-1\" onclick=\"Shiny.setInputValue(\\\"cat_edit_id\\\",'+data+',{priority:\\\"event\\\"})\">Edit</button>' +
                   '<button class=\"btn btn-xs btn-outline-danger\" onclick=\"Shiny.setInputValue(\\\"cat_delete_id\\\",'+data+',{priority:\\\"event\\\"})\">Del</button>';
          }")
        ))
      )
    )
  })

  # ── Add category ───────────────────────────────────────────────────────────
  observeEvent(input$cat_add_btn, {
    updateNumericInput(session, "cat_edit_id",     value = NA)
    updateTextInput(session,   "cat_name_inp",    value = "")
    updateSelectInput(session, "cat_type_inp",    selected = "expense")
    updateTextInput(session,   "cat_color_inp",   value = "#6E84A3")
    updateTextInput(session,   "cat_icon_inp",    value = "tag")
    updateNumericInput(session,"cat_budget_inp",  value = 0)
    shinyjs::show("cat_modal_div")
  })
  output$cat_modal_title <- renderUI({
    if (!is.null(input$cat_edit_id) && !is.na(input$cat_edit_id)) "Edit Category" else "Add Category"
  })

  # ── Edit category ──────────────────────────────────────────────────────────
  observeEvent(input$cat_edit_id, {
    id  <- as.integer(input$cat_edit_id)
    row <- dbGetQuery(rv$db, sprintf("SELECT * FROM categories WHERE id=%d", id))
    if (nrow(row) == 0) return()
    updateNumericInput(session, "cat_edit_id",   value = id)
    updateTextInput(session,   "cat_name_inp",   value = row$name[1])
    updateSelectInput(session, "cat_type_inp",   selected = row$type[1])
    updateTextInput(session,   "cat_color_inp",  value = row$color[1])
    updateTextInput(session,   "cat_icon_inp",   value = row$icon[1])
    updateNumericInput(session,"cat_budget_inp", value = row$budget_limit[1] %||% 0)
    shinyjs::show("cat_modal_div")
  })

  # ── Save category ──────────────────────────────────────────────────────────
  observeEvent(input$cat_save_btn, {
    name <- trimws(input$cat_name_inp %||% "")
    if (nchar(name) == 0) {
      shinyalert("Validation", "Category name is required.", type = "warning")
      return()
    }
    edit_id <- input$cat_edit_id
    tryCatch({
      if (!is.null(edit_id) && !is.na(edit_id)) {
        db_update_category(rv$db, as.integer(edit_id), name, input$cat_type_inp,
          input$cat_color_inp %||% "#6E84A3",
          input$cat_icon_inp  %||% "tag",
          as.numeric(input$cat_budget_inp %||% 0))
      } else {
        db_add_category(rv$db, name, input$cat_type_inp,
          input$cat_color_inp %||% "#6E84A3",
          input$cat_icon_inp  %||% "tag",
          as.numeric(input$cat_budget_inp %||% 0))
      }
      shinyjs::hide("cat_modal_div")
      shinyalert("Saved", "Category saved.", type = "success")
    }, error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
  })

  # ── Delete category ────────────────────────────────────────────────────────
  observeEvent(input$cat_delete_id, {
    id <- as.integer(input$cat_delete_id)
    shinyalert("Delete Category?",
               "Transactions using this category won't be deleted but will lose their category.",
               type = "warning", showCancelButton = TRUE,
               callbackR = function(x) {
                 if (isTRUE(x)) {
                   tryCatch(db_delete_category(rv$db, id),
                            error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
                 }
               })
  })

  observeEvent(input$cat_modal_close, shinyjs::hide("cat_modal_div"))
  observeEvent(input$cat_cancel_btn,  shinyjs::hide("cat_modal_div"))

  # ── Keyword rules ──────────────────────────────────────────────────────────
  output$keyword_rules_table <- renderDT({
    input$rule_save_btn
    rules <- db_get_keyword_rules(rv$db)
    if (nrow(rules) == 0) {
      return(datatable(data.frame(Message = "No keyword rules configured."),
                        options = list(dom = "t"), rownames = FALSE))
    }
    rules$Actions <- as.character(rules$id)
    datatable(
      rules %>% select(keyword, category_name, match_type, Actions) %>%
        rename(Keyword = keyword, Category = category_name, `Match Type` = match_type),
      escape = FALSE, rownames = FALSE, selection = "none",
      options = list(
        pageLength = 20, dom = "frtip",
        columnDefs = list(list(
          targets = 3,
          render  = JS("function(data){
            return '<button class=\"btn btn-xs btn-outline-danger\" onclick=\"Shiny.setInputValue(\\\"rule_delete_id\\\",'+data+',{priority:\\\"event\\\"})\">Delete</button>';
          }")
        ))
      )
    )
  })

  observe({
    cats <- db_get_categories(rv$db)
    updateSelectInput(session, "rule_category_inp",
                       choices = setNames(cats$id, cats$name))
  })

  observeEvent(input$rule_add_btn, {
    shinyjs::show("rule_modal_div")
  })
  observeEvent(input$rule_save_btn, {
    kw  <- trimws(input$rule_keyword_inp %||% "")
    cat <- as.integer(input$rule_category_inp)
    if (nchar(kw) == 0 || is.na(cat)) {
      shinyalert("Validation", "Keyword and category are required.", type = "warning")
      return()
    }
    tryCatch({
      db_add_keyword_rule(rv$db, kw, cat, input$rule_match_inp %||% "contains")
      shinyjs::hide("rule_modal_div")
      updateTextInput(session, "rule_keyword_inp", value = "")
      shinyalert("Saved", "Rule added.", type = "success")
    }, error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
  })
  observeEvent(input$rule_delete_id, {
    id <- as.integer(input$rule_delete_id)
    tryCatch(db_delete_keyword_rule(rv$db, id),
             error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
  })
  observeEvent(input$rule_modal_close, shinyjs::hide("rule_modal_div"))
  observeEvent(input$rule_cancel_btn,  shinyjs::hide("rule_modal_div"))

  # ── Change master password ─────────────────────────────────────────────────
  observeEvent(input$sec_change_pwd_btn, {
    old  <- input$sec_old_pwd  %||% ""
    new  <- input$sec_new_pwd  %||% ""
    conf <- input$sec_conf_pwd %||% ""
    if (nchar(new) < 8) {
      shinyalert("Validation", "New password must be at least 8 characters.", type = "warning")
      return()
    }
    if (new != conf) {
      shinyalert("Validation", "New passwords do not match.", type = "warning")
      return()
    }
    result <- change_master_password(DB_PATH, old, new, rv$enc_key)
    if (result$success) {
      shinyalert("Success", "Password changed. Please re-login.", type = "success",
                  callbackR = function(x) session$reload())
    } else {
      shinyalert("Error", result$message, type = "error")
    }
  })

  # ── Data export ────────────────────────────────────────────────────────────
  output$data_export_db <- downloadHandler(
    filename = function() paste0("finance_backup_", Sys.Date(), ".db"),
    content  = function(file) file.copy(DB_PATH, file)
  )
  output$data_export_csv_all <- downloadHandler(
    filename = function() paste0("all_transactions_", Sys.Date(), ".csv"),
    content  = function(file) {
      df <- db_get_transactions(rv$db, limit = 999999)
      readr::write_csv(df, file)
    }
  )

  # ── Data restore ───────────────────────────────────────────────────────────
  observeEvent(input$data_restore_btn, {
    req(input$data_restore_file)
    shinyalert("Restore Database?",
               "This will overwrite all current data. Are you sure?",
               type = "warning", showCancelButton = TRUE,
               callbackR = function(x) {
                 if (isTRUE(x)) {
                   tryCatch({
                     dbDisconnect(rv$db)
                     file.copy(input$data_restore_file$datapath, DB_PATH, overwrite = TRUE)
                     rv$db <- dbConnect(SQLite(), DB_PATH)
                     shinyalert("Restored", "Database restored successfully.", type = "success")
                   }, error = function(e) {
                     rv$db <- dbConnect(SQLite(), DB_PATH)
                     shinyalert("Error", conditionMessage(e), type = "error")
                   })
                 }
               })
  })

  # ── Preferences ────────────────────────────────────────────────────────────
  observe({
    sym <- db_get_setting(rv$db, "currency_symbol", "$")
    fmt <- db_get_setting(rv$db, "date_format", "%Y-%m-%d")
    updateTextInput(session, "pref_currency", value = sym)
    updateSelectInput(session, "pref_date_format", selected = fmt)
  })
  observeEvent(input$pref_save_btn, {
    tryCatch({
      db_upsert_setting(rv$db, "currency_symbol", input$pref_currency %||% "$")
      db_upsert_setting(rv$db, "date_format",     input$pref_date_format %||% "%Y-%m-%d")
      shinyalert("Saved", "Preferences saved.", type = "success")
    }, error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
  })
}
