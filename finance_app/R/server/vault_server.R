# R/server/vault_server.R
# Security: all encrypt/decrypt operations use the in-memory key from rv$enc_key

vault_server <- function(input, output, session, rv) {
  # ── Reactive vault entries (decrypted for display — titles only) ──────────
  vault_data <- reactive({
    input$vault_save_btn
    db_get_vault_entries(rv$db)
  })

  # ── Filtered entries list ─────────────────────────────────────────────────
  output$vault_entries_list <- renderUI({
    df      <- vault_data()
    search  <- tolower(trimws(input$vault_search %||% ""))
    cat_f   <- input$vault_filter_cat %||% ""

    if (nrow(df) == 0) {
      return(div(class = "p-4 text-muted text-center",
        icon("shield-halved", class = "fa-3x mb-2"), br(), "Vault is empty. Add an entry to begin."))
    }

    if (nchar(search) > 0) {
      df <- df[grepl(search, tolower(df$institution)) |
               grepl(search, tolower(df$username)), ]
    }
    if (nchar(cat_f) > 0) {
      df <- df[df$category == cat_f, ]
    }
    if (nrow(df) == 0) {
      return(div(class = "p-4 text-muted text-center", "No matching vault entries."))
    }

    cat_icons <- list(banking = "building-columns", investment = "chart-line",
                       insurance = "shield", utilities = "bolt", other = "star")

    tagList(lapply(seq_len(nrow(df)), function(i) {
      entry     <- df[i, ]
      icon_name <- cat_icons[[entry$category]] %||% "star"
      div(class = "d-flex justify-content-between align-items-center p-3 border-bottom",
        div(class = "d-flex align-items-center gap-3",
          div(class = "bg-success bg-opacity-10 rounded p-2",
            icon(icon_name, class = "text-success")),
          div(
            div(class = "fw-semibold", entry$institution),
            div(class = "text-muted small", entry$username),
            div(class = "text-muted", style = "font-size:0.75rem",
              stringr::str_to_title(entry$category), " · Updated: ",
              format(as.POSIXct(entry$last_updated), "%b %d, %Y"))
          )
        ),
        div(class = "d-flex gap-2",
          actionButton(paste0("vault_view_", entry$id), "",
            icon = icon("eye"), class = "btn-sm btn-outline-success",
            onclick = sprintf(
              "Shiny.setInputValue('vault_view_id',%d,{priority:'event'})", entry$id)),
          actionButton(paste0("vault_edit_", entry$id), "",
            icon = icon("pen"), class = "btn-sm btn-outline-primary",
            onclick = sprintf(
              "Shiny.setInputValue('vault_edit_id_click',%d,{priority:'event'})", entry$id)),
          actionButton(paste0("vault_del_", entry$id), "",
            icon = icon("trash"), class = "btn-sm btn-outline-danger",
            onclick = sprintf(
              "Shiny.setInputValue('vault_delete_id',%d,{priority:'event'})", entry$id))
        )
      )
    }))
  })

  # ── Add modal ──────────────────────────────────────────────────────────────
  observeEvent(input$vault_add_btn, {
    updateNumericInput(session,  "vault_edit_id",        value = NA)
    updateTextInput(session,     "vault_institution_inp", value = "")
    updateSelectInput(session,   "vault_category_inp",    selected = "banking")
    updateTextInput(session,     "vault_username_inp",    value = "")
    updateTextInput(session,     "vault_url_inp",         value = "")
    updateTextInput(session,     "vault_password_inp",    value = "")
    updateTextAreaInput(session, "vault_notes_inp",       value = "")
    shinyjs::show("vault_modal_div")
  })
  output$vault_modal_title <- renderUI({
    if (!is.null(input$vault_edit_id) && !is.na(input$vault_edit_id))
      "Edit Vault Entry" else "New Vault Entry"
  })

  # ── Edit modal ─────────────────────────────────────────────────────────────
  observeEvent(input$vault_edit_id_click, {
    id    <- as.integer(input$vault_edit_id_click)
    row   <- dbGetQuery(rv$db,
      sprintf("SELECT * FROM vault_entries WHERE id=%d", id))
    if (nrow(row) == 0) return()
    pwd_plain   <- vault_decrypt(row$encrypted_password[1], rv$enc_key)
    notes_plain <- vault_decrypt(row$encrypted_notes[1],    rv$enc_key)
    updateNumericInput(session,  "vault_edit_id",        value = id)
    updateTextInput(session,     "vault_institution_inp", value = row$institution[1])
    updateSelectInput(session,   "vault_category_inp",    selected = row$category[1])
    updateTextInput(session,     "vault_username_inp",    value = row$username[1] %||% "")
    updateTextInput(session,     "vault_url_inp",         value = row$url[1] %||% "")
    updateTextInput(session,     "vault_password_inp",    value = pwd_plain)
    updateTextAreaInput(session, "vault_notes_inp",       value = notes_plain)
    shinyjs::show("vault_modal_div")
  })

  # ── View entry ─────────────────────────────────────────────────────────────
  observeEvent(input$vault_view_id, {
    id  <- as.integer(input$vault_view_id)
    row <- dbGetQuery(rv$db,
      sprintf("SELECT * FROM vault_entries WHERE id=%d", id))
    if (nrow(row) == 0) return()
    pwd_plain   <- vault_decrypt(row$encrypted_password[1], rv$enc_key)
    notes_plain <- vault_decrypt(row$encrypted_notes[1],    rv$enc_key)

    output$vault_view_content <- renderUI({
      tagList(
        div(class = "mb-2",
          strong("Institution: "), row$institution[1]),
        div(class = "mb-2",
          strong("Category: "), stringr::str_to_title(row$category[1])),
        div(class = "mb-2",
          strong("Username: "), row$username[1] %||% ""),
        div(class = "mb-2",
          strong("URL: "),
          if (nchar(row$url[1] %||% "") > 0)
            tags$a(href = row$url[1], target = "_blank", row$url[1])
          else ""),
        div(class = "mb-2",
          strong("Password: "),
          div(class = "input-group input-group-sm",
            tags$input(type = "password", id = "view_pwd_field",
                        class = "form-control", value = pwd_plain, readonly = "readonly"),
            tags$button(class = "btn btn-outline-secondary",
              type = "button",
              onclick = "var f=document.getElementById('view_pwd_field');
                         f.type=(f.type==='password'?'text':'password');",
              icon("eye")
            ),
            tags$button(class = "btn btn-outline-success",
              type = "button",
              onclick = paste0("navigator.clipboard.writeText('", pwd_plain, "');"),
              icon("copy"), " Copy"
            )
          )
        ),
        if (nchar(notes_plain) > 0) div(class = "mb-2",
          strong("Notes: "), br(), pre(class = "bg-light p-2 small rounded", notes_plain)),
        div(class = "text-muted small",
          "Last updated: ", format(as.POSIXct(row$last_updated[1]), "%b %d, %Y %H:%M"))
      )
    })
    shinyjs::show("vault_view_div")
  })

  # ── Generate password ─────────────────────────────────────────────────────
  observeEvent(input$vault_gen_btn, {
    shinyjs::toggle("vault_gen_options")
  })

  observeEvent(input$vault_gen_length, {
    pwd <- generate_password(
      length  = as.integer(input$vault_gen_length %||% 20),
      upper   = isTRUE(input$vault_gen_upper),
      lower   = isTRUE(input$vault_gen_lower),
      digits  = isTRUE(input$vault_gen_digits),
      symbols = isTRUE(input$vault_gen_symbols)
    )
    updateTextInput(session, "vault_password_inp", value = pwd)
  }, ignoreInit = TRUE)

  # ── Show/hide password ────────────────────────────────────────────────────
  observeEvent(input$vault_show_pwd, {
    runjs("var f=document.getElementById('vault_password_inp');
           f.type=(f.type==='password'?'text':'password');")
  })

  # ── Save vault entry ──────────────────────────────────────────────────────
  observeEvent(input$vault_save_btn, {
    inst <- trimws(input$vault_institution_inp %||% "")
    if (nchar(inst) == 0) {
      shinyalert("Validation", "Institution name is required.", type = "warning")
      return()
    }
    enc_pwd   <- vault_encrypt(input$vault_password_inp %||% "", rv$enc_key)
    enc_notes <- vault_encrypt(input$vault_notes_inp    %||% "", rv$enc_key)
    edit_id   <- input$vault_edit_id
    tryCatch({
      if (!is.null(edit_id) && !is.na(edit_id)) {
        db_update_vault_entry(rv$db, as.integer(edit_id), inst,
          input$vault_username_inp %||% "", enc_pwd,
          input$vault_url_inp      %||% "", enc_notes,
          input$vault_category_inp)
      } else {
        db_add_vault_entry(rv$db, inst,
          input$vault_username_inp %||% "", enc_pwd,
          input$vault_url_inp      %||% "", enc_notes,
          input$vault_category_inp)
      }
      shinyjs::hide("vault_modal_div")
      shinyalert("Saved", "Vault entry saved and encrypted.", type = "success")
    }, error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
  })

  # ── Delete ─────────────────────────────────────────────────────────────────
  observeEvent(input$vault_delete_id, {
    id <- as.integer(input$vault_delete_id)
    shinyalert("Delete Entry?", "This cannot be undone.", type = "warning",
               showCancelButton = TRUE,
               callbackR = function(x) {
                 if (isTRUE(x)) {
                   tryCatch(db_delete_vault_entry(rv$db, id),
                            error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
                 }
               })
  })

  observeEvent(input$vault_modal_close,  shinyjs::hide("vault_modal_div"))
  observeEvent(input$vault_cancel_btn,   shinyjs::hide("vault_modal_div"))
  observeEvent(input$vault_view_close,   shinyjs::hide("vault_view_div"))
}
