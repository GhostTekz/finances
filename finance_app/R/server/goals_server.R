# R/server/goals_server.R

goals_server <- function(input, output, session, rv) {
  observe({
    accts <- db_get_accounts(rv$db)
    choices <- c("None" = "", setNames(accts$id, accts$name))
    updateSelectInput(session, "goal_account_inp", choices = choices)
  })

  goals_data <- reactive({
    input$goal_save_btn
    input$goal_contrib_save
    db_get_goals(rv$db)
  })

  # ── Goal cards ─────────────────────────────────────────────────────────────
  output$goals_cards <- renderUI({
    df <- goals_data()
    if (nrow(df) == 0) {
      return(div(class = "p-4 text-muted text-center",
        icon("flag", class = "fa-3x mb-2"), br(),
        "No goals yet. Click 'Add Goal' to get started."))
    }

    cat_icons <- list(
      emergency = "shield-halved", vacation = "plane", home = "house",
      retirement = "umbrella-beach", debt = "credit-card", other = "flag"
    )
    cat_colors <- list(
      emergency = "danger", vacation = "primary", home = "success",
      retirement = "warning", debt = "dark", other = "secondary"
    )

    tagList(lapply(seq_len(nrow(df)), function(i) {
      g      <- df[i, ]
      pct    <- if (g$target_amount > 0)
                  min(round(g$current_amount / g$target_amount * 100, 1), 100) else 0
      remain <- max(g$target_amount - g$current_amount, 0)
      ic     <- cat_icons[[g$category]]  %||% "flag"
      col    <- cat_colors[[g$category]] %||% "secondary"

      # Project completion date
      days_to_target <- if (!is.na(g$target_date))
                          as.integer(as.Date(g$target_date) - Sys.Date()) else NA
      contrib_history <- dbGetQuery(rv$db,
        sprintf("SELECT date, amount FROM goal_contributions WHERE goal_id=%d ORDER BY date",
                as.integer(g$id)))
      monthly_rate <- if (nrow(contrib_history) > 0) {
        monthly <- contrib_history %>%
          mutate(m = format(as.Date(date), "%Y-%m")) %>%
          group_by(m) %>% summarise(s = sum(amount), .groups="drop")
        mean(monthly$s)
      } else 0
      months_to_go <- if (monthly_rate > 0 && remain > 0)
                        ceiling(remain / monthly_rate) else NA

      div(class = "col-md-6 col-lg-4 mb-4",
        div(class = "card border-0 shadow-sm h-100",
          div(class = paste0("card-header bg-", col, " bg-opacity-10 border-0"),
            div(class = "d-flex justify-content-between align-items-center",
              div(class = "d-flex align-items-center gap-2",
                icon(ic, class = paste0("text-", col)),
                span(class = "fw-semibold", g$name)
              ),
              div(class = "d-flex gap-1",
                actionButton(paste0("goal_add_contrib_", g$id), "",
                  icon = icon("circle-plus"), class = "btn-xs btn-outline-success",
                  onclick = sprintf(
                    "Shiny.setInputValue('goal_contrib_open',%d,{priority:'event'})", g$id)),
                actionButton(paste0("goal_edit_", g$id), "",
                  icon = icon("pen"), class = "btn-xs btn-outline-primary",
                  onclick = sprintf(
                    "Shiny.setInputValue('goal_edit_id_click',%d,{priority:'event'})", g$id)),
                actionButton(paste0("goal_del_", g$id), "",
                  icon = icon("trash"), class = "btn-xs btn-outline-danger",
                  onclick = sprintf(
                    "Shiny.setInputValue('goal_delete_id',%d,{priority:'event'})", g$id))
              )
            )
          ),
          div(class = "card-body",
            div(class = "d-flex justify-content-between mb-2",
              div(
                div(class = "small text-muted", "Saved"),
                div(class = "fw-bold fs-5", fmt_currency(g$current_amount))
              ),
              div(class = "text-end",
                div(class = "small text-muted", "Target"),
                div(class = "fw-bold fs-5", fmt_currency(g$target_amount))
              )
            ),
            div(class = "progress mb-2", style = "height:16px;",
              div(class = paste0("progress-bar bg-", col),
                  style = paste0("width:", pct, "%;"),
                  role = "progressbar",
                  paste0(pct, "%"))
            ),
            div(class = "d-flex justify-content-between small text-muted",
              span(if (remain > 0) paste0(fmt_currency(remain), " to go") else "Goal reached!"),
              if (!is.na(g$target_date))
                span(format(as.Date(g$target_date), "%b %d, %Y"))
            ),
            if (!is.na(months_to_go)) div(class = "mt-2 small text-info",
              icon("clock"), " ~", months_to_go, " months at current pace"),
            if (!is.na(g$account_name) && nchar(g$account_name) > 0)
              div(class = "mt-1 small text-muted",
                icon("building-columns"), " ", g$account_name)
          )
        )
      )
    }) %>% {
      div(class = "row", .)
    })
  })

  # ── Add modal ──────────────────────────────────────────────────────────────
  observeEvent(input$goal_add_btn, {
    updateNumericInput(session,  "goal_edit_id",     value = NA)
    updateTextInput(session,     "goal_name_inp",    value = "")
    updateSelectInput(session,   "goal_cat_inp",     selected = "other")
    updateNumericInput(session,  "goal_target_inp",  value = 1000)
    updateNumericInput(session,  "goal_current_inp", value = 0)
    updateDateInput(session,     "goal_date_inp",    value = Sys.Date() + 365)
    updateTextAreaInput(session, "goal_notes_inp",   value = "")
    shinyjs::show("goal_modal_div")
  })
  output$goal_modal_title <- renderUI({
    if (!is.null(input$goal_edit_id) && !is.na(input$goal_edit_id)) "Edit Goal" else "Add Goal"
  })

  # ── Edit ───────────────────────────────────────────────────────────────────
  observeEvent(input$goal_edit_id_click, {
    id  <- as.integer(input$goal_edit_id_click)
    row <- dbGetQuery(rv$db, sprintf("SELECT * FROM goals WHERE id=%d", id))
    if (nrow(row) == 0) return()
    updateNumericInput(session,  "goal_edit_id",     value = id)
    updateTextInput(session,     "goal_name_inp",    value = row$name[1])
    updateSelectInput(session,   "goal_cat_inp",     selected = row$category[1])
    updateNumericInput(session,  "goal_target_inp",  value = row$target_amount[1])
    updateNumericInput(session,  "goal_current_inp", value = row$current_amount[1])
    if (!is.na(row$target_date[1]))
      updateDateInput(session, "goal_date_inp", value = as.Date(row$target_date[1]))
    updateSelectInput(session,  "goal_account_inp",  selected = row$account_id[1] %||% "")
    updateTextAreaInput(session, "goal_notes_inp",   value = row$notes[1] %||% "")
    shinyjs::show("goal_modal_div")
  })

  # ── Save ───────────────────────────────────────────────────────────────────
  observeEvent(input$goal_save_btn, {
    name <- trimws(input$goal_name_inp %||% "")
    tgt  <- as.numeric(input$goal_target_inp %||% 0)
    if (nchar(name) == 0 || is.na(tgt) || tgt <= 0) {
      shinyalert("Validation", "Name and target amount are required.", type = "warning")
      return()
    }
    acct_id <- tryCatch(as.integer(input$goal_account_inp), error = function(e) NA)
    edit_id <- input$goal_edit_id
    tryCatch({
      if (!is.null(edit_id) && !is.na(edit_id)) {
        db_update_goal(rv$db, as.integer(edit_id), name, tgt,
          as.numeric(input$goal_current_inp %||% 0),
          input$goal_date_inp, acct_id, input$goal_cat_inp,
          input$goal_notes_inp %||% "")
      } else {
        db_add_goal(rv$db, name, tgt,
          as.numeric(input$goal_current_inp %||% 0),
          input$goal_date_inp, acct_id, input$goal_cat_inp,
          input$goal_notes_inp %||% "")
      }
      shinyjs::hide("goal_modal_div")
      shinyalert("Saved", "Goal saved.", type = "success")
    }, error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
  })

  # ── Contribution modal ─────────────────────────────────────────────────────
  observeEvent(input$goal_contrib_open, {
    updateNumericInput(session, "goal_contrib_goal_id", value = input$goal_contrib_open)
    updateNumericInput(session, "goal_contrib_amount",  value = 0)
    updateDateInput(session,    "goal_contrib_date",    value = Sys.Date())
    updateTextInput(session,    "goal_contrib_notes",   value = "")
    shinyjs::show("goal_contrib_div")
  })
  observeEvent(input$goal_contrib_save, {
    goal_id <- as.integer(input$goal_contrib_goal_id)
    amount  <- as.numeric(input$goal_contrib_amount %||% 0)
    if (is.na(goal_id) || is.na(amount) || amount <= 0) {
      shinyalert("Validation", "Enter a positive contribution amount.", type = "warning")
      return()
    }
    tryCatch({
      db_add_goal_contribution(rv$db, goal_id, amount,
        input$goal_contrib_date, input$goal_contrib_notes %||% "")
      shinyjs::hide("goal_contrib_div")
      shinyalert("Saved", paste0(fmt_currency(amount), " contribution added."), type = "success")
    }, error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
  })

  # ── Delete ─────────────────────────────────────────────────────────────────
  observeEvent(input$goal_delete_id, {
    id <- as.integer(input$goal_delete_id)
    shinyalert("Delete Goal?", "This cannot be undone.", type = "warning",
               showCancelButton = TRUE,
               callbackR = function(x) {
                 if (isTRUE(x)) {
                   tryCatch(db_delete_goal(rv$db, id),
                            error = function(e) shinyalert("Error", conditionMessage(e), type = "error"))
                 }
               })
  })

  observeEvent(input$goal_modal_close,   shinyjs::hide("goal_modal_div"))
  observeEvent(input$goal_cancel_btn,    shinyjs::hide("goal_modal_div"))
  observeEvent(input$goal_contrib_close, shinyjs::hide("goal_contrib_div"))
  observeEvent(input$goal_contrib_cancel,shinyjs::hide("goal_contrib_div"))
}
