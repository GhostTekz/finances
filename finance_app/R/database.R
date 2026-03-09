# R/database.R — SQLite schema creation and CRUD helpers

library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)

# ── Schema initialisation ──────────────────────────────────────────────────────

init_database <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  dbExecute(con, "PRAGMA journal_mode=WAL;")
  dbExecute(con, "PRAGMA foreign_keys=ON;")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS settings (
      key   TEXT PRIMARY KEY,
      value TEXT
    );")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS categories (
      id           INTEGER PRIMARY KEY AUTOINCREMENT,
      name         TEXT NOT NULL UNIQUE,
      type         TEXT NOT NULL DEFAULT 'expense',  -- income / expense
      color        TEXT DEFAULT '#6E84A3',
      icon         TEXT DEFAULT 'tag',
      budget_limit REAL DEFAULT 0
    );")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS accounts (
      id            INTEGER PRIMARY KEY AUTOINCREMENT,
      name          TEXT NOT NULL,
      type          TEXT NOT NULL,   -- checking/savings/credit/investment/loan/cash
      balance       REAL DEFAULT 0,
      currency      TEXT DEFAULT 'USD',
      interest_rate REAL DEFAULT 0,
      notes         TEXT DEFAULT '',
      created_at    TEXT DEFAULT (datetime('now'))
    );")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS transactions (
      id           INTEGER PRIMARY KEY AUTOINCREMENT,
      date         TEXT NOT NULL,
      amount       REAL NOT NULL,
      category_id  INTEGER REFERENCES categories(id),
      account_id   INTEGER REFERENCES accounts(id),
      description  TEXT DEFAULT '',
      tags         TEXT DEFAULT '',
      is_recurring INTEGER DEFAULT 0,
      tax_deductible INTEGER DEFAULT 0,
      created_at   TEXT DEFAULT (datetime('now'))
    );")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS bills (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      name        TEXT NOT NULL,
      amount      REAL NOT NULL,
      due_date    TEXT NOT NULL,
      frequency   TEXT DEFAULT 'monthly',  -- weekly/monthly/yearly/once
      account_id  INTEGER REFERENCES accounts(id),
      autopay     INTEGER DEFAULT 0,
      active      INTEGER DEFAULT 1,
      notes       TEXT DEFAULT '',
      created_at  TEXT DEFAULT (datetime('now'))
    );")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS vault_entries (
      id                  INTEGER PRIMARY KEY AUTOINCREMENT,
      institution         TEXT NOT NULL,
      username            TEXT DEFAULT '',
      encrypted_password  TEXT DEFAULT '',
      url                 TEXT DEFAULT '',
      encrypted_notes     TEXT DEFAULT '',
      category            TEXT DEFAULT 'banking',  -- banking/investment/insurance/utilities/other
      last_updated        TEXT DEFAULT (datetime('now')),
      created_at          TEXT DEFAULT (datetime('now'))
    );")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS goals (
      id             INTEGER PRIMARY KEY AUTOINCREMENT,
      name           TEXT NOT NULL,
      target_amount  REAL NOT NULL,
      current_amount REAL DEFAULT 0,
      target_date    TEXT,
      account_id     INTEGER REFERENCES accounts(id),
      category       TEXT DEFAULT 'other',
      notes          TEXT DEFAULT '',
      created_at     TEXT DEFAULT (datetime('now'))
    );")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS goal_contributions (
      id         INTEGER PRIMARY KEY AUTOINCREMENT,
      goal_id    INTEGER REFERENCES goals(id) ON DELETE CASCADE,
      amount     REAL NOT NULL,
      date       TEXT NOT NULL,
      notes      TEXT DEFAULT '',
      created_at TEXT DEFAULT (datetime('now'))
    );")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS budgets (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      category_id INTEGER REFERENCES categories(id),
      month       INTEGER NOT NULL,
      year        INTEGER NOT NULL,
      amount      REAL NOT NULL,
      rollover    INTEGER DEFAULT 0,
      UNIQUE(category_id, month, year)
    );")

  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS keyword_rules (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      keyword     TEXT NOT NULL,
      category_id INTEGER REFERENCES categories(id),
      match_type  TEXT DEFAULT 'contains'   -- contains / starts_with / exact
    );")

  # ── Seed default categories ─────────────────────────────────────────────────
  existing <- dbGetQuery(con, "SELECT COUNT(*) as n FROM categories")$n
  if (existing == 0) {
    seed_categories <- data.frame(
      name  = c("Salary","Freelance","Investment Income","Other Income",
                "Housing","Food & Dining","Transport","Utilities",
                "Healthcare","Entertainment","Shopping","Education",
                "Insurance","Savings","Debt Payment","Personal Care","Travel","Other"),
      type  = c(rep("income", 4), rep("expense", 14)),
      color = c("#00D97E","#2C7BE5","#F6C343","#6E84A3",
                "#E63757","#FF6B6B","#4ECDC4","#45B7D1",
                "#96CEB4","#FFEAA7","#DDA0DD","#98D8C8",
                "#F0E68C","#90EE90","#FFB347","#DEB887","#87CEEB","#D3D3D3"),
      icon  = c("money-bill","laptop","chart-line","plus",
                "house","utensils","car","bolt",
                "heart-pulse","film","shopping-bag","graduation-cap",
                "shield","piggy-bank","credit-card","spa","plane","tag"),
      budget_limit = 0,
      stringsAsFactors = FALSE
    )
    dbWriteTable(con, "categories", seed_categories, append = TRUE)
  }

  # ── Seed default settings ───────────────────────────────────────────────────
  default_settings <- list(
    currency_symbol = "$",
    date_format     = "%Y-%m-%d",
    app_version     = "1.0.0"
  )
  for (k in names(default_settings)) {
    exists <- dbGetQuery(con,
      sprintf("SELECT COUNT(*) as n FROM settings WHERE key='%s'", k))$n
    if (exists == 0) {
      dbExecute(con, "INSERT INTO settings(key,value) VALUES(?,?)",
                list(k, default_settings[[k]]))
    }
  }

  invisible(TRUE)
}

# ── Generic helpers ────────────────────────────────────────────────────────────

db_get_setting <- function(con, key, default = NULL) {
  res <- dbGetQuery(con, sprintf(
    "SELECT value FROM settings WHERE key='%s'", key))
  if (nrow(res) == 0) return(default)
  res$value[1] %||% default
}

db_upsert_setting <- function(con, key, value) {
  n <- dbGetQuery(con,
    sprintf("SELECT COUNT(*) as n FROM settings WHERE key='%s'", key))$n
  if (n > 0) {
    dbExecute(con, "UPDATE settings SET value=? WHERE key=?", list(value, key))
  } else {
    dbExecute(con, "INSERT INTO settings(key,value) VALUES(?,?)", list(key, value))
  }
}

# ── Categories ─────────────────────────────────────────────────────────────────

db_get_categories <- function(con, type = NULL) {
  q <- "SELECT * FROM categories ORDER BY type DESC, name"
  if (!is.null(type)) {
    q <- sprintf("SELECT * FROM categories WHERE type='%s' ORDER BY name", type)
  }
  dbGetQuery(con, q)
}

db_add_category <- function(con, name, type, color = "#6E84A3",
                             icon = "tag", budget_limit = 0) {
  dbExecute(con,
    "INSERT INTO categories(name,type,color,icon,budget_limit) VALUES(?,?,?,?,?)",
    list(name, type, color, icon, budget_limit))
}

db_update_category <- function(con, id, name, type, color, icon, budget_limit) {
  dbExecute(con,
    "UPDATE categories SET name=?,type=?,color=?,icon=?,budget_limit=? WHERE id=?",
    list(name, type, color, icon, budget_limit, id))
}

db_delete_category <- function(con, id) {
  dbExecute(con, "DELETE FROM categories WHERE id=?", list(id))
}

# ── Accounts ───────────────────────────────────────────────────────────────────

db_get_accounts <- function(con) {
  dbGetQuery(con, "SELECT * FROM accounts ORDER BY type, name")
}

db_add_account <- function(con, name, type, balance = 0, currency = "USD",
                            interest_rate = 0, notes = "") {
  dbExecute(con,
    "INSERT INTO accounts(name,type,balance,currency,interest_rate,notes) VALUES(?,?,?,?,?,?)",
    list(name, type, balance, currency, interest_rate, notes))
}

db_update_account <- function(con, id, name, type, balance, currency,
                               interest_rate, notes) {
  dbExecute(con,
    "UPDATE accounts SET name=?,type=?,balance=?,currency=?,interest_rate=?,notes=? WHERE id=?",
    list(name, type, balance, currency, interest_rate, notes, id))
}

db_delete_account <- function(con, id) {
  dbExecute(con, "DELETE FROM accounts WHERE id=?", list(id))
}

db_get_net_worth <- function(con) {
  res <- dbGetQuery(con, "SELECT SUM(balance) as total FROM accounts")
  res$total[1] %||% 0
}

# ── Transactions ───────────────────────────────────────────────────────────────

db_get_transactions <- function(con, limit = 500, offset = 0,
                                  date_from = NULL, date_to = NULL,
                                  category_id = NULL, account_id = NULL,
                                  search = NULL) {
  where <- "WHERE 1=1"
  if (!is.null(date_from))   where <- paste0(where, sprintf(" AND t.date >= '%s'", date_from))
  if (!is.null(date_to))     where <- paste0(where, sprintf(" AND t.date <= '%s'",   date_to))
  if (!is.null(category_id)) where <- paste0(where, sprintf(" AND t.category_id = %d", as.integer(category_id)))
  if (!is.null(account_id))  where <- paste0(where, sprintf(" AND t.account_id = %d",  as.integer(account_id)))
  if (!is.null(search) && nchar(trimws(search)) > 0) {
    s <- gsub("'", "''", search)
    where <- paste0(where, sprintf(
      " AND (t.description LIKE '%%%s%%' OR t.tags LIKE '%%%s%%')", s, s))
  }
  dbGetQuery(con, sprintf("
    SELECT t.*,
           c.name  AS category_name,
           c.type  AS category_type,
           c.color AS category_color,
           a.name  AS account_name
    FROM transactions t
    LEFT JOIN categories c ON t.category_id = c.id
    LEFT JOIN accounts   a ON t.account_id  = a.id
    %s
    ORDER BY t.date DESC, t.created_at DESC
    LIMIT %d OFFSET %d", where, limit, offset))
}

db_add_transaction <- function(con, date, amount, category_id, account_id,
                                 description = "", tags = "",
                                 is_recurring = 0, tax_deductible = 0) {
  dbExecute(con,
    "INSERT INTO transactions
      (date,amount,category_id,account_id,description,tags,is_recurring,tax_deductible)
     VALUES(?,?,?,?,?,?,?,?)",
    list(as.character(date), amount, category_id, account_id,
         description, tags, is_recurring, tax_deductible))
  # Update account balance
  type_row <- dbGetQuery(con,
    sprintf("SELECT type FROM categories WHERE id=%d", as.integer(category_id)))
  if (nrow(type_row) > 0) {
    delta <- if (type_row$type[1] == "income") abs(amount) else -abs(amount)
    dbExecute(con,
      "UPDATE accounts SET balance = balance + ? WHERE id = ?",
      list(delta, account_id))
  }
}

db_update_transaction <- function(con, id, date, amount, category_id,
                                    account_id, description, tags,
                                    is_recurring, tax_deductible) {
  # Reverse old balance effect
  old <- dbGetQuery(con,
    sprintf("SELECT amount, category_id, account_id FROM transactions WHERE id=%d", as.integer(id)))
  if (nrow(old) > 0) {
    old_type <- dbGetQuery(con,
      sprintf("SELECT type FROM categories WHERE id=%d", as.integer(old$category_id[1])))
    if (nrow(old_type) > 0) {
      old_delta <- if (old_type$type[1] == "income") -abs(old$amount[1]) else abs(old$amount[1])
      dbExecute(con, "UPDATE accounts SET balance = balance + ? WHERE id = ?",
                list(old_delta, old$account_id[1]))
    }
  }
  dbExecute(con,
    "UPDATE transactions SET date=?,amount=?,category_id=?,account_id=?,
     description=?,tags=?,is_recurring=?,tax_deductible=? WHERE id=?",
    list(as.character(date), amount, category_id, account_id,
         description, tags, is_recurring, tax_deductible, id))
  # Apply new balance effect
  new_type <- dbGetQuery(con,
    sprintf("SELECT type FROM categories WHERE id=%d", as.integer(category_id)))
  if (nrow(new_type) > 0) {
    delta <- if (new_type$type[1] == "income") abs(amount) else -abs(amount)
    dbExecute(con, "UPDATE accounts SET balance = balance + ? WHERE id = ?",
              list(delta, account_id))
  }
}

db_delete_transaction <- function(con, id) {
  old <- dbGetQuery(con,
    sprintf("SELECT amount, category_id, account_id FROM transactions WHERE id=%d", as.integer(id)))
  if (nrow(old) > 0) {
    old_type <- dbGetQuery(con,
      sprintf("SELECT type FROM categories WHERE id=%d", as.integer(old$category_id[1])))
    if (nrow(old_type) > 0) {
      delta <- if (old_type$type[1] == "income") -abs(old$amount[1]) else abs(old$amount[1])
      dbExecute(con, "UPDATE accounts SET balance = balance + ? WHERE id = ?",
                list(delta, old$account_id[1]))
    }
  }
  dbExecute(con, "DELETE FROM transactions WHERE id=?", list(id))
}

# ── Bills ──────────────────────────────────────────────────────────────────────

db_get_bills <- function(con, active_only = TRUE) {
  q <- "SELECT b.*, a.name as account_name FROM bills b
        LEFT JOIN accounts a ON b.account_id = a.id"
  if (active_only) q <- paste0(q, " WHERE b.active = 1")
  q <- paste0(q, " ORDER BY b.due_date")
  dbGetQuery(con, q)
}

db_add_bill <- function(con, name, amount, due_date, frequency = "monthly",
                          account_id = NA, autopay = 0, notes = "") {
  dbExecute(con,
    "INSERT INTO bills(name,amount,due_date,frequency,account_id,autopay,notes)
     VALUES(?,?,?,?,?,?,?)",
    list(name, amount, as.character(due_date), frequency,
         if (is.na(account_id)) NULL else account_id, autopay, notes))
}

db_update_bill <- function(con, id, name, amount, due_date, frequency,
                             account_id, autopay, notes) {
  dbExecute(con,
    "UPDATE bills SET name=?,amount=?,due_date=?,frequency=?,account_id=?,autopay=?,notes=?
     WHERE id=?",
    list(name, amount, as.character(due_date), frequency,
         if (is.na(account_id)) NULL else account_id, autopay, notes, id))
}

db_delete_bill <- function(con, id) {
  dbExecute(con, "DELETE FROM bills WHERE id=?", list(id))
}

# ── Goals ──────────────────────────────────────────────────────────────────────

db_get_goals <- function(con) {
  dbGetQuery(con, "
    SELECT g.*, a.name as account_name
    FROM goals g
    LEFT JOIN accounts a ON g.account_id = a.id
    ORDER BY g.target_date")
}

db_add_goal <- function(con, name, target_amount, current_amount = 0,
                          target_date = NULL, account_id = NA,
                          category = "other", notes = "") {
  dbExecute(con,
    "INSERT INTO goals(name,target_amount,current_amount,target_date,account_id,category,notes)
     VALUES(?,?,?,?,?,?,?)",
    list(name, target_amount, current_amount,
         if (is.null(target_date)) NULL else as.character(target_date),
         if (is.na(account_id)) NULL else account_id, category, notes))
}

db_update_goal <- function(con, id, name, target_amount, current_amount,
                             target_date, account_id, category, notes) {
  dbExecute(con,
    "UPDATE goals SET name=?,target_amount=?,current_amount=?,target_date=?,
     account_id=?,category=?,notes=? WHERE id=?",
    list(name, target_amount, current_amount,
         if (is.null(target_date)) NULL else as.character(target_date),
         if (is.na(account_id)) NULL else account_id, category, notes, id))
}

db_add_goal_contribution <- function(con, goal_id, amount, date, notes = "") {
  dbExecute(con,
    "INSERT INTO goal_contributions(goal_id,amount,date,notes) VALUES(?,?,?,?)",
    list(goal_id, amount, as.character(date), notes))
  dbExecute(con,
    "UPDATE goals SET current_amount = current_amount + ? WHERE id = ?",
    list(amount, goal_id))
}

db_delete_goal <- function(con, id) {
  dbExecute(con, "DELETE FROM goals WHERE id=?", list(id))
}

# ── Budgets ────────────────────────────────────────────────────────────────────

db_get_budgets <- function(con, month, year) {
  dbGetQuery(con, sprintf("
    SELECT b.*, c.name AS category_name, c.color, c.type AS category_type
    FROM budgets b
    JOIN categories c ON b.category_id = c.id
    WHERE b.month = %d AND b.year = %d", as.integer(month), as.integer(year)))
}

db_upsert_budget <- function(con, category_id, month, year, amount, rollover = 0) {
  n <- dbGetQuery(con, sprintf(
    "SELECT COUNT(*) as n FROM budgets WHERE category_id=%d AND month=%d AND year=%d",
    as.integer(category_id), as.integer(month), as.integer(year)))$n
  if (n > 0) {
    dbExecute(con,
      "UPDATE budgets SET amount=?,rollover=? WHERE category_id=? AND month=? AND year=?",
      list(amount, rollover, category_id, month, year))
  } else {
    dbExecute(con,
      "INSERT INTO budgets(category_id,month,year,amount,rollover) VALUES(?,?,?,?,?)",
      list(category_id, month, year, amount, rollover))
  }
}

db_get_budget_vs_actual <- function(con, month, year) {
  dbGetQuery(con, sprintf("
    SELECT c.name AS category, c.color,
           COALESCE(b.amount, 0)  AS budgeted,
           COALESCE(SUM(CASE WHEN c2.type='expense' THEN ABS(t.amount) ELSE 0 END), 0) AS spent
    FROM categories c2
    JOIN categories c  ON c.id = c2.id
    LEFT JOIN budgets b ON b.category_id = c.id AND b.month=%d AND b.year=%d
    LEFT JOIN transactions t ON t.category_id = c.id
      AND strftime('%%m', t.date) = '%02d'
      AND strftime('%%Y', t.date) = '%d'
    WHERE c.type = 'expense'
    GROUP BY c.id, c.name, c.color, b.amount
    ORDER BY spent DESC", as.integer(month), as.integer(year),
    as.integer(month), as.integer(year)))
}

# ── Vault ──────────────────────────────────────────────────────────────────────

db_get_vault_entries <- function(con) {
  dbGetQuery(con,
    "SELECT id, institution, username, encrypted_password, url,
            encrypted_notes, category, last_updated
     FROM vault_entries ORDER BY category, institution")
}

db_add_vault_entry <- function(con, institution, username,
                                 encrypted_password, url,
                                 encrypted_notes, category) {
  dbExecute(con,
    "INSERT INTO vault_entries
      (institution,username,encrypted_password,url,encrypted_notes,category,last_updated)
     VALUES(?,?,?,?,?,?,datetime('now'))",
    list(institution, username, encrypted_password, url, encrypted_notes, category))
}

db_update_vault_entry <- function(con, id, institution, username,
                                    encrypted_password, url,
                                    encrypted_notes, category) {
  dbExecute(con,
    "UPDATE vault_entries SET institution=?,username=?,encrypted_password=?,url=?,
     encrypted_notes=?,category=?,last_updated=datetime('now') WHERE id=?",
    list(institution, username, encrypted_password, url, encrypted_notes, category, id))
}

db_delete_vault_entry <- function(con, id) {
  dbExecute(con, "DELETE FROM vault_entries WHERE id=?", list(id))
}

# ── Keyword rules ──────────────────────────────────────────────────────────────

db_get_keyword_rules <- function(con) {
  dbGetQuery(con, "
    SELECT kr.*, c.name AS category_name
    FROM keyword_rules kr
    LEFT JOIN categories c ON kr.category_id = c.id
    ORDER BY kr.keyword")
}

db_add_keyword_rule <- function(con, keyword, category_id, match_type = "contains") {
  dbExecute(con,
    "INSERT INTO keyword_rules(keyword,category_id,match_type) VALUES(?,?,?)",
    list(keyword, category_id, match_type))
}

db_delete_keyword_rule <- function(con, id) {
  dbExecute(con, "DELETE FROM keyword_rules WHERE id=?", list(id))
}

#' Auto-categorise a transaction description using keyword rules
auto_categorize <- function(con, description) {
  rules <- db_get_keyword_rules(con)
  if (nrow(rules) == 0) return(NA_integer_)
  desc_lower <- tolower(description)
  for (i in seq_len(nrow(rules))) {
    kw <- tolower(rules$keyword[i])
    match <- switch(rules$match_type[i],
      "exact"       = desc_lower == kw,
      "starts_with" = startsWith(desc_lower, kw),
      grepl(kw, desc_lower, fixed = TRUE)  # contains (default)
    )
    if (isTRUE(match)) return(rules$category_id[i])
  }
  NA_integer_
}

# ── Null coalescing (re-export) ────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b
