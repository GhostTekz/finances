#!/usr/bin/env Rscript
# seed_data.R — Populate the database with demo data
# Usage: Rscript seed_data.R [master_password]
# Default password: "DemoPass123!"

library(DBI)
library(RSQLite)
library(lubridate)

source("R/auth.R")
source("R/database.R")

DB_PATH <- "data/finance.db"
dir.create("data", showWarnings = FALSE)

DEMO_PASSWORD <- commandArgs(trailingOnly = TRUE)[1] %||% "DemoPass123!"

cat("Initialising database...\n")
init_database(DB_PATH)

# Set up master password
if (!auth_password_exists(DB_PATH)) {
  cat("Creating demo master password:", DEMO_PASSWORD, "\n")
  result <- create_master_password(DB_PATH, DEMO_PASSWORD)
  if (!result$success) stop("Failed to create password: ", result$message)
} else {
  cat("Master password already set. Skipping.\n")
}

con <- dbConnect(SQLite(), DB_PATH)
on.exit(dbDisconnect(con))

# ── Clear existing demo data (keep categories and settings) ──────────────────
cat("Clearing existing transaction/account/bill/goal data...\n")
dbExecute(con, "DELETE FROM transactions")
dbExecute(con, "DELETE FROM accounts")
dbExecute(con, "DELETE FROM bills")
dbExecute(con, "DELETE FROM goals")
dbExecute(con, "DELETE FROM goal_contributions")
dbExecute(con, "DELETE FROM budgets")

# ── Accounts ─────────────────────────────────────────────────────────────────
cat("Adding accounts...\n")
accounts <- list(
  list("Chase Checking",     "checking",   3842.50, "USD", 0.01,  "Primary checking"),
  list("Chase Savings",      "savings",   12500.00, "USD", 4.25,  "HYSA"),
  list("Chase Credit Card",  "credit",    -1250.75, "USD", 24.99, "Freedom Unlimited"),
  list("Vanguard 401k",      "investment",98450.00, "USD", 0,     "Retirement account"),
  list("Ally Savings",       "savings",    5000.00, "USD", 4.50,  "Emergency fund"),
  list("Cash",               "cash",         250.00,"USD", 0,     "Wallet cash")
)
for (a in accounts) {
  db_add_account(con, a[[1]], a[[2]], a[[3]], a[[4]], a[[5]], a[[6]])
}

acct_ids <- dbGetQuery(con, "SELECT id, name FROM accounts")

get_acct_id <- function(name) {
  acct_ids$id[grepl(name, acct_ids$name, fixed = TRUE)][1]
}

# ── Category IDs ─────────────────────────────────────────────────────────────
cats <- dbGetQuery(con, "SELECT id, name FROM categories")
get_cat_id <- function(name) cats$id[grepl(name, cats$name, fixed = TRUE)][1]

cat_salary    <- get_cat_id("Salary")
cat_freelance <- get_cat_id("Freelance")
cat_food      <- get_cat_id("Food")
cat_transport <- get_cat_id("Transport")
cat_housing   <- get_cat_id("Housing")
cat_utils     <- get_cat_id("Utilities")
cat_entertain <- get_cat_id("Entertainment")
cat_shopping  <- get_cat_id("Shopping")
cat_health    <- get_cat_id("Healthcare")
cat_savings   <- get_cat_id("Savings")
cat_other     <- get_cat_id("Other")

checking_id  <- get_acct_id("Checking")
savings_id   <- get_acct_id("Chase Savings")
credit_id    <- get_acct_id("Credit")

# ── Transactions (last 6 months) ──────────────────────────────────────────────
cat("Adding transactions...\n")

today <- Sys.Date()

add_tx <- function(date, amount, cat_id, acct_id, desc, tags = "", recurring = 0, tax = 0) {
  dbExecute(con,
    "INSERT INTO transactions(date,amount,category_id,account_id,description,tags,is_recurring,tax_deductible)
     VALUES(?,?,?,?,?,?,?,?)",
    list(as.character(date), amount, cat_id, acct_id, desc, tags, recurring, tax))
}

# Monthly salary for last 6 months
for (i in 0:5) {
  d <- floor_date(today - months(i), "month") + 14  # 15th of each month
  add_tx(d, 5800, cat_salary, checking_id, "Paycheck - Employer Co", "salary,income", 1)
}

# Freelance income (sporadic)
add_tx(today - 45, 1200, cat_freelance, checking_id, "Freelance web project", "freelance,income")
add_tx(today - 12, 800,  cat_freelance, checking_id, "Consulting fee",        "freelance,income")

# ── Monthly recurring expenses ────────────────────────────────────────────────
months_back <- 0:5
for (i in months_back) {
  d_base <- floor_date(today - months(i), "month")

  # Rent (1st)
  add_tx(d_base + 0,  1950, cat_housing,   checking_id, "Rent payment",      "housing,rent",   1, 0)
  # Electricity
  add_tx(d_base + 14, 85 + sample(-15:30, 1), cat_utils, checking_id, "Electric bill", "utilities", 1)
  # Internet
  add_tx(d_base + 5,  65,   cat_utils,     checking_id, "Comcast Internet",  "utilities",      1)
  # Phone
  add_tx(d_base + 8,  45,   cat_utils,     checking_id, "T-Mobile phone",    "utilities,phone",1)
  # Netflix
  add_tx(d_base + 3,  15.49,cat_entertain, credit_id,   "Netflix",           "streaming",      1)
  # Spotify
  add_tx(d_base + 3,  10.99,cat_entertain, credit_id,   "Spotify",           "music,streaming",1)
  # Gym
  add_tx(d_base + 1,  40,   cat_health,    credit_id,   "Planet Fitness",    "fitness",        1)
  # Savings transfer
  add_tx(d_base + 1,  500,  cat_savings,   checking_id, "Transfer to savings","savings",       1)
}

# ── Variable grocery/dining ────────────────────────────────────────────────────
grocery_stores <- c("Whole Foods", "Trader Joe's", "Safeway", "Kroger", "Costco")
restaurants    <- c("Chipotle","Starbucks","McDonalds","Panda Express","Local Diner")
set.seed(42)
for (i in 1:50) {
  d    <- today - sample(0:180, 1)
  is_g <- runif(1) > 0.4
  if (is_g) {
    add_tx(d, round(runif(1, 20, 180), 2), cat_food, credit_id,
           sample(grocery_stores, 1), "groceries")
  } else {
    add_tx(d, round(runif(1, 8, 45), 2), cat_food, credit_id,
           sample(restaurants, 1), "dining")
  }
}

# ── Transport ──────────────────────────────────────────────────────────────────
for (i in 1:20) {
  d <- today - sample(0:180, 1)
  desc <- sample(c("Shell Gas Station","BP Gas","Uber","Lyft","Parking"), 1)
  add_tx(d, round(runif(1, 10, 70), 2), cat_transport, credit_id, desc, "transport")
}

# ── Shopping ──────────────────────────────────────────────────────────────────
shopping_stores <- c("Amazon","Target","Best Buy","Walmart","IKEA")
for (i in 1:15) {
  d <- today - sample(0:180, 1)
  add_tx(d, round(runif(1, 15, 300), 2), cat_shopping, credit_id,
         sample(shopping_stores, 1), "shopping")
}

# ── Tax-deductible ─────────────────────────────────────────────────────────────
add_tx(today - 30,  1200, cat_other,  checking_id, "Home office equipment",   "work,tax", 0, 1)
add_tx(today - 60,   450, cat_health, checking_id, "Medical bills",           "health",   0, 1)
add_tx(today - 90,   350, cat_other,  checking_id, "Professional development","tax,edu",  0, 1)

# ── Bills ─────────────────────────────────────────────────────────────────────
cat("Adding bills...\n")
bills <- list(
  list("Rent",        1950.00, today + 1,  "monthly", checking_id, 0),
  list("Electric",      85.00, today + 14, "monthly", checking_id, 1),
  list("Internet",      65.00, today + 5,  "monthly", checking_id, 1),
  list("Cell Phone",    45.00, today + 8,  "monthly", checking_id, 1),
  list("Netflix",       15.49, today + 3,  "monthly", credit_id,   1),
  list("Spotify",       10.99, today + 3,  "monthly", credit_id,   1),
  list("Gym Membership",40.00, today + 1,  "monthly", credit_id,   1),
  list("Car Insurance", 125.00,today + 20, "monthly", checking_id, 0),
  list("Renter's Insurance", 18.00, today + 15, "monthly", checking_id, 1),
  list("Amazon Prime",  14.99, today + 10, "yearly",  credit_id,   1)
)
for (b in bills) {
  db_add_bill(con, b[[1]], b[[2]], b[[3]], b[[4]], b[[5]], b[[6]])
}

# ── Goals ─────────────────────────────────────────────────────────────────────
cat("Adding goals...\n")
goals <- list(
  list("Emergency Fund",    10000,  7500, today + 180, savings_id,  "emergency",
       "6 months of expenses"),
  list("Vacation to Japan",  4500,   800, today + 300, savings_id,  "vacation",
       "Summer 2026 trip"),
  list("New Laptop",         2000,   350, today + 120, checking_id, "other",
       "MacBook Pro upgrade"),
  list("Down Payment Fund", 50000,  8200, today + 730, savings_id,  "home",
       "20% down on a condo"),
  list("Credit Card Payoff", 1250.75,600, today + 90,  checking_id, "debt",
       "Pay off Chase card")
)
for (g in goals) {
  db_add_goal(con, g[[1]], g[[2]], g[[3]], g[[4]], g[[5]], g[[6]], g[[7]])
}

# Add some contribution history
goal_ids <- dbGetQuery(con, "SELECT id, name FROM goals")
emerg_id <- goal_ids$id[goal_ids$name == "Emergency Fund"][1]
vac_id   <- goal_ids$id[goal_ids$name == "Vacation to Japan"][1]

if (!is.na(emerg_id)) {
  for (i in 1:6) {
    db_add_goal_contribution(con, emerg_id, 500,
      floor_date(today - months(i), "month") + 1, "Monthly contribution")
  }
  # Adjust current_amount to match contributions
  dbExecute(con, sprintf("UPDATE goals SET current_amount=7500 WHERE id=%d", emerg_id))
}
if (!is.na(vac_id)) {
  for (i in 1:2) {
    db_add_goal_contribution(con, vac_id, 400,
      floor_date(today - months(i), "month") + 1, "Japan fund")
  }
  dbExecute(con, sprintf("UPDATE goals SET current_amount=800 WHERE id=%d", vac_id))
}

# ── Budgets ────────────────────────────────────────────────────────────────────
cat("Adding budgets for current month...\n")
m <- as.integer(format(today, "%m"))
y <- as.integer(format(today, "%Y"))

budget_map <- list(
  list("Housing",       2000),
  list("Food & Dining", 600),
  list("Transport",     300),
  list("Utilities",     250),
  list("Entertainment", 100),
  list("Shopping",      400),
  list("Healthcare",    200),
  list("Savings",       500),
  list("Other",         200)
)
for (b in budget_map) {
  cat_id <- get_cat_id(b[[1]])
  if (!is.na(cat_id)) db_upsert_budget(con, cat_id, m, y, b[[2]], 0)
}

cat("\n=================================================\n")
cat("Seed data loaded successfully!\n")
cat("Database:", DB_PATH, "\n")
cat("Master password:", DEMO_PASSWORD, "\n")
cat("=================================================\n")
