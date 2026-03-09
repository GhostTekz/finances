# R Personal Finance & Audit Platform

A comprehensive, fully local personal finance management system built with R Shiny.
All data stays on your machine — no cloud, no telemetry.

---

## Features

| Module | Description |
|---|---|
| **Dashboard** | Net worth, monthly income/expense charts, budget gauges, upcoming bills |
| **Transactions** | Manual entry, CSV bank import, auto-categorisation, search/filter |
| **Budget Manager** | Monthly budgets per category, progress bars, copy-last-month, annual view |
| **Bill Tracker** | Bill scheduling, calendar view, overdue alerts, autopay tracking |
| **Account Manager** | Multi-account balance tracking, net worth calculation, interest rates |
| **Password Vault** | AES-256 encrypted credential store, password generator, copy-to-clipboard |
| **Financial Goals** | Savings goal tracking, contribution history, projected completion |
| **Reports** | 12-month trends, category breakdown, tax report, PDF/CSV export |
| **Settings** | Categories, keyword rules, master password change, backup/restore |

---

## Quick Start

### Prerequisites
- Ubuntu/Debian Linux (or any Linux with apt-get)
- R ≥ 4.2 (auto-installed if missing)
- Internet for initial R package installation

### Launch

```bash
cd finance_app
chmod +x start_finance_app.sh
./start_finance_app.sh
```

The script will:
1. Install R if not present
2. Install all required R packages
3. Start the Shiny app at **http://localhost:7777**
4. Open your browser automatically

### First Run

On first launch you will be prompted to **create a master password**.
This password:
- Is hashed with bcrypt (cost factor 12) — never stored in plaintext
- Derives your AES-256-CBC vault encryption key via PBKDF2 (200,000 iterations)
- **Cannot be recovered** — store it in a safe place

---

## Load Demo Data (Optional)

```bash
cd finance_app
Rscript seed_data.R
# Default demo password: DemoPass123!
```

This seeds 6 months of realistic transactions, accounts, bills, budgets, and goals.

---

## Security Architecture

```
Master Password
      │
      ├──► bcrypt hash (cost 12)  ──► stored in SQLite settings table
      │
      └──► PBKDF2-HMAC-SHA256 ────► AES-256-CBC key (in memory only)
                 │                         │
            200,000 iter              ┌────┴────┐
            random 32-byte salt       │ Encrypt │  vault_entries.encrypted_password
            stored in settings        │         │  vault_entries.encrypted_notes
                                      └─────────┘
```

- **At rest**: Vault data encrypted with AES-256-CBC; the key is never written to disk
- **In memory**: Key lives only for the duration of the authenticated session
- **Session**: Auto-locks after 15 minutes of inactivity
- **Localhost only**: App binds to 127.0.0.1 — never exposed to the network
- **No outbound**: Zero external requests; fully offline

---

## File Structure

```
finance_app/
├── app.R                        # Main Shiny entry point
├── start_finance_app.sh         # Linux launch script
├── install_packages.R           # Package installer
├── seed_data.R                  # Demo data populator
├── README.md
├── R/
│   ├── auth.R                   # bcrypt + AES-256 encryption
│   ├── database.R               # SQLite schema + CRUD
│   ├── ui/                      # Per-module UI definitions
│   │   ├── dashboard_ui.R
│   │   ├── transactions_ui.R
│   │   ├── budget_ui.R
│   │   ├── bills_ui.R
│   │   ├── accounts_ui.R
│   │   ├── vault_ui.R
│   │   ├── goals_ui.R
│   │   ├── reports_ui.R
│   │   └── settings_ui.R
│   ├── server/                  # Per-module server logic
│   │   ├── dashboard_server.R
│   │   ├── transactions_server.R
│   │   ├── budget_server.R
│   │   ├── bills_server.R
│   │   ├── accounts_server.R
│   │   ├── vault_server.R
│   │   ├── goals_server.R
│   │   ├── reports_server.R
│   │   └── settings_server.R
│   └── utils/
│       ├── charts.R             # Reusable plotly chart builders
│       ├── formatters.R         # Currency/date formatters
│       └── csv_parser.R         # Bank statement CSV parser
├── data/
│   └── finance.db               # SQLite database (auto-created)
└── www/
    └── custom.css               # Custom styles
```

---

## Database Schema

```sql
settings        (key, value)
categories      (id, name, type, color, icon, budget_limit)
accounts        (id, name, type, balance, currency, interest_rate, notes)
transactions    (id, date, amount, category_id, account_id,
                 description, tags, is_recurring, tax_deductible)
bills           (id, name, amount, due_date, frequency, account_id, autopay, active, notes)
vault_entries   (id, institution, username, encrypted_password,
                 url, encrypted_notes, category, last_updated)
goals           (id, name, target_amount, current_amount, target_date,
                 account_id, category, notes)
goal_contributions (id, goal_id, amount, date, notes)
budgets         (id, category_id, month, year, amount, rollover)
keyword_rules   (id, keyword, category_id, match_type)
```

---

## Required R Packages

```r
shiny, bslib, DBI, RSQLite, openssl, bcrypt, dplyr, tidyr,
lubridate, ggplot2, plotly, DT, shinyalert, shinycssloaders,
rmarkdown, knitr, readr, stringr, purrr, htmltools, shinyjs,
scales, glue, digest, shinyWidgets, fontawesome
```

---

## Backup & Restore

- **Settings → Data → Export Database**: Downloads a full copy of `finance.db`
- **Settings → Data → Restore**: Upload a backup `.db` file to restore

The SQLite database is a single portable file. Copy `data/finance.db` to back up manually.

---

## Stopping the App

Press `Ctrl+C` in the terminal running `start_finance_app.sh`.

---

## Troubleshooting

| Issue | Fix |
|---|---|
| Port 7777 already in use | The script will kill the old process automatically |
| R package install fails | Run `sudo apt-get install libssl-dev libcurl4-openssl-dev libxml2-dev` first |
| App opens but blank | Check `finance_app.log` for errors |
| Forgot master password | Cannot be recovered — restore from a backup that was made before the password was set |

---

## Privacy

This application:
- **Never** sends data to any external server
- **Never** phones home or makes network requests
- Stores everything locally in `data/finance.db`
- Only binds to `127.0.0.1` (localhost)
