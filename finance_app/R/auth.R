# R/auth.R — Master password, bcrypt hashing, AES-256 encryption
# Security model:
#   • Master password hashed with bcrypt (cost 12) and stored in settings table
#   • AES-256-CBC encryption key derived from master password via PBKDF2
#     (256-bit key, 200 000 iterations, SHA-256, salt stored in settings)
#   • Vault passwords/notes encrypted at rest; key lives only in memory

library(openssl)
library(bcrypt)
library(DBI)
library(RSQLite)
library(digest)

# ── Helpers ────────────────────────────────────────────────────────────────────

#' Derive AES-256 key from password + salt using PBKDF2-HMAC-SHA256
derive_key <- function(password, salt_hex) {
  salt <- as.raw(strtoi(substring(salt_hex,
                                   seq(1, nchar(salt_hex) - 1, 2),
                                   seq(2, nchar(salt_hex),      2)), 16L))
  key  <- openssl::pbkdf2_hmac(
    password   = chartr("", "", password),
    salt       = salt,
    iterations = 200000L,
    size       = 32L,      # 256-bit
    hash       = "sha256"
  )
  key  # raw vector (32 bytes)
}

#' Generate a random hex salt (32 bytes = 64 hex chars)
random_salt_hex <- function() {
  paste(as.character(format(as.hexmode(as.integer(openssl::rand_bytes(32))),
                             width = 2)), collapse = "")
}

# ── Password management ────────────────────────────────────────────────────────

#' TRUE if a master password hash exists in the settings table
auth_password_exists <- function(db_path) {
  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con))
  res <- dbGetQuery(con, "SELECT value FROM settings WHERE key = 'master_hash'")
  nrow(res) > 0 && !is.na(res$value[1]) && nchar(res$value[1]) > 0
}

#' Store a new master password (bcrypt hash + PBKDF2 salt)
#' Returns list(success, key, message)
create_master_password <- function(db_path, password) {
  tryCatch({
    hash     <- bcrypt::hashpw(password)
    salt_hex <- random_salt_hex()
    key      <- derive_key(password, salt_hex)

    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))

    upsert_setting <- function(k, v) {
      existing <- dbGetQuery(con,
        sprintf("SELECT COUNT(*) as n FROM settings WHERE key='%s'", k))$n
      if (existing > 0) {
        dbExecute(con, "UPDATE settings SET value=? WHERE key=?", list(v, k))
      } else {
        dbExecute(con, "INSERT INTO settings(key,value) VALUES(?,?)", list(k, v))
      }
    }

    upsert_setting("master_hash", hash)
    upsert_setting("pbkdf2_salt", salt_hex)

    list(success = TRUE, key = key, message = "Password created.")
  }, error = function(e) {
    list(success = FALSE, key = NULL, message = conditionMessage(e))
  })
}

#' Verify the supplied password against stored bcrypt hash
#' Returns list(success, key, message)
verify_master_password <- function(db_path, password) {
  tryCatch({
    con <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))

    row <- dbGetQuery(con, "SELECT value FROM settings WHERE key='master_hash'")
    if (nrow(row) == 0) {
      return(list(success = FALSE, key = NULL,
                  message = "No master password configured."))
    }
    stored_hash <- row$value[1]

    if (!bcrypt::checkpw(password, stored_hash)) {
      return(list(success = FALSE, key = NULL, message = "Incorrect password."))
    }

    salt_row <- dbGetQuery(con, "SELECT value FROM settings WHERE key='pbkdf2_salt'")
    salt_hex <- salt_row$value[1]
    key      <- derive_key(password, salt_hex)

    list(success = TRUE, key = key, message = "Authenticated.")
  }, error = function(e) {
    list(success = FALSE, key = NULL, message = conditionMessage(e))
  })
}

#' Change master password — re-encrypts all vault entries
#' Returns list(success, message)
change_master_password <- function(db_path, old_password, new_password, old_key) {
  tryCatch({
    # Verify old password first
    check <- verify_master_password(db_path, old_password)
    if (!check$success) return(list(success = FALSE, message = "Old password incorrect."))

    new_result <- create_master_password(db_path, new_password)
    if (!new_result$success) return(new_result)

    new_key <- new_result$key
    con     <- dbConnect(SQLite(), db_path)
    on.exit(dbDisconnect(con))

    # Re-encrypt all vault entries
    entries <- dbGetQuery(con, "SELECT id, encrypted_password, encrypted_notes FROM vault_entries")
    if (nrow(entries) > 0) {
      for (i in seq_len(nrow(entries))) {
        pwd_plain   <- vault_decrypt(entries$encrypted_password[i], old_key)
        notes_plain <- vault_decrypt(entries$encrypted_notes[i],    old_key)
        new_pwd     <- vault_encrypt(pwd_plain,   new_key)
        new_notes   <- vault_encrypt(notes_plain, new_key)
        dbExecute(con,
          "UPDATE vault_entries SET encrypted_password=?, encrypted_notes=? WHERE id=?",
          list(new_pwd, new_notes, entries$id[i]))
      }
    }

    list(success = TRUE, message = "Password changed and vault re-encrypted.")
  }, error = function(e) {
    list(success = FALSE, message = conditionMessage(e))
  })
}

# ── AES-256-CBC Vault encryption/decryption ────────────────────────────────────

#' Encrypt a plaintext string → base64-encoded "IV:CIPHER" string
vault_encrypt <- function(plaintext, key) {
  if (is.null(plaintext) || is.na(plaintext) || nchar(trimws(plaintext)) == 0) {
    return("")
  }
  iv        <- openssl::rand_bytes(16)
  ciphertext <- openssl::aes_cbc_encrypt(
    data = chartr("", "", plaintext),
    key  = key,
    iv   = iv
  )
  # Store as base64(iv) : base64(cipher)
  paste0(base64_encode(iv), ":", base64_encode(ciphertext))
}

#' Decrypt a "IV:CIPHER" base64 string back to plaintext
vault_decrypt <- function(encrypted_str, key) {
  if (is.null(encrypted_str) || is.na(encrypted_str) ||
      nchar(trimws(encrypted_str)) == 0) return("")
  tryCatch({
    parts      <- strsplit(encrypted_str, ":", fixed = TRUE)[[1]]
    if (length(parts) < 2) return("")
    iv         <- base64_decode(parts[1])
    ciphertext <- base64_decode(paste(parts[-1], collapse = ":"))
    raw_text   <- openssl::aes_cbc_decrypt(ciphertext, key = key, iv = iv)
    rawToChar(raw_text)
  }, error = function(e) "[Decryption failed]")
}

# ── Password generator ─────────────────────────────────────────────────────────

#' Generate a random password
generate_password <- function(length = 20, upper = TRUE, lower = TRUE,
                               digits = TRUE, symbols = TRUE) {
  pool <- character(0)
  if (upper)   pool <- c(pool, LETTERS)
  if (lower)   pool <- c(pool, letters)
  if (digits)  pool <- c(pool, as.character(0:9))
  if (symbols) pool <- c(pool, strsplit("!@#$%^&*()-_=+[]{}|;:,.<>?", "")[[1]])
  if (length(pool) == 0) pool <- c(LETTERS, letters, as.character(0:9))

  bytes <- as.integer(openssl::rand_bytes(length * 2))
  chars <- pool[(bytes %% length(pool)) + 1]
  paste(chars[seq_len(length)], collapse = "")
}

# ── Utility ────────────────────────────────────────────────────────────────────
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b
