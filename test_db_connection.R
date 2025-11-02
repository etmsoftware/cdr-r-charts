# ===============================================================
# Database Connection Test Script
# ===============================================================
# This script tests the database connection and queries sample data
# Run this before integrating with the main Shiny app

cat("=======================================================\n")
cat("PostgreSQL Database Connection Test\n")
cat("=======================================================\n\n")

# Load required libraries
suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(pool)
  library(config)
  library(dplyr)
})

# Source the database connection utilities
source("R/utils/db_connection.R")

# Step 1: Check if .env file exists
cat("Step 1: Checking for .env file...\n")
if (file.exists(".env")) {
  cat("  ✓ .env file found\n\n")
} else {
  cat("  ✗ .env file NOT found\n")
  cat("  Please create .env file from .env.example and add your credentials\n")
  cat("  Command: cp .env.example .env\n\n")
  stop("Missing .env file")
}

cat("Step 2: Loading configuration...\n")
tryCatch({
  load_env()
  cat("  ✓ Environment variables loaded\n\n")
}, error = function(e) {
  cat("  ✗ Failed to load environment variables:", e$message, "\n\n")
  stop(e)
})

cat("Step 3: Creating database connection pool...\n")
pool <- NULL
tryCatch({
  pool <- create_db_pool()
  cat("  ✓ Connection pool created successfully\n\n")
}, error = function(e) {
  cat("  ✗ Failed to create connection pool\n")
  cat("  Error:", e$message, "\n\n")
  stop(e)
})

cat("Step 4: Testing database connection...\n")
tryCatch({
  is_valid <- test_db_connection(pool)
  if (is_valid) {
    cat("  ✓ Connection test passed\n\n")
  }
}, error = function(e) {
  cat("  ✗ Connection test failed:", e$message, "\n\n")
  close_db_pool(pool)
  stop(e)
})

cat("Step 5: Retrieving database information...\n")
tryCatch({
  db_info <- get_db_info(pool)
  cat("  Database:", db_info$database, "\n")
  cat("  Host:", db_info$host, "\n")
  cat("  Port:", db_info$port, "\n")
  cat("  User:", db_info$user, "\n")
  cat("  Server Version:", db_info$server_version, "\n\n")
}, error = function(e) {
  cat("  ✗ Failed to get database info:", e$message, "\n\n")
})

cat("Step 6: Querying sample data from v_mpox_drc (first 10 rows)...\n")
sample_data <- NULL
tryCatch({
  sample_data <- query_mpox_data(pool, limit = 10)
  cat("  ✓ Sample data retrieved successfully\n\n")

  cat("  Data preview:\n")
  cat("  - Total rows:", nrow(sample_data), "\n")
  cat("  - Total columns:", ncol(sample_data), "\n")
  cat("  - Column names (first 10):\n")
  for (col in head(names(sample_data), 10)) {
    cat("    *", col, "\n")
  }
  cat("\n")

  cat("  First 3 records:\n")
  print(head(sample_data, 3))
  cat("\n")

}, error = function(e) {
  cat("  ✗ Failed to query sample data:", e$message, "\n\n")
  close_db_pool(pool)
  stop(e)
})

cat("Step 7: Querying FULL dataset from v_mpox_drc...\n")
full_data <- NULL
tryCatch({
  full_data <- query_mpox_data(pool)
  cat("  ✓ Full dataset retrieved successfully\n")
  cat("  - Total records:", nrow(full_data), "\n")
  cat("  - Total fields:", ncol(full_data), "\n\n")

  cat("  Data Quality Summary:\n")
  cat("  - Records with Case ID:", sum(!is.na(full_data$`Case ID`)), "\n")
  cat("  - Records with Sex:", sum(!is.na(full_data$Sex)), "\n")
  cat("  - Records with Age In Years:", sum(!is.na(full_data$`Age In Years`)), "\n")
  cat("  - Records with Province:", sum(!is.na(full_data$`Reporting Location (subnational)`)), "\n")
  cat("  - Unique provinces:", length(unique(full_data$`Reporting Location (subnational)`)), "\n")
  cat("\n")

}, error = function(e) {
  cat("  ✗ Failed to query full dataset:", e$message, "\n\n")
  close_db_pool(pool)
  stop(e)
})

cat("Step 8: Closing database connection...\n")
tryCatch({
  close_db_pool(pool)
  cat("  ✓ Connection closed successfully\n\n")
}, error = function(e) {
  cat("  ✗ Failed to close connection:", e$message, "\n\n")
})

cat("=======================================================\n")
cat("Database Connection Test: SUCCESS! ✓\n")
cat("=======================================================\n\n")

cat("Next steps:\n")
cat("1. Review the data structure above\n")
cat("2. Update data_loader.R to map PostgreSQL fields to R data frame\n")
cat("3. Test with Shiny app\n\n")

cat("To save full data for inspection, run:\n")
cat("  saveRDS(full_data, 'test_data.rds')\n\n")
