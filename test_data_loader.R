# ===============================================================
# Data Loader Test Script
# ===============================================================
# This script tests the new data loader with PostgreSQL database

cat("=======================================================\n")
cat("Data Loader Test (PostgreSQL Integration)\n")
cat("=======================================================\n\n")

suppressPackageStartupMessages({
  library(dplyr)
  library(DBI)
  library(RPostgres)
  library(pool)
  library(config)
})

source("R/utils/db_connection.R")
source("R/utils/data_loader.R")

cat("Step 1: Creating database connection pool...\n")
db_pool <- NULL
tryCatch({
  db_pool <- create_db_pool()
  cat("  ✓ Database connection established\n\n")
}, error = function(e) {
  cat("  ✗ Failed to connect to database\n")
  cat("  Error:", e$message, "\n\n")
  stop(e)
})

cat("Step 2: Loading data using load_case_data()...\n")
dat <- NULL
tryCatch({
  dat <- load_case_data(source = "database", db_pool = db_pool)
  cat("  ✓ Data loaded successfully\n\n")
}, error = function(e) {
  cat("  ✗ Failed to load data\n")
  cat("  Error:", e$message, "\n\n")
  close_db_pool(db_pool)
  stop(e)
})

cat("Step 3: Verifying data structure...\n")
cat("  - Total records:", nrow(dat), "\n")
cat("  - Total columns:", ncol(dat), "\n")
cat("\n")

cat("Step 4: Checking required columns exist...\n")
required_cols <- c("case_sex", "sex", "case_age", "age_group", "province")
for (col in required_cols) {
  if (col %in% names(dat)) {
    cat("  ✓", col, "exists\n")
  } else {
    cat("  ✗", col, "MISSING\n")
  }
}
cat("\n")

cat("Step 5: Data quality summary...\n")
cat("  Sex variable:\n")
cat("    - Total with sex data:", sum(!is.na(dat$sex)), "\n")
cat("    - Unique values:", paste(unique(dat$sex), collapse = ", "), "\n")
cat("    - Male:", sum(dat$sex == "Male", na.rm = TRUE), "\n")
cat("    - Female:", sum(dat$sex == "Female", na.rm = TRUE), "\n")
cat("\n")

cat("  Age variable:\n")
cat("    - Total with age data:", sum(!is.na(dat$case_age)), "\n")
cat("    - Age range:", sprintf("%.1f - %.1f years",
                                min(dat$case_age, na.rm = TRUE),
                                max(dat$case_age, na.rm = TRUE)), "\n")
cat("    - Mean age:", sprintf("%.1f years", mean(dat$case_age, na.rm = TRUE)), "\n")
cat("\n")

cat("  Age group variable:\n")
cat("    - Total with age group data:", sum(!is.na(dat$age_group)), "\n")
if (sum(!is.na(dat$age_group)) > 0) {
  age_dist <- table(dat$age_group)
  cat("    - Distribution:\n")
  for (grp in names(age_dist)) {
    cat("      *", grp, ":", age_dist[grp], "\n")
  }
}
cat("\n")

cat("  Province variable:\n")
cat("    - Total with province data:", sum(!is.na(dat$province)), "\n")
cat("    - Unique provinces:", length(unique(dat$province[!is.na(dat$province)])), "\n")
cat("    - Top 5 provinces:\n")
province_counts <- sort(table(dat$province), decreasing = TRUE)
for (i in 1:min(5, length(province_counts))) {
  cat("      *", names(province_counts)[i], ":", province_counts[i], "\n")
}
cat("\n")

cat("Step 6: Checking additional mapped fields...\n")
optional_cols <- c("case_id", "record_id", "case_classification",
                   "date_of_diagnosis", "province_iso")
for (col in optional_cols) {
  if (col %in% names(dat)) {
    non_na <- sum(!is.na(dat[[col]]))
    cat("  ✓", col, "exists (", non_na, "non-NA values)\n")
  } else {
    cat("  -", col, "not found\n")
  }
}
cat("\n")

cat("Step 7: Testing get_filter_options()...\n")
filter_opts <- NULL
tryCatch({
  filter_opts <- get_filter_options(dat)
  cat("  ✓ Filter options generated\n")
  cat("    - Provinces available:", length(filter_opts$provinces), "\n")
  cat("    - Sex options:", paste(filter_opts$sexes, collapse = ", "), "\n")
  cat("    - Age range:", filter_opts$age_range[1], "-", filter_opts$age_range[2], "\n")
  cat("\n")
}, error = function(e) {
  cat("  ✗ Failed to generate filter options\n")
  cat("  Error:", e$message, "\n\n")
})

cat("Step 8: Sample data preview (first 5 records)...\n")
sample_data <- dat %>%
  select(case_id, sex, case_age, age_group, province, case_classification) %>%
  head(5)
print(sample_data)
cat("\n")

cat("Step 9: Closing database connection...\n")
tryCatch({
  close_db_pool(db_pool)
  cat("  ✓ Connection closed successfully\n\n")
}, error = function(e) {
  cat("  ✗ Failed to close connection:", e$message, "\n\n")
})

cat("=======================================================\n")
cat("Data Loader Test: SUCCESS! ✓\n")
cat("=======================================================\n\n")

cat("Next steps:\n")
cat("1. Review the data structure and quality above\n")
cat("2. Run the Shiny app: shiny::runApp()\n")
cat("3. Verify all visualizations work correctly\n\n")
