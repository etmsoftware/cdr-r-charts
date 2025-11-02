# ===============================================================
# Run Mpox Dashboard Application
# ===============================================================

cat("=======================================================\n")
cat("Starting Mpox Dashboard - DRC\n")
cat("=======================================================\n\n")

cat("Checking prerequisites...\n")

if (!file.exists(".env")) {
  stop("\n✗ .env file not found!\n",
       "Please create .env file from .env.example and configure your database credentials.\n",
       "Command: cp .env.example .env\n")
}
cat("✓ .env file found\n")

required_packages <- c("shiny", "DBI", "RPostgres", "pool", "config", "dplyr")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop("\n✗ Missing required packages: ", paste(missing_packages, collapse = ", "), "\n",
       "Please run: source('install_packages.R')\n")
}
cat("✓ All required packages installed\n\n")

cat("Starting Shiny application...\n")
cat("The dashboard will open in your browser.\n")
cat("Press Ctrl+C or Cmd+C to stop the application.\n\n")

shiny::runApp(launch.browser = TRUE)
