cat("Installing required packages...\n")

packages <- c(
  "shiny",
  "bslib",
  "thematic",
  "tidyverse",
  "readxl",
  "scales",
  "forcats",
  "stringi",
  "sf",
  "geodata",
  "terra",
  "patchwork"
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org", quiet = FALSE)
  } else {
    cat(pkg, "is already installed\n")
  }
}

cat("\nAll packages installed successfully!\n")
cat("You can now run the dashboard with: shiny::runApp()\n")
