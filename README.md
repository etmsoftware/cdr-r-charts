# Mpox Case Dashboard - DRC

A modern dashboard for visualizing and analyzing Mpox epidemiological data from the Democratic Republic of Congo, powered by PostgreSQL database integration.

## ✨ Features

### 📊 Interactive Visualizations

**Overview Tab:**
- Real-time value boxes showing case counts
- Interactive age-sex pyramid
- Summary statistics by sex

**Age Analysis Tab:**
- Violin + box plot showing age distribution patterns
- Age group bar charts (counts and percentages)
- Comprehensive statistical summaries

**Geographic Tab:**
- Interactive choropleth map of DRC provinces
- Top 15 provinces by case count
- Provincial statistics table with percentages

**Analytics Tab:**
- Density curves comparing distributions
- Notched box plots with confidence intervals
- Searchable data table
- CSV export functionality

### 🔧 Interactive Filters
- **Province**: Multi-select dropdown (14 provinces)
- **Sex**: All, Male, or Female
- **Age Range**: Slider (0-110 years)

## 🚀 Quick Start

### 1. Install Required Packages

```r
source("install_packages.R")
```

### 2. Configure Database Connection

```bash
# Copy the example environment file
cp .env.example .env

# Edit .env with your PostgreSQL credentials
nano .env  # or use your preferred editor
```

Update the following values in `.env`:
```bash
DB_HOST=your_postgres_host
DB_PORT=5432
DB_NAME=your_database_name
DB_USER=your_username
DB_PASSWORD=your_secure_password
```

### 3. Test Database Connection

```r
# Test the connection
source("test_db_connection.R")

# Test the data loader
source("test_data_loader.R")
```

### 4. Run the Dashboard

```r
# Option 1: Using the run script
source("run_app.R")

# Option 2: Direct run
shiny::runApp()
```

---

## 🚀 Deploying to Posit Connect Cloud

The application is fully compatible with Posit Connect Cloud:

**Quick Start**: See [DEPLOYMENT_QUICK_START.md](DEPLOYMENT_QUICK_START.md)

**Complete Guide**: See [POSIT_CONNECT_DEPLOYMENT.md](POSIT_CONNECT_DEPLOYMENT.md)

**Key Points:**
- ✅ Uses server environment variables (set in Posit Connect UI)
- ✅ No `.env` file needed on server
- ✅ Same code works locally and on server
- ✅ Environment variables take precedence over `.env`

## 📁 Project Structure

```
cdr-r-chars/
├── app.R                      # Main Shiny application
├── run_app.R                  # Application launcher script
├── config.yml                 # Database configuration
├── .env                       # Database credentials (not in git)
├── .env.example              # Example environment variables
├── install_packages.R        # Package installation script
├── test_db_connection.R      # Database connection test
├── test_data_loader.R        # Data loader test
├── R/
│   ├── utils/
│   │   ├── db_connection.R   # PostgreSQL connection utilities
│   │   ├── data_loader.R     # Data processing & mapping
│   │   └── theme_config.R    # Modern theme configuration
│   └── modules/
│       ├── mod_overview.R     # Overview tab
│       ├── mod_age_analysis.R # Age analysis tab
│       ├── mod_geographic.R   # Geographic tab
│       └── mod_analytics.R    # Analytics tab
└── www/
    └── logo.png
```

## 📊 Data Source

### PostgreSQL Database
- **View**: `v_mpox_drc`
- **Records**: 15,120+ cases
- **Provinces**: 22 unique provinces
- **Fields**: 44+ columns from the database
- **Completeness**:
  - Sex data: 91% (13,780 records)
  - Age data: 92% (13,879 records)
  - Province data: 99% (14,932 records)

### Data Mapping
PostgreSQL fields are automatically mapped to R data frame:
- `Sex` → `case_sex` / `sex` (standardized)
- `Age In Years` / `Date Of Birth` → `case_age`
- `Reporting Location (subnational)` → `province`
- Age groups automatically calculated from `case_age`

## 🛠 Technology Stack

### Core Framework
- **Shiny** - Interactive web framework
- **bslib** - Modern Bootstrap 5 theming

### Data & Database
- **PostgreSQL** - Database backend
- **RPostgres** - PostgreSQL driver
- **DBI** - Database interface
- **pool** - Connection pooling for Shiny
- **config** - Configuration management

### Visualization & Analysis
- **ggplot2** - Professional visualizations
- **tidyverse** - Data manipulation
- **sf/geodata** - Geospatial mapping
- **DT** - Interactive tables

## 🎨 Customization

### Change Colors
Edit `R/utils/theme_config.R`:
```r
get_color_palette <- function() {
  c("Female" = "#E63946", "Male" = "#457B9D", ...)
}
```

### Add New Tab
1. Create module in `R/modules/mod_newtab.R`
2. Define `newtab_ui()` and `newtab_server()`
3. Source in `app.R` and add `nav_panel()`

## 🔧 Troubleshooting

### Connection Issues

**Problem**: Can't connect to database
```
Error: Failed to create database connection pool
```

**Solutions**:
1. Verify `.env` file exists and contains correct credentials
2. Check PostgreSQL server is running and accessible
3. Verify network connectivity to database host
4. Confirm user has proper permissions
5. Check firewall settings allow port 5432 (or your DB port)

### Package Installation Issues

**Problem**: Missing packages
```
Error: there is no package called 'RPostgres'
```

**Solution**:
```r
source("install_packages.R")
```

### Data Loading Issues

**Problem**: Data not loading or mapping incorrectly

**Solution**:
```r
# Test the data loader
source("test_data_loader.R")

# Check database view structure
source("test_db_connection.R")
```

## 📝 License

Internal use only.

---

**Version**: 3.0 | **Updated**: 2025-11-02 | **Database**: PostgreSQL Integration
