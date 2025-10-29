# Mpox Case Dashboard - DRC

A modern dashboard for visualizing and analyzing Mpox epidemiological data from the Democratic Republic of Congo.

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

```r
# Install packages
source("install_packages.R")

# Run dashboard
shiny::runApp()
```

## 📁 Project Structure

```
cdr-r-chars/
├── app.R                     # Main application
├── data/
│   └── complete_case_dataset_sim_final.xlsx
├── R/
│   ├── utils/
│   │   ├── data_loader.R    # Data processing module
│   │   └── theme_config.R   # Modern theme configuration
│   └── modules/
│       ├── mod_overview.R    # Overview tab
│       ├── mod_age_analysis.R    # Age analysis tab
│       ├── mod_geographic.R  # Geographic tab
│       └── mod_analytics.R   # Analytics tab
└── www/
    └── logo.png
```

## 📊 Data

- **Cases**: 2,764 records
- **Provinces**: 14
- **Variables**: 59 columns including demographics and geographic data

## 🛠 Technology Stack

- **Shiny** - Interactive web framework
- **bslib** - Modern Bootstrap 5 theming
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

## 📝 License

Internal use only.

---

**Version**: 2.0 | **Updated**: 2025-10-28
