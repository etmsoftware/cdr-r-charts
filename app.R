# ===============================================================
# Mpox Case Dashboard - DRC
# ===============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(thematic)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readxl)
  library(readr)
  library(stringr)
  library(forcats)
  library(scales)
  library(DT)
  library(lubridate)
  library(zoo)
})

has_geo_packages <- FALSE
tryCatch({
  suppressPackageStartupMessages({
    library(sf)
    library(geodata)
    library(terra)
  })
  has_geo_packages <- TRUE
}, error = function(e) {
  message("Geospatial packages not available. Map will be disabled.")
})

source("R/utils/db_connection.R")
source("R/utils/data_loader.R")
source("R/utils/theme_config.R")
source("R/utils/query_params.R")
source("R/utils/standalone_charts.R")

source("R/modules/mod_overview.R")
source("R/modules/mod_age_analysis.R")
source("R/modules/mod_geographic.R")
source("R/modules/mod_analytics.R")
source("R/modules/mod_epidemiology.R")
source("R/modules/mod_lab_results.R")
source("R/modules/mod_classification.R")

# Create database connection pool
message("Initializing database connection...")
db_pool <- create_db_pool()

# Load data from database
dat <- load_case_data(source = "database", db_pool = db_pool)

filter_opts <- get_filter_options(dat)

drc_sf <- NULL
if (has_geo_packages) {
  tryCatch({
    drc_sf <- geodata::gadm(country = "COD", level = 1, path = tempdir()) %>%
      st_as_sf() %>%
      rename(province_gadm = NAME_1) %>%
      mutate(province_gadm_norm = stringi::stri_trans_general(province_gadm, "Latin-ASCII") %>%
               str_to_lower() %>%
               str_replace_all("[^a-z0-9]+", " ") %>%
               str_squish())
  }, error = function(e) {
    message("Could not load DRC shapefile.")
  })
}

apply_modern_theme()
thematic_shiny()

# UI function - will be conditional based on query params
ui <- function(request) {
  # Parse query parameters
  query <- parseQueryString(request$QUERY_STRING)
  chart_name <- query$chart

  # Check if standalone mode
  if (!is.null(chart_name) && is_valid_chart(chart_name)) {
    # Standalone mode - show only the requested chart
    params <- list(
      chart = chart_name,
      hide_filters = as.logical(query$hide_filters %||% FALSE),
      hide_header = as.logical(query$hide_header %||% FALSE)
    )
    return(create_standalone_ui(chart_name, params))
  }

  # Default: Full dashboard
  page_navbar(
  title = "Mpox Dashboard - DRC1",
  id = "main_nav",
  theme = get_app_theme(),
  fillable = TRUE,

  sidebar = sidebar(
    id = "sidebar",
    title = tags$div(
      style = "font-size: 1.2rem; font-weight: bold;",
      "Filters"
    ),

    selectInput(
      "province_filter",
      "Provinces",
      choices = filter_opts$provinces,
      selected = filter_opts$provinces,
      multiple = TRUE,
      selectize = TRUE
    ),

    selectInput(
      "sex_filter",
      "Sex",
      choices = filter_opts$sexes,
      selected = "All",
      multiple = FALSE
    ),

    sliderInput(
      "age_filter",
      "Age Range",
      min = filter_opts$age_range[1],
      max = filter_opts$age_range[2],
      value = filter_opts$age_range,
      step = 1,
      post = " years"
    ),

    hr(),

    tags$div(
      class = "small text-muted",
      style = "line-height: 1.5;",
      "This dashboard provides comprehensive analysis of Mpox surveillance data ",
      "from the Democratic Republic of Congo. Use filters to explore different ",
      "segments of the data."
    ),

    tags$div(
      style = "margin-top: 20px; text-align: center;",
      tags$img(
        src = "https://africacdc.org/wp-content/uploads/2020/02/AfricaCDC_Logo.png",
        width = "80%",
        style = "opacity: 0.8; border-radius: 8px;"
      )
    )
  ),

  nav_panel(
    title = tagList(icon("house"), "Overview"),
    value = "overview",
    overview_ui("overview")
  ),

  nav_panel(
    title = tagList(icon("chart-line"), "Age Analysis"),
    value = "age_analysis",
    age_analysis_ui("age_analysis")
  ),

  nav_panel(
    title = tagList(icon("map"), "Geography"),
    value = "geographic",
    geographic_ui("geographic")
  ),

  nav_panel(
    title = tagList(icon("table"), "Analytics"),
    value = "analytics",
    analytics_ui("analytics")
  ),

  nav_panel(
    title = tagList(icon("chart-area"), "Epidemiology"),
    value = "epidemiology",
    epidemiology_ui("epidemiology")
  ),

  nav_panel(
    title = tagList(icon("flask"), "Lab Results"),
    value = "lab_results",
    lab_results_ui("lab_results")
  ),

  nav_panel(
    title = tagList(icon("clipboard-check"), "Classification"),
    value = "classification",
    classification_ui("classification")
  ),

  nav_panel(
    title = tagList(icon("info-circle"), "About"),
    value = "about",
    layout_columns(
      card(
        class = "border-0 shadow-sm",
        card_header(
          class = "bg-transparent border-0",
          "About This Dashboard"
        ),
        card_body(
          tags$h4("Mpox Case Surveillance Dashboard"),
          tags$p(
            "This interactive dashboard provides comprehensive visualization and ",
            "analysis of Mpox (Monkeypox) case data from the Democratic Republic of Congo."
          ),

          tags$h5("Features"),
          tags$ul(
            tags$li("Interactive filtering by province, sex, and age"),
            tags$li("Age-sex pyramid with population distribution"),
            tags$li("Detailed age distribution analysis"),
            tags$li("Geographic choropleth mapping"),
            tags$li("Statistical summaries and data export")
          ),

          tags$h5("Data Source"),
          tags$p(
            "Case surveillance data from DRC health authorities. ",
            "The dataset includes demographic and geographic information for ",
            comma(nrow(dat)), " reported cases."
          ),

          tags$h5("Technical Stack"),
          tags$ul(
            tags$li(tags$strong("Framework: "), "Shiny with bslib"),
            tags$li(tags$strong("Visualization: "), "ggplot2 with custom theme"),
            tags$li(tags$strong("Geospatial: "), "sf, geodata, GADM boundaries"),
            tags$li(tags$strong("Architecture: "), "Modular design with separate modules")
          ),

          tags$hr(),

          tags$p(
            class = "text-muted small",
            "Dashboard version 2.0 | ",
            "Last updated: ", Sys.Date()
          )
        )
      ),
      col_widths = 12
    )
  )
  )  # End page_navbar
}  # End ui function

server <- function(input, output, session) {

  # Parse query parameters (reactive)
  params <- parse_query_params(session)

  # Check standalone mode (reactive)
  standalone_mode <- reactive({
    is_standalone_mode(params())
  })

  # Close database connection when session ends
  session$onSessionEnded(function() {
    message("Shiny session ended, closing database connection...")
    close_db_pool(db_pool)
  })

  # Filtered data reactive
  filtered_data <- reactive({
    data <- dat

    # In standalone mode, apply URL filters
    if (standalone_mode()) {
      data <- apply_url_filters(data, params(), filter_opts)
      return(data)
    }

    # Full dashboard mode - apply UI filters
    # Apply province filter - include records without province data
    if (!is.null(input$province_filter) && length(input$province_filter) > 0) {
      data <- data %>% filter(
        is.na(province) | province == "" | province %in% input$province_filter
      )
    }

    if (input$sex_filter != "All") {
      data <- data %>% filter(sex == input$sex_filter)
    }

    # Apply age filter only to rows that have age data
    # Keep rows without age data (NA) so total count is accurate
    data <- data %>%
      filter(
        is.na(case_age) |
        (case_age >= input$age_filter[1] & case_age <= input$age_filter[2])
      )

    data
  })

  # Standalone mode value boxes
  moduleServer("standalone", function(input, output, session) {
    output$total_cases <- renderText({ comma(nrow(filtered_data())) })
    output$male_cases <- renderText({
      comma(sum(filtered_data()$sex == "Male", na.rm = TRUE))
    })
    output$female_cases <- renderText({
      comma(sum(filtered_data()$sex == "Female", na.rm = TRUE))
    })
  })

  # Call all module servers (they only render if their UI is present)
  overview_server("standalone", filtered_data)
  age_analysis_server("standalone", filtered_data)
  geographic_server("standalone", filtered_data, drc_sf)
  analytics_server("standalone", filtered_data)

  # Full dashboard modules
  overview_server("overview", filtered_data)
  age_analysis_server("age_analysis", filtered_data)
  geographic_server("geographic", filtered_data, drc_sf)
  analytics_server("analytics", filtered_data)
  epidemiology_server("epidemiology", filtered_data)
  lab_results_server("lab_results", filtered_data)
  classification_server("classification", filtered_data)
}

shinyApp(ui = ui, server = server)
