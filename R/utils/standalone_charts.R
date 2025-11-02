# ===============================================================
# Standalone Chart UIs for iFrame Embedding
# ===============================================================

#' Get standalone UI for a specific chart
#' @param chart_name Character string of chart name
#' @param ns Namespace function
#' @return Shiny UI element
get_standalone_chart_ui <- function(chart_name, ns = NS("standalone")) {

  switch(chart_name,

    # Overview - Value Boxes
    "value_boxes" = tagList(
      value_box(
        title = "Total Cases",
        value = textOutput(ns("total_cases")),
        theme = "primary",
        class = "border-0 shadow-sm"
      ),
      value_box(
        title = "Male Cases",
        value = textOutput(ns("male_cases")),
        theme = "info",
        class = "border-0 shadow-sm"
      ),
      value_box(
        title = "Female Cases",
        value = textOutput(ns("female_cases")),
        theme = "danger",
        class = "border-0 shadow-sm"
      )
    ),

    "total_cases" = value_box(
      title = "Total Cases",
      value = textOutput(ns("total_cases")),
      theme = "primary",
      class = "border-0 shadow-sm",
      full_screen = FALSE
    ),

    "male_cases" = value_box(
      title = "Male Cases",
      value = textOutput(ns("male_cases")),
      theme = "info",
      class = "border-0 shadow-sm",
      full_screen = FALSE
    ),

    "female_cases" = value_box(
      title = "Female Cases",
      value = textOutput(ns("female_cases")),
      theme = "danger",
      class = "border-0 shadow-sm",
      full_screen = FALSE
    ),

    # Overview - Pyramid
    "pyramid" = plotOutput(ns("pyramid_plot"), height = "600px"),

    # Overview - Summary
    "overview_summary" = tableOutput(ns("summary_table")),

    # Age Analysis - Violin Plot
    "violin_plot" = plotOutput(ns("violin_plot"), height = "600px"),

    # Age Analysis - Age Group Bar
    "age_group_bar" = plotOutput(ns("age_group_plot"), height = "550px"),

    # Age Analysis - Stats Table
    "age_stats" = tableOutput(ns("age_stats_table")),

    # Geographic - Map
    "map" = plotOutput(ns("map_plot"), height = "700px"),

    # Geographic - Top Provinces
    "top_provinces" = plotOutput(ns("top_provinces_plot"), height = "550px"),

    # Geographic - Province Table
    "province_table" = tableOutput(ns("province_stats_table")),

    # Analytics - Density Curve
    "density_curve" = plotOutput(ns("density_plot"), height = "550px"),

    # Analytics - Boxplot
    "boxplot" = plotOutput(ns("boxplot_plot"), height = "550px"),

    # Analytics - Data Table
    "data_table" = DTOutput(ns("data_table")),

    # Default
    div(
      class = "alert alert-warning",
      h4("Chart not found"),
      p("The requested chart '", chart_name, "' is not available."),
      p("Please check the chart name and try again.")
    )
  )
}

#' Create standalone page UI
#' @param chart_name Character string of chart name
#' @param params Query parameters
#' @return Shiny page UI
create_standalone_ui <- function(chart_name, params) {
  ns <- NS("standalone")

  title_text <- get_chart_title(chart_name)

  page_fillable(
    title = title_text,
    theme = get_app_theme(),
    padding = 0,
    fillable_mobile = TRUE,

    # Optional: Show title bar
    if (!params$hide_header) {
      div(
        style = "padding: 10px; margin: 0; background: transparent;",
        h4(style = "margin: 0; font-weight: 600;", title_text)
      )
    },

    # Chart content - no padding, no background
    div(
      style = "width: 100%; height: 100%; margin: 0; padding: 0;",
      get_standalone_chart_ui(chart_name, ns)
    )
  )
}
