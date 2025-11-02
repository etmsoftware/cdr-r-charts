# ===============================================================
# URL Query Parameter Utilities for iFrame Embedding
# ===============================================================

#' Parse URL query parameters from session
#' @param session Shiny session object
#' @return Reactive list of parsed parameters
parse_query_params <- function(session) {
  reactive({
    query <- shiny::parseQueryString(session$clientData$url_search)

    list(
      chart = query$chart %||% NULL,
      hide_filters = as.logical(query$hide_filters %||% FALSE),
      hide_header = as.logical(query$hide_header %||% FALSE),
      fullscreen = as.logical(query$fullscreen %||% FALSE),

      # Filter parameters
      provinces = if (!is.null(query$provinces)) {
        strsplit(query$provinces, ",")[[1]]
      } else {
        NULL
      },
      sex = query$sex %||% "All",
      age_min = as.numeric(query$age_min %||% 0),
      age_max = as.numeric(query$age_max %||% 150)
    )
  })
}

#' Validate chart name
#' @param chart_name Character string of chart name
#' @return TRUE if valid, FALSE otherwise
is_valid_chart <- function(chart_name) {
  if (is.null(chart_name)) return(FALSE)

  valid_charts <- c(
    # Overview tab
    "value_boxes", "total_cases", "male_cases", "female_cases",
    "pyramid", "overview_summary",

    # Age analysis tab
    "violin_plot", "age_group_bar", "age_stats",

    # Geographic tab
    "map", "top_provinces", "province_table",

    # Analytics tab
    "density_curve", "boxplot", "data_table"
  )

  chart_name %in% valid_charts
}

#' Check if app is in standalone mode
#' @param params Parsed query parameters
#' @return TRUE if standalone mode, FALSE otherwise
is_standalone_mode <- function(params) {
  !is.null(params$chart) && is_valid_chart(params$chart)
}

#' Apply URL filters to data
#' @param data Data frame
#' @param params Parsed query parameters
#' @param filter_opts Filter options from get_filter_options
#' @return Filtered data frame
apply_url_filters <- function(data, params, filter_opts) {

  # Province filter
  if (!is.null(params$provinces) && length(params$provinces) > 0) {
    data <- data %>%
      filter(
        is.na(province) | province == "" | province %in% params$provinces
      )
  }

  # Sex filter
  if (!is.null(params$sex) && params$sex != "All") {
    data <- data %>%
      filter(sex == params$sex)
  }

  # Age filter
  if (!is.null(params$age_min) || !is.null(params$age_max)) {
    age_min <- params$age_min %||% filter_opts$age_range[1]
    age_max <- params$age_max %||% filter_opts$age_range[2]

    data <- data %>%
      filter(
        is.na(case_age) |
        (case_age >= age_min & case_age <= age_max)
      )
  }

  return(data)
}

#' Get chart title for standalone mode
#' @param chart_name Character string of chart name
#' @return Character string of title
get_chart_title <- function(chart_name) {
  titles <- list(
    "value_boxes" = "Case Summary",
    "total_cases" = "Total Cases",
    "male_cases" = "Male Cases",
    "female_cases" = "Female Cases",
    "pyramid" = "Age-Sex Distribution Pyramid",
    "overview_summary" = "Summary Statistics",
    "violin_plot" = "Age Distribution by Sex",
    "age_group_bar" = "Age Group Distribution",
    "age_stats" = "Age Statistics by Sex",
    "map" = "Geographic Distribution",
    "top_provinces" = "Top Provinces by Cases",
    "province_table" = "Province Statistics",
    "density_curve" = "Age Density Distribution",
    "boxplot" = "Age Distribution Boxplot",
    "data_table" = "Case Data Table"
  )

  titles[[chart_name]] %||% "Dashboard"
}
