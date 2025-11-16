# ===============================================================
# Epidemiology Module - Epi Curves & Seasonality
# ===============================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(scales)
library(zoo)
library(lubridate)

# Source theme config for colors
if (!exists("get_superset_colors")) {
  source("R/utils/theme_config.R")
}

#' Epidemiology UI
#' @param id Module ID
epidemiology_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(12, 12, 6, 6),

      # Weekly Epi Curve
      card(
        card_header("Weekly Epidemic Curve"),
        plotOutput(ns("epi_curve_weekly"), height = "400px")
      ),

      # Monthly Epi Curve
      card(
        card_header("Monthly Epidemic Curve"),
        plotOutput(ns("epi_curve_monthly"), height = "400px")
      ),

      # Cumulative Curve
      card(
        card_header("Cumulative Cases Over Time"),
        plotOutput(ns("cumulative_curve"), height = "400px")
      ),

      # Seasonality Ribbon
      card(
        card_header("Seasonality Pattern (by ISO Week)"),
        plotOutput(ns("seasonality_ribbon"), height = "400px")
      )
    )
  )
}

#' Epidemiology Server
#' @param id Module ID
#' @param filtered_data Reactive data frame
epidemiology_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    # Weekly Epi Curve
    output$epi_curve_weekly <- renderPlot({
      req(filtered_data())
      df <- filtered_data() %>%
        filter(!is.na(notification_date))

      if (nrow(df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0, label = "No data available", size = 6) +
                 theme_void())
      }

      # Build weekly counts with 3-week moving average
      df_week_all <- df %>%
        mutate(
          week_start = floor_date(notification_date, unit = "week", week_start = 1),
          iso_year = isoyear(notification_date),
          iso_week = isoweek(notification_date)
        ) %>%
        count(week_start, iso_year, iso_week, name = "cases") %>%
        arrange(week_start) %>%
        mutate(cumulative = cumsum(cases))

      # Find where outbreak really starts (5% of total cases)
      total_cases <- sum(df_week_all$cases)
      threshold <- total_cases * 0.05
      start_week <- df_week_all %>%
        filter(cumulative >= threshold) %>%
        pull(week_start) %>%
        min()

      # Filter to show only from outbreak start
      df_week <- df_week_all %>%
        filter(week_start >= start_week) %>%
        select(-cumulative) %>%
        mutate(ma3 = zoo::rollmean(cases, k = 3, fill = NA, align = "center"))

      # Get the actual date range of the data
      date_range <- range(df_week$week_start, na.rm = TRUE)

      ggplot(df_week, aes(x = week_start)) +
        geom_col(aes(y = cases), fill = "#7FB3A6", width = 6.5) +
        geom_line(aes(y = ma3), linewidth = 1.1, color = "#D62728") +
        scale_x_date(
          limits = date_range,
          date_breaks = "2 weeks",
          date_labels = "%b %d\n%Y",
          expand = expansion(mult = c(0.01, 0.02))
        ) +
        scale_y_continuous(labels = comma) +
        labs(
          title = "Epidemic curve (weekly) — Mpox notifications",
          subtitle = "Bars: weekly cases (Monday week start) • Line: 3-week moving average",
          x = "Notification week",
          y = "Cases"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor = element_blank()
        )
    })

    # Monthly Epi Curve
    output$epi_curve_monthly <- renderPlot({
      req(filtered_data())
      df <- filtered_data() %>%
        filter(!is.na(notification_date))

      if (nrow(df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0, label = "No data available", size = 6) +
                 theme_void())
      }

      df_month_all <- df %>%
        mutate(month = floor_date(notification_date, "month")) %>%
        count(month, name = "cases") %>%
        arrange(month) %>%
        mutate(cumulative = cumsum(cases))

      # Find where outbreak really starts (5% of total cases)
      total_cases <- sum(df_month_all$cases)
      threshold <- total_cases * 0.05
      start_month <- df_month_all %>%
        filter(cumulative >= threshold) %>%
        pull(month) %>%
        min()

      # Filter to show only from outbreak start
      df_month <- df_month_all %>%
        filter(month >= start_month) %>%
        select(-cumulative) %>%
        mutate(ma3 = zoo::rollmean(cases, k = 3, fill = NA, align = "center"))

      # Get the actual date range of the data
      date_range <- range(df_month$month, na.rm = TRUE)

      ggplot(df_month, aes(x = month)) +
        geom_col(aes(y = cases), fill = "#9F2241", width = 25) +
        geom_line(aes(y = ma3), linewidth = 1.1, color = "grey20") +
        scale_x_date(
          limits = date_range,
          date_breaks = "2 months",
          date_labels = "%b\n%Y",
          expand = expansion(mult = c(0.01, 0.02))
        ) +
        scale_y_continuous(labels = comma) +
        labs(
          title = "Epidemic curve (monthly) — Mpox notifications",
          subtitle = "Bars: monthly cases • Line: 3-month moving average",
          x = "Month",
          y = "Cases"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold"),
          axis.text.x = element_text(angle = 0, vjust = 0.5),
          panel.grid.minor = element_blank()
        )
    })

    # Cumulative Curve
    output$cumulative_curve <- renderPlot({
      req(filtered_data())
      df <- filtered_data() %>%
        filter(!is.na(notification_date))

      if (nrow(df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0, label = "No data available", size = 6) +
                 theme_void())
      }

      df_cumul_all <- df %>%
        mutate(week_start = floor_date(notification_date, unit = "week", week_start = 1)) %>%
        count(week_start, name = "cases") %>%
        arrange(week_start) %>%
        mutate(cumulative = cumsum(cases))

      # Find where outbreak really starts (5% of total cases)
      total_cases <- max(df_cumul_all$cumulative)
      threshold <- total_cases * 0.05
      start_week <- df_cumul_all %>%
        filter(cumulative >= threshold) %>%
        pull(week_start) %>%
        min()

      # Filter to show only from outbreak start
      df_cumul <- df_cumul_all %>%
        filter(week_start >= start_week)

      # Get the actual date range of the data
      date_range <- range(df_cumul$week_start, na.rm = TRUE)

      ggplot(df_cumul, aes(x = week_start)) +
        geom_area(aes(y = cumulative), fill = "#7FB3A6", alpha = 0.6) +
        geom_line(aes(y = cumulative), color = "#58595B", linewidth = 1.2) +
        scale_x_date(
          limits = date_range,
          date_breaks = "2 months",
          date_labels = "%b\n%Y"
        ) +
        scale_y_continuous(labels = comma) +
        labs(
          title = "Cumulative cases over time",
          subtitle = "Weekly aggregation",
          x = "Week",
          y = "Cumulative cases"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
    })

    # Seasonality Ribbon
    output$seasonality_ribbon <- renderPlot({
      req(filtered_data())
      df <- filtered_data() %>%
        filter(!is.na(notification_date))

      if (nrow(df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0, label = "No data available", size = 6) +
                 theme_void())
      }

      # Calculate quantiles by ISO week across all years
      df_ribbon <- df %>%
        mutate(
          iso_year = isoyear(notification_date),
          iso_week = isoweek(notification_date)
        ) %>%
        count(iso_year, iso_week, name = "cases") %>%
        group_by(iso_week) %>%
        summarise(
          min_cases = min(cases, na.rm = TRUE),
          q25 = quantile(cases, 0.25, na.rm = TRUE),
          median = median(cases, na.rm = TRUE),
          q75 = quantile(cases, 0.75, na.rm = TRUE),
          max_cases = max(cases, na.rm = TRUE),
          .groups = "drop"
        )

      ggplot(df_ribbon, aes(x = iso_week)) +
        geom_ribbon(aes(ymin = min_cases, ymax = max_cases), fill = "#7FB3A6", alpha = 0.2) +
        geom_ribbon(aes(ymin = q25, ymax = q75), fill = "#7FB3A6", alpha = 0.4) +
        geom_line(aes(y = median), color = "#58595B", linewidth = 1.2) +
        scale_x_continuous(breaks = seq(1, 53, by = 4)) +
        scale_y_continuous(labels = comma) +
        labs(
          title = "Seasonality pattern by ISO epidemiological week",
          subtitle = "Dark ribbon: IQR (Q1-Q3) • Light ribbon: min-max range • Line: median",
          x = "ISO Week",
          y = "Weekly cases"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
    })

  })
}
