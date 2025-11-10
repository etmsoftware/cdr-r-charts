# ===============================================================
# Classification & Outcomes Module
# ===============================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(scales)
library(stringr)
library(forcats)
library(lubridate)
library(tidyr)

# Source theme config for colors
if (!exists("get_superset_colors")) {
  source("R/utils/theme_config.R")
}

#' Classification UI
#' @param id Module ID
classification_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(6, 6, 12, 6, 6),

      # Final Classification Distribution
      card(
        card_header("Final Case Classification"),
        plotOutput(ns("classification_dist"), height = "400px")
      ),

      # Outcome Distribution
      card(
        card_header("Patient Outcomes"),
        plotOutput(ns("outcome_dist"), height = "400px")
      ),

      # Classification Over Time
      card(
        card_header("Case Classification Trends Over Time"),
        plotOutput(ns("classification_time"), height = "400px")
      ),

      # Classification by Province
      card(
        card_header("Classification by Province (Top 10)"),
        plotOutput(ns("classification_province"), height = "500px")
      ),

      # Outcome by Sex
      card(
        card_header("Outcomes by Sex"),
        plotOutput(ns("outcome_sex"), height = "400px")
      )
    )
  )
}

#' Classification Server
#' @param id Module ID
#' @param filtered_data Reactive data frame
classification_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    # Get theme colors
    colors <- get_superset_colors(5)

    # Final Classification Distribution
    output$classification_dist <- renderPlot({
      req(filtered_data())
      df <- filtered_data() %>%
        filter(!is.na(final_classification) & final_classification != "")

      if (nrow(df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0,
                          label = "No classification data available", size = 6) +
                 theme_void())
      }

      # Clean and standardize classification labels
      df <- df %>%
        mutate(classification = str_to_title(str_trim(final_classification)))

      tab <- df %>%
        count(classification, name = "cases") %>%
        mutate(p = cases / sum(cases)) %>%
        arrange(desc(cases)) %>%
        mutate(
          classification = factor(classification, levels = classification),
          lbl = paste0(comma(cases), "\n", percent(p, 0.1))
        )

      # Use theme colors
      n_cats <- nrow(tab)
      pal <- setNames(get_superset_colors(n_cats), tab$classification)

      ggplot(tab, aes(x = classification, y = cases, fill = classification)) +
        geom_col(width = 0.7, color = "white", linewidth = 0.4) +
        geom_text(aes(label = lbl), vjust = -0.35, lineheight = 0.95,
                  size = 4, fontface = "bold") +
        scale_fill_manual(values = pal, guide = "none") +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.10))) +
        labs(
          title = "Final Case Classification",
          subtitle = paste0("Total: ", comma(nrow(df)), " cases"),
          x = NULL,
          y = "Cases"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold", size = 16),
          plot.subtitle = element_text(color = "grey30"),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 15, hjust = 1)
        )
    })

    # Outcome Distribution
    output$outcome_dist <- renderPlot({
      req(filtered_data())
      df <- filtered_data() %>%
        filter(!is.na(case_status) & case_status != "")

      if (nrow(df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0,
                          label = "No outcome data available", size = 6) +
                 theme_void())
      }

      # Clean status labels
      df <- df %>%
        mutate(outcome = str_to_title(str_trim(case_status)))

      tab <- df %>%
        count(outcome, name = "cases") %>%
        mutate(p = cases / sum(cases)) %>%
        arrange(desc(cases)) %>%
        mutate(
          outcome = factor(outcome, levels = outcome),
          lbl = paste0(comma(cases), "\n", percent(p, 0.1))
        )

      # Use theme colors - Alive (green teal), Deceased (dark blue)
      pal <- setNames(get_superset_colors(nrow(tab)), tab$outcome)

      ggplot(tab, aes(x = outcome, y = cases, fill = outcome)) +
        geom_col(width = 0.6, color = "white", linewidth = 0.4) +
        geom_text(aes(label = lbl), vjust = -0.35, lineheight = 0.95,
                  size = 4.5, fontface = "bold") +
        scale_fill_manual(values = pal, guide = "none") +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.10))) +
        labs(
          title = "Patient Outcomes",
          subtitle = paste0("Total: ", comma(nrow(df)), " cases"),
          x = NULL,
          y = "Cases"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold", size = 16),
          plot.subtitle = element_text(color = "grey30"),
          panel.grid.minor = element_blank()
        )
    })

    # Classification Over Time
    output$classification_time <- renderPlot({
      req(filtered_data())
      df <- filtered_data() %>%
        filter(!is.na(final_classification) & final_classification != "" &
                 !is.na(notification_date))

      if (nrow(df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0,
                          label = "No time series data available", size = 6) +
                 theme_void())
      }

      # Clean classification
      df <- df %>%
        mutate(
          classification = str_to_title(str_trim(final_classification)),
          month = floor_date(notification_date, "month")
        )

      # Monthly counts
      df_time <- df %>%
        count(month, classification, name = "cases") %>%
        arrange(month)

      # Use theme colors
      classif_levels <- unique(df_time$classification)
      pal <- setNames(get_superset_colors(length(classif_levels)), classif_levels)

      ggplot(df_time, aes(x = month, y = cases, fill = classification)) +
        geom_area(position = "stack", alpha = 0.8) +
        scale_fill_manual(values = pal, name = "Classification") +
        scale_x_date(date_breaks = "1 month", date_labels = "%b\n%Y") +
        scale_y_continuous(labels = comma) +
        labs(
          title = "Case Classification Trends Over Time",
          subtitle = "Monthly aggregation (stacked area)",
          x = "Month",
          y = "Cases"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom"
        )
    })

    # Classification by Province (Top 10)
    output$classification_province <- renderPlot({
      req(filtered_data())
      df <- filtered_data() %>%
        filter(!is.na(final_classification) & final_classification != "" &
                 !is.na(province) & province != "")

      if (nrow(df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0,
                          label = "No provincial data available", size = 6) +
                 theme_void())
      }

      # Clean data
      df <- df %>%
        mutate(classification = str_to_title(str_trim(final_classification)))

      # Get top 10 provinces by total cases
      top_provinces <- df %>%
        count(province, name = "total") %>%
        arrange(desc(total)) %>%
        head(10) %>%
        pull(province)

      # Cross-tabulation
      df_prov <- df %>%
        filter(province %in% top_provinces) %>%
        count(province, classification, name = "cases") %>%
        mutate(province = factor(province,
                                levels = rev(top_provinces)))

      # Use theme colors
      classif_levels <- unique(df_prov$classification)
      pal <- setNames(get_superset_colors(length(classif_levels)), classif_levels)

      ggplot(df_prov, aes(x = province, y = cases, fill = classification)) +
        geom_col(position = "stack", width = 0.7) +
        coord_flip() +
        scale_fill_manual(values = pal, name = "Classification") +
        scale_y_continuous(labels = comma) +
        labs(
          title = "Case Classification by Province",
          subtitle = "Top 10 provinces by total cases",
          x = NULL,
          y = "Cases"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom"
        )
    })

    # Outcome by Sex
    output$outcome_sex <- renderPlot({
      req(filtered_data())
      df <- filtered_data() %>%
        filter(!is.na(case_status) & case_status != "" &
                 !is.na(sex) & sex != "Unknown")

      if (nrow(df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0,
                          label = "No outcome/sex data available", size = 6) +
                 theme_void())
      }

      # Clean data
      df <- df %>%
        mutate(outcome = str_to_title(str_trim(case_status)))

      tab <- df %>%
        count(sex, outcome, name = "cases")

      # Use theme colors for outcomes
      outcome_levels <- unique(tab$outcome)
      pal <- setNames(get_superset_colors(length(outcome_levels)), outcome_levels)

      ggplot(tab, aes(x = sex, y = cases, fill = outcome)) +
        geom_col(position = "stack", width = 0.6) +
        geom_text(aes(label = comma(cases)),
                  position = position_stack(vjust = 0.5),
                  color = "white", size = 4, fontface = "bold") +
        scale_fill_manual(values = pal, name = "Outcome") +
        scale_y_continuous(labels = comma) +
        labs(
          title = "Patient Outcomes by Sex",
          subtitle = "Stacked counts",
          x = "Sex",
          y = "Cases"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom"
        )
    })

  })
}
