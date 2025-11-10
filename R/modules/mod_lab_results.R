# ===============================================================
# Lab Results Module
# ===============================================================

library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(scales)
library(stringr)
library(forcats)

# Source theme config for colors
if (!exists("get_superset_colors")) {
  source("R/utils/theme_config.R")
}

#' Lab Results UI
#' @param id Module ID
lab_results_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(6, 6, 12),

      # Lab Result Distribution (Bar Chart)
      card(
        card_header("Lab Result Distribution"),
        plotOutput(ns("lab_result_bars"), height = "400px")
      ),

      # Lab Result Composition (Pie Chart)
      card(
        card_header("Lab Result Composition"),
        plotOutput(ns("lab_result_pie"), height = "400px")
      ),

      # Lab Result Summary Table
      card(
        card_header("Lab Result Summary"),
        tableOutput(ns("lab_result_table"))
      )
    )
  )
}

#' Lab Results Server
#' @param id Module ID
#' @param filtered_data Reactive data frame
lab_results_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    # Process lab results
    lab_data <- reactive({
      req(filtered_data())
      df <- filtered_data()

      # Normalize lab_results field
      df %>%
        mutate(
          lab_result_clean = case_when(
            str_detect(str_to_lower(lab_results), "positif|positive|pos") ~ "Positive",
            str_detect(str_to_lower(lab_results), "negatif|négatif|negative|neg") ~ "Negative",
            str_detect(str_to_lower(lab_results), "indetermina|indéterminé") ~ "Indeterminate",
            str_detect(str_to_lower(lab_results), "invalide|invalid") ~ "Invalid",
            is.na(lab_results) | lab_results == "" ~ "Missing / Not recorded",
            TRUE ~ "Missing / Not recorded"
          )
        ) %>%
        filter(!is.na(lab_result_clean))
    })

    # Lab Result Bar Chart
    output$lab_result_bars <- renderPlot({
      req(lab_data())
      df <- lab_data()

      if (nrow(df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0, label = "No lab result data available", size = 6) +
                 theme_void())
      }

      # Count and calculate percentages
      tab <- df %>%
        count(lab_result_clean, name = "cases") %>%
        mutate(p = cases / sum(cases)) %>%
        arrange(desc(cases)) %>%
        mutate(
          lab_result_clean = factor(lab_result_clean, levels = lab_result_clean),
          lbl = paste0(comma(cases), "\n", percent(p, 0.1))
        )

      # Use theme colors - intuitive healthcare colors
      theme_colors <- get_superset_colors(12)
      pal <- c(
        "Positive" = "#E74C3C",             # Red (danger) - positive disease result
        "Negative" = "#4EB69D",             # Green (success) - negative disease result
        "Indeterminate" = theme_colors[5],  # Light blue - uncertain
        "Invalid" = theme_colors[11],       # Darker teal - error
        "Missing / Not recorded" = "grey70" # Grey for missing
      )

      ggplot(tab, aes(x = lab_result_clean, y = cases, fill = lab_result_clean)) +
        geom_col(width = 0.7, color = "white", linewidth = 0.4) +
        geom_text(aes(label = lbl), vjust = -0.35, lineheight = 0.95,
                  size = 4.2, fontface = "bold") +
        scale_fill_manual(values = pal, guide = "none") +
        scale_y_continuous(labels = comma, expand = expansion(mult = c(0, 0.10))) +
        labs(
          title = "Lab Result Distribution",
          subtitle = paste0("N = ", comma(nrow(df))),
          x = NULL,
          y = "Cases"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold", size = 16),
          plot.subtitle = element_text(color = "grey30"),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 10, hjust = 1)
        )
    })

    # Lab Result Pie Chart
    output$lab_result_pie <- renderPlot({
      req(lab_data())
      df <- lab_data()

      if (nrow(df) == 0) {
        return(ggplot() +
                 annotate("text", x = 0, y = 0, label = "No lab result data available", size = 6) +
                 theme_void())
      }

      tab <- df %>%
        count(lab_result_clean, name = "cases") %>%
        mutate(p = cases / sum(cases))

      # Use same theme colors as bar chart - intuitive healthcare colors
      theme_colors <- get_superset_colors(12)
      pal <- c(
        "Positive" = "#E74C3C",             # Red (danger) - positive disease result
        "Negative" = "#4EB69D",             # Green (success) - negative disease result
        "Indeterminate" = theme_colors[5],  # Light blue - uncertain
        "Invalid" = theme_colors[11],       # Darker teal - error
        "Missing / Not recorded" = "grey70" # Grey for missing
      )

      ggplot(tab, aes(x = "", y = p, fill = lab_result_clean)) +
        geom_col(color = "white", width = 1) +
        coord_polar(theta = "y") +
        geom_text(aes(label = paste0(percent(p, 0.1), "\n", comma(cases))),
                  position = position_stack(vjust = 0.5),
                  color = "white", size = 4, lineheight = 0.95, fontface = "bold") +
        scale_fill_manual(values = pal, name = NULL) +
        labs(
          title = "Lab Result Distribution",
          subtitle = "Labels show percent and count",
          x = NULL,
          y = NULL
        ) +
        theme_void(base_size = 12) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          legend.position = "bottom"
        )
    })

    # Lab Result Table
    output$lab_result_table <- renderTable({
      req(lab_data())
      df <- lab_data()

      df %>%
        count(lab_result_clean, name = "Cases") %>%
        mutate(
          Percentage = percent(Cases / sum(Cases), 0.1)
        ) %>%
        rename(`Lab Result` = lab_result_clean) %>%
        arrange(desc(Cases))
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

  })
}
