age_analysis_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      card(
        class = "border-0 shadow-sm",
        card_header(
          class = "bg-transparent border-0",
          "Age Distribution by Sex"
        ),
        card_body(
          plotOutput(ns("violin_plot"), height = "500px")
        )
      ),
      col_widths = 12
    ),

    layout_columns(
      card(
        class = "border-0 shadow-sm",
        card_header(
          class = "bg-transparent border-0",
          "Age Group Distribution"
        ),
        card_body(
          plotOutput(ns("age_group_plot"), height = "450px")
        )
      ),
      card(
        class = "border-0 shadow-sm",
        card_header(
          class = "bg-transparent border-0",
          "Detailed Statistics"
        ),
        card_body(
          tableOutput(ns("age_stats_table"))
        )
      ),
      col_widths = c(7, 5)
    )
  )
}

#' Age Analysis Server
#' @param id Module namespace ID
#' @param filtered_data Reactive data frame
age_analysis_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    output$violin_plot <- renderPlot({
      plot_data <- filtered_data() %>%
        filter(!is.na(sex), !is.na(case_age), case_age >= 0, case_age <= 110)

      if (nrow(plot_data) == 0) {
        return(ggplot() + theme_void() +
                 annotate("text", x = 0.5, y = 0.5,
                         label = "No data available", size = 6))
      }

      pal <- get_color_palette()

      ggplot(plot_data, aes(x = sex, y = case_age, fill = sex)) +
        geom_violin(
          trim = FALSE,
          alpha = 0.6,
          color = NA,
          scale = "width",
          width = 0.8
        ) +
        geom_boxplot(
          width = 0.15,
          alpha = 0.9,
          outlier.size = 1.5,
          outlier.alpha = 0.5,
          color = "#333333",
          fill = "#FFFFFF"
        ) +
        stat_summary(
          fun = median,
          geom = "point",
          size = 3,
          color = "#1A1A1A",
          shape = 18
        ) +
        scale_fill_manual(
          values = c("Male" = pal["Male"], "Female" = pal["Female"])
        ) +
        scale_y_continuous(
          breaks = seq(0, 100, 10),
          limits = c(0, 85)
        ) +
        labs(
          title = "Age Distribution Comparison by Sex",
          subtitle = paste0("N = ", comma(nrow(plot_data)),
                          " | Showing ages 0-85 years"),
          x = NULL,
          y = "Age (years)",
          caption = "Distribution shape (violin) with quartiles (box) and median (diamond)"
        ) +
        theme_modern(base_size = 14) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(size = 14, face = "bold"),
          panel.grid.major.x = element_blank()
        )
    })

    output$age_group_plot <- renderPlot({
      age_data <- filtered_data() %>%
        filter(!is.na(age_group)) %>%
        count(age_group, name = "n") %>%
        mutate(
          pct = 100 * n / sum(n),
          label = paste0(comma(n), "\n(", round(pct, 1), "%)")
        )

      if (nrow(age_data) == 0) {
        return(ggplot() + theme_void() +
                 annotate("text", x = 0.5, y = 0.5,
                         label = "No data available", size = 6))
      }

      pal <- get_color_palette()

      ggplot(age_data, aes(x = age_group, y = n)) +
        geom_col(
          fill = pal["primary"],
          alpha = 0.9,
          width = 0.75
        ) +
        geom_text(
          aes(label = comma(n)),
          vjust = -0.5,
          size = 4,
          fontface = "bold",
          color = "#333333"
        ) +
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.15)),
          labels = comma
        ) +
        labs(
          title = "Case Distribution by Age Group",
          subtitle = "Counts with percentage of total",
          x = "Age Group (years)",
          y = "Number of Cases",
          caption = NULL
        ) +
        theme_modern(base_size = 13) +
        theme(
          axis.text.x = element_text(angle = 0, hjust = 0.5, size = 11),
          panel.grid.major.x = element_blank()
        )
    })

    output$age_stats_table <- renderTable({
      filtered_data() %>%
        filter(!is.na(sex), !is.na(case_age)) %>%
        group_by(Sex = sex) %>%
        summarise(
          N = comma(n()),
          Mean = sprintf("%.1f", mean(case_age, na.rm = TRUE)),
          Median = sprintf("%.1f", median(case_age, na.rm = TRUE)),
          SD = sprintf("%.1f", sd(case_age, na.rm = TRUE)),
          Min = sprintf("%.0f", min(case_age, na.rm = TRUE)),
          Max = sprintf("%.0f", max(case_age, na.rm = TRUE)),
          `IQR` = sprintf("%.1f", IQR(case_age, na.rm = TRUE)),
          .groups = "drop"
        )
    }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%",
    align = "lrrrrrrr")
  })
}
