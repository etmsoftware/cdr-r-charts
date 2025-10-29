overview_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
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
      ),
      col_widths = c(4, 4, 4)
    ),

    layout_columns(
      card(
        class = "border-0 shadow-sm",
        card_header(
          class = "bg-transparent border-0",
          "Age-Sex Distribution Pyramid"
        ),
        card_body(
          plotOutput(ns("pyramid_plot"), height = "550px")
        )
      ),
      card(
        class = "border-0 shadow-sm",
        card_header(
          class = "bg-transparent border-0",
          "Summary Statistics"
        ),
        card_body(
          tableOutput(ns("summary_table"))
        )
      ),
      col_widths = c(8, 4)
    )
  )
}

overview_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    output$total_cases <- renderText({
      comma(nrow(filtered_data()))
    })

    output$male_cases <- renderText({
      comma(sum(filtered_data()$sex == "Male", na.rm = TRUE))
    })

    output$female_cases <- renderText({
      comma(sum(filtered_data()$sex == "Female", na.rm = TRUE))
    })

    output$summary_table <- renderTable({
      filtered_data() %>%
        filter(!is.na(sex)) %>%
        group_by(sex) %>%
        summarise(
          Cases = comma(n()),
          `Median Age` = sprintf("%.1f", median(case_age, na.rm = TRUE)),
          `Mean Age` = sprintf("%.1f", mean(case_age, na.rm = TRUE)),
          `Std Dev` = sprintf("%.1f", sd(case_age, na.rm = TRUE)),
          .groups = "drop"
        )
    }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

    output$pyramid_plot <- renderPlot({
      pyr_data <- filtered_data() %>%
        filter(!is.na(age_group), !is.na(sex)) %>%
        count(age_group, sex, name = "n") %>%
        mutate(n_mirror = ifelse(sex == "Male", -n, n))

      if (nrow(pyr_data) == 0) {
        return(ggplot() + theme_void() +
                 annotate("text", x = 0.5, y = 0.5,
                         label = "No data available", size = 6))
      }

      lim <- max(abs(pyr_data$n_mirror), na.rm = TRUE)
      pal <- get_color_palette()

      ggplot(pyr_data, aes(x = age_group, y = n_mirror, fill = sex)) +
        geom_col(width = 0.85, alpha = 0.9) +
        geom_text(
          aes(label = comma(abs(n_mirror))),
          color = "#FFFFFF",
          size = 4,
          fontface = "bold",
          position = position_stack(vjust = 0.5)
        ) +
        coord_flip() +
        scale_y_continuous(
          limits = c(-lim * 1.15, lim * 1.15),
          labels = function(x) comma(abs(x)),
          expand = expansion(mult = c(0.02, 0.02))
        ) +
        scale_fill_manual(
          values = c("Male" = pal["Male"], "Female" = pal["Female"])
        ) +
        labs(
          title = "Population Distribution by Age and Sex",
          subtitle = paste0("Total Cases: ", comma(sum(pyr_data$n))),
          x = "Age Group",
          y = "Number of Cases",
          fill = NULL,
          caption = "Data: Mpox case surveillance, DRC"
        ) +
        theme_modern(base_size = 14) +
        theme(
          legend.position = "top",
          legend.justification = "center",
          axis.text.y = element_text(size = 12, face = "bold"),
          panel.grid.major.y = element_blank()
        )
    })
  })
}
