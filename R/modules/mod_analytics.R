analytics_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      card(
        class = "border-0 shadow-sm",
        card_header(
          class = "bg-transparent border-0",
          "Age Distribution Density"
        ),
        card_body(
          plotOutput(ns("density_plot"), height = "450px")
        )
      ),
      card(
        class = "border-0 shadow-sm",
        card_header(
          class = "bg-transparent border-0",
          "Comparative Box Plots"
        ),
        card_body(
          plotOutput(ns("box_plot"), height = "450px")
        )
      ),
      col_widths = c(6, 6)
    ),

    layout_columns(
      card(
        class = "border-0 shadow-sm",
        full_screen = TRUE,
        card_header(
          class = "bg-transparent border-0",
          "Case Data Explorer"
        ),
        card_body(
          downloadButton(ns("download_data"), "Export CSV",
                        class = "btn-sm btn-primary mb-3"),
          DT::dataTableOutput(ns("detailed_table"))
        )
      ),
      col_widths = 12
    )
  )
}

analytics_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {

    # Modern Density Plot
    output$density_plot <- renderPlot({
      plot_data <- filtered_data() %>%
        filter(!is.na(sex), !is.na(case_age), case_age >= 0, case_age <= 100)

      if (nrow(plot_data) == 0) {
        return(ggplot() + theme_void() +
                 annotate("text", x = 0.5, y = 0.5,
                         label = "No data available", size = 6))
      }

      pal <- get_color_palette()

      ggplot(plot_data, aes(x = case_age, fill = sex, color = sex)) +
        geom_density(alpha = 0.4, linewidth = 1.2) +
        geom_rug(aes(color = sex), alpha = 0.3, length = unit(0.03, "npc")) +
        scale_fill_manual(
          values = c("Male" = pal["Male"], "Female" = pal["Female"])
        ) +
        scale_color_manual(
          values = c("Male" = pal["Male"], "Female" = pal["Female"])
        ) +
        scale_x_continuous(breaks = seq(0, 100, 10)) +
        labs(
          title = "Age Distribution Density Curves",
          subtitle = "Comparing age patterns between sexes",
          x = "Age (years)",
          y = "Density",
          fill = "Sex",
          color = "Sex",
          caption = "Rug marks show individual case ages"
        ) +
        theme_modern(base_size = 13) +
        theme(
          legend.position = "top",
          panel.grid.minor.x = element_blank()
        )
    })

    output$box_plot <- renderPlot({
      plot_data <- filtered_data() %>%
        filter(!is.na(sex), !is.na(case_age), case_age >= 0, case_age <= 100)

      if (nrow(plot_data) == 0) {
        return(ggplot() + theme_void() +
                 annotate("text", x = 0.5, y = 0.5,
                         label = "No data available", size = 6))
      }

      pal <- get_color_palette()

      stats_data <- plot_data %>%
        group_by(sex) %>%
        summarise(
          median = median(case_age, na.rm = TRUE),
          .groups = "drop"
        )

      ggplot(plot_data, aes(x = sex, y = case_age, fill = sex)) +
        geom_boxplot(
          width = 0.5,
          alpha = 0.8,
          outlier.size = 2,
          outlier.alpha = 0.4,
          notch = TRUE,
          notchwidth = 0.5
        ) +
        geom_jitter(
          aes(color = sex),
          width = 0.15,
          alpha = 0.15,
          size = 1.5,
          show.legend = FALSE
        ) +
        stat_summary(
          fun = mean,
          geom = "point",
          size = 4,
          color = "#1A1A1A",
          shape = 23,
          fill = "#FFFFFF"
        ) +
        scale_fill_manual(
          values = c("Male" = pal["Male"], "Female" = pal["Female"])
        ) +
        scale_color_manual(
          values = c("Male" = pal["Male"], "Female" = pal["Female"])
        ) +
        scale_y_continuous(breaks = seq(0, 100, 10)) +
        labs(
          title = "Age Comparison with Distribution",
          subtitle = "Notched boxplots show median confidence intervals",
          x = NULL,
          y = "Age (years)",
          caption = "Diamond = mean | Box = IQR | Points = individual cases"
        ) +
        theme_modern(base_size = 13) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(size = 13, face = "bold"),
          panel.grid.major.x = element_blank()
        )
    })

    output$detailed_table <- DT::renderDataTable({
      filtered_data() %>%
        select(Province = province, Sex = sex,
               Age = case_age, `Age Group` = age_group) %>%
        head(1000)
    },
    options = list(
      pageLength = 25,
      scrollX = TRUE,
      scrollY = "500px",
      scrollCollapse = TRUE,
      searching = TRUE,
      ordering = TRUE,
      info = TRUE,
      autoWidth = TRUE,
      columnDefs = list(
        list(className = 'dt-center', targets = 1:3),
        list(width = '30%', targets = 0),
        list(width = '20%', targets = 1:2),
        list(width = '30%', targets = 3)
      ),
      language = list(
        search = "Filter:",
        lengthMenu = "Show _MENU_ entries",
        info = "Showing _START_ to _END_ of _TOTAL_ cases",
        paginate = list(
          first = "First",
          last = "Last",
          `next` = "Next",
          previous = "Previous"
        )
      )
    ),
    class = 'cell-border stripe hover',
    rownames = FALSE,
    filter = 'top',
    selection = 'none'
    )

    output$download_data <- downloadHandler(
      filename = function() {
        paste0("mpox_filtered_data_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write_csv(filtered_data(), file)
      }
    )
  })
}
