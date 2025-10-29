geographic_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      card(
        class = "border-0 shadow-sm",
        full_screen = TRUE,
        card_header(
          class = "bg-transparent border-0",
          "Geographic Distribution - DRC Provinces"
        ),
        card_body(
          plotOutput(ns("province_map"), height = "600px")
        )
      ),
      col_widths = 12
    ),

    layout_columns(
      card(
        class = "border-0 shadow-sm",
        card_header(
          class = "bg-transparent border-0",
          "Top Provinces by Case Count"
        ),
        card_body(
          plotOutput(ns("province_bar"), height = "500px")
        )
      ),
      card(
        class = "border-0 shadow-sm",
        card_header(
          class = "bg-transparent border-0",
          "Provincial Summary"
        ),
        card_body(
          tableOutput(ns("province_table"))
        )
      ),
      col_widths = c(7, 5)
    )
  )
}

geographic_server <- function(id, filtered_data, drc_sf) {
  moduleServer(id, function(input, output, session) {

    output$province_map <- renderPlot({
      tryCatch({
        if (is.null(drc_sf)) {
          return(
            ggplot() +
              theme_void() +
              annotate("text", x = 0.5, y = 0.5,
                      label = "Map data not available\nInstall: sf, geodata, terra",
                      size = 6, color = "#666666")
          )
        }

        normalize <- function(x) {
          x %>%
            stringi::stri_trans_general("Latin-ASCII") %>%
            str_to_lower() %>%
            str_replace_all("[^a-z0-9]+", " ") %>%
            str_squish()
        }

        prov_counts <- filtered_data() %>%
          filter(!is.na(province), province != "") %>%
          count(province, name = "cases") %>%
          mutate(province_norm = normalize(province))

        if (nrow(prov_counts) == 0) {
          return(
            ggplot() +
              theme_void() +
              annotate("text", x = 0.5, y = 0.5,
                      label = "No province data available with current filters",
                      size = 6, color = "#666666")
          )
        }

        map_sf <- drc_sf %>%
          left_join(prov_counts, by = c("province_gadm_norm" = "province_norm"))

        ggplot(map_sf) +
          geom_sf(aes(fill = cases), color = "white", size = 0.4) +
          scale_fill_gradient(
            low = "#E8F4F8",
            high = "#1D3557",
            na.value = "#F5F5F5",
            labels = comma,
            name = "Cases",
            trans = "sqrt"
          ) +
          labs(
            title = "Mpox Cases by Province",
            subtitle = "Democratic Republic of Congo",
            caption = "Data: Case surveillance | Boundaries: GADM"
          ) +
          theme_void(base_size = 13) +
          theme(
            plot.title = element_text(face = "bold", hjust = 0.5,
                                     size = 18, color = "#1A1A1A",
                                     margin = margin(b = 5)),
            plot.subtitle = element_text(hjust = 0.5, size = 13,
                                        color = "#666666",
                                        margin = margin(b = 15)),
            plot.caption = element_text(color = "#999999", size = 10,
                                       hjust = 0.5, margin = margin(t = 10)),
            legend.position = "right",
            legend.title = element_text(face = "bold", size = 11),
            plot.background = element_rect(fill = "#FFFFFF", color = NA),
            plot.margin = margin(15, 15, 15, 15)
          )
      }, error = function(e) {
        ggplot() +
          theme_void() +
          annotate("text", x = 0.5, y = 0.5,
                  label = paste0("Map error:\n", e$message, "\n\nTry refreshing or check filters"),
                  size = 5, color = "#E63946", hjust = 0.5)
      })
    })

    output$province_bar <- renderPlot({
      prov_data <- filtered_data() %>%
        filter(!is.na(province), province != "") %>%
        count(province, name = "cases") %>%
        arrange(desc(cases)) %>%
        slice_head(n = 15) %>%
        mutate(province = fct_reorder(province, cases))

      if (nrow(prov_data) == 0) {
        return(ggplot() + theme_void() +
                 annotate("text", x = 0.5, y = 0.5,
                         label = "No data available", size = 6))
      }

      pal <- get_color_palette()

      ggplot(prov_data, aes(x = province, y = cases)) +
        geom_col(
          fill = pal["primary"],
          alpha = 0.9,
          width = 0.8
        ) +
        geom_text(
          aes(label = comma(cases)),
          hjust = -0.2,
          size = 4,
          fontface = "bold",
          color = "#333333"
        ) +
        coord_flip(clip = "off") +
        scale_y_continuous(
          expand = expansion(mult = c(0, 0.15)),
          labels = comma
        ) +
        labs(
          title = "Top 15 Provinces by Case Count",
          subtitle = "Ranked by total cases",
          x = NULL,
          y = "Number of Cases"
        ) +
        theme_modern(base_size = 13) +
        theme(
          axis.text.y = element_text(size = 11),
          panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(color = "#E5E5E5", linewidth = 0.3)
        )
    })

    output$province_table <- renderTable({
      filtered_data() %>%
        filter(!is.na(province), province != "") %>%
        count(province, name = "Cases") %>%
        arrange(desc(Cases)) %>%
        mutate(
          Percent = paste0(round(100 * Cases / sum(Cases), 1), "%"),
          Cases = comma(Cases)
        ) %>%
        head(15)
    }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")
  })
}
