get_color_palette <- function() {
  c(
    "Female" = "#E63946",      # Modern vibrant red
    "Male" = "#457B9D",        # Modern slate blue
    "primary" = "#1D3557",     # Deep navy
    "secondary" = "#A8DADC",   # Light cyan
    "accent" = "#F1FAEE",      # Off-white
    "background" = "#0A1929",  # Deep blue-black
    "text" = "#E8F0F2"         # Light gray-blue
  )
}

get_app_theme <- function() {
  bs_theme(
    version = 5,
    preset = "shiny",
    bg = "#0A1929",
    fg = "#E8F0F2",
    primary = "#457B9D",
    secondary = "#A8DADC",
    success = "#06D6A0",
    info = "#118AB2",
    warning = "#FFD166",
    danger = "#E63946",
    base_font = font_google("Inter"),
    heading_font = font_google("Outfit"),
    code_font = font_google("JetBrains Mono")
  )
}

theme_modern <- function(base_size = 13) {
  pal <- get_color_palette()

  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.grid.major = element_line(color = "#E5E5E5", linewidth = 0.3),
      panel.grid.minor = element_blank(),

      plot.title = element_text(
        size = base_size * 1.3,
        face = "bold",
        color = "#1A1A1A",
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = base_size * 0.9,
        color = "#666666",
        margin = margin(b = 15)
      ),
      plot.caption = element_text(
        size = base_size * 0.8,
        color = "#999999",
        hjust = 0,
        margin = margin(t = 10)
      ),
      axis.title = element_text(
        size = base_size * 0.95,
        color = "#333333",
        face = "bold"
      ),
      axis.text = element_text(
        size = base_size * 0.85,
        color = "#555555"
      ),

      legend.background = element_rect(fill = "#FFFFFF", color = NA),
      legend.key = element_rect(fill = "#FFFFFF", color = NA),
      legend.title = element_text(
        size = base_size * 0.9,
        face = "bold",
        color = "#333333"
      ),
      legend.text = element_text(
        size = base_size * 0.85,
        color = "#555555"
      ),
      legend.position = "bottom",

      strip.background = element_rect(fill = "#F5F5F5", color = NA),
      strip.text = element_text(
        size = base_size * 0.9,
        face = "bold",
        color = "#333333",
        margin = margin(5, 5, 5, 5)
      ),

      plot.margin = margin(15, 15, 15, 15)
    )
}

apply_modern_theme <- function() {
  theme_set(theme_modern())
}

scale_color_sex <- function(discrete = TRUE) {
  pal <- get_color_palette()
  if (discrete) {
    scale_color_manual(
      values = c("Male" = pal["Male"], "Female" = pal["Female"]),
      name = "Sex"
    )
  }
}

scale_fill_sex <- function(discrete = TRUE) {
  pal <- get_color_palette()
  if (discrete) {
    scale_fill_manual(
      values = c("Male" = pal["Male"], "Female" = pal["Female"]),
      name = "Sex"
    )
  }
}

scale_fill_continuous_modern <- function() {
  scale_fill_gradient2(
    low = "#F1FAEE",
    mid = "#457B9D",
    high = "#1D3557",
    midpoint = NULL,
    na.value = "#E5E5E5"
  )
}
