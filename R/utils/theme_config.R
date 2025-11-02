get_color_palette <- function() {
  c(
    # Superset "Preset + Superset" color scheme
    "Male" = "#1F4E79",        # Dark blue (primary)
    "Female" = "#5DA5C9",      # Medium blue
    "Unknown" = "#A2D4E8",     # Light blue

    # Full Superset palette (12 colors)
    "color1" = "#1F4E79",      # Dark navy blue
    "color2" = "#3F7BA8",      # Medium dark blue
    "color3" = "#5DA5C9",      # Medium blue
    "color4" = "#7BC8E2",      # Light medium blue
    "color5" = "#9AD9EC",      # Light blue
    "color6" = "#ADE3F1",      # Very light blue
    "color7" = "#B8E8E8",      # Pale cyan
    "color8" = "#7FD4C1",      # Mint green
    "color9" = "#6BC4A6",      # Teal green
    "color10" = "#4EB69D",     # Dark teal
    "color11" = "#3A9D84",     # Darker teal
    "color12" = "#1F7A6D",     # Deep teal

    # Theme colors
    "primary" = "#1F4E79",     # Dark navy from Superset
    "secondary" = "#5DA5C9",   # Medium blue
    "accent" = "#7FD4C1",      # Mint green
    "background" = "#FFFFFF",  # White background
    "text" = "#2B3C4E"         # Dark gray-blue text
  )
}

get_app_theme <- function() {
  pal <- get_color_palette()
  bs_theme(
    version = 5,
    preset = "shiny",
    bg = "#FFFFFF",
    fg = "#2B3C4E",
    primary = pal["primary"],      # Dark navy blue
    secondary = pal["secondary"],  # Medium blue
    success = "#4EB69D",           # Dark teal from palette
    info = pal["color3"],          # Medium blue
    warning = "#FFB84D",           # Warm orange
    danger = "#E74C3C",            # Red (complementary)
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
      values = c("Male" = pal["Male"], "Female" = pal["Female"], "Unknown" = pal["Unknown"]),
      name = "Sex"
    )
  }
}

scale_fill_sex <- function(discrete = TRUE) {
  pal <- get_color_palette()
  if (discrete) {
    scale_fill_manual(
      values = c("Male" = pal["Male"], "Female" = pal["Female"], "Unknown" = pal["Unknown"]),
      name = "Sex"
    )
  }
}

scale_fill_continuous_modern <- function() {
  scale_fill_gradient2(
    low = "#ADE3F1",
    mid = "#5DA5C9",
    high = "#1F4E79",
    midpoint = NULL,
    na.value = "#E5E5E5"
  )
}

# Get Superset sequential colors for multiple categories
get_superset_colors <- function(n) {
  pal <- get_color_palette()
  colors <- c(
    pal["color1"],   # #1F4E79 - Dark navy
    pal["color2"],   # #3F7BA8 - Medium dark blue
    pal["color3"],   # #5DA5C9 - Medium blue
    pal["color4"],   # #7BC8E2 - Light medium blue
    pal["color5"],   # #9AD9EC - Light blue
    pal["color6"],   # #ADE3F1 - Very light blue
    pal["color7"],   # #B8E8E8 - Pale cyan
    pal["color8"],   # #7FD4C1 - Mint green
    pal["color9"],   # #6BC4A6 - Teal green
    pal["color10"],  # #4EB69D - Dark teal
    pal["color11"],  # #3A9D84 - Darker teal
    pal["color12"]   # #1F7A6D - Deep teal
  )

  if (n <= 12) {
    return(colors[1:n])
  } else {
    # If more colors needed, interpolate
    return(colorRampPalette(colors)(n))
  }
}
