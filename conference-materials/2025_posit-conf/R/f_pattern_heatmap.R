library(ggplot2)
library(dplyr)

# Create a grid representing the dashboard layout
# Higher values = more attention/fixation
grid_size <- 20
dashboard_grid <- expand.grid(
  x = 1:grid_size,
  y = 1:grid_size
)

# Define F-pattern attention weights
# F-pattern: High attention on top row, left column, and middle horizontal
dashboard_grid$attention <- 0

# Top horizontal bar (executive summary area) - highest attention
dashboard_grid$attention[dashboard_grid$y >= 17] <-
  100 - (20 - dashboard_grid$y[dashboard_grid$y >= 17]) * 5

# Left vertical bar (navigation/filters) - high attention
dashboard_grid$attention[dashboard_grid$x <= 4] <-
  dashboard_grid$attention[dashboard_grid$x <= 4] +
  (80 - dashboard_grid$y[dashboard_grid$x <= 4] * 3)

# Middle horizontal bar (main content area) - moderate attention
dashboard_grid$attention[dashboard_grid$y >= 10 & dashboard_grid$y <= 13] <-
  dashboard_grid$attention[dashboard_grid$y >= 10 & dashboard_grid$y <= 13] +
  (60 -
    abs(
      dashboard_grid$x[dashboard_grid$y >= 10 & dashboard_grid$y <= 13] - 10
    ) *
      2)

# Add some natural decay and noise
dashboard_grid$attention <- dashboard_grid$attention *
  (1 - (dashboard_grid$x + dashboard_grid$y) / 60) +
  rnorm(nrow(dashboard_grid), 0, 5)

dashboard_grid$attention[dashboard_grid$attention < 0] <- 0
dashboard_grid$attention[dashboard_grid$attention > 100] <- 100

# Create the F-pattern heatmap
f_pattern_plot <- ggplot(dashboard_grid, aes(x = x, y = y, fill = attention)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#f0f0f0",
    mid = "#ffeb3b",
    high = "#f44336",
    midpoint = 50,
    limits = c(0, 100),
    name = "Visual\nAttention"
  ) +

  # Overlay dashboard elements
  # Executive Summary (top)
  annotate(
    "rect",
    xmin = 2,
    xmax = 19,
    ymin = 17,
    ymax = 19,
    fill = NA,
    color = "black",
    size = 1.5,
    linetype = "solid"
  ) +
  annotate(
    "text",
    x = 10.5,
    y = 18,
    label = "Executive Summary",
    size = 5,
    fontface = "bold"
  ) +

  # Three filters (below summary)
  annotate(
    "rect",
    xmin = 2,
    xmax = 7,
    ymin = 14.5,
    ymax = 16,
    fill = NA,
    color = "black",
    size = 1
  ) +
  annotate("text", x = 4.5, y = 15.25, label = "Product", size = 3.5) +

  annotate(
    "rect",
    xmin = 7.5,
    xmax = 12.5,
    ymin = 14.5,
    ymax = 16,
    fill = NA,
    color = "black",
    size = 1
  ) +
  annotate("text", x = 10, y = 15.25, label = "Region", size = 3.5) +

  annotate(
    "rect",
    xmin = 13,
    xmax = 19,
    ymin = 14.5,
    ymax = 16,
    fill = NA,
    color = "black",
    size = 1
  ) +
  annotate("text", x = 16, y = 15.25, label = "Metric", size = 3.5) +

  # Main visualizations (middle)
  annotate(
    "rect",
    xmin = 2,
    xmax = 10,
    ymin = 8,
    ymax = 13,
    fill = NA,
    color = "black",
    size = 1
  ) +
  annotate("text", x = 6, y = 10.5, label = "Revenue Chart", size = 4) +

  annotate(
    "rect",
    xmin = 11,
    xmax = 19,
    ymin = 8,
    ymax = 13,
    fill = NA,
    color = "black",
    size = 1
  ) +
  annotate("text", x = 15, y = 10.5, label = "Satisfaction Chart", size = 4) +

  # Details (bottom - progressive disclosure)
  annotate(
    "rect",
    xmin = 2,
    xmax = 19,
    ymin = 2,
    ymax = 6,
    fill = NA,
    color = "gray",
    size = 0.5,
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = 10.5,
    y = 4,
    label = "Details (Hidden by Default)",
    size = 3,
    color = "gray",
    fontface = "italic"
  ) +

  # Add F-pattern arrows
  annotate(
    "segment",
    x = 2,
    xend = 19,
    y = 20.5,
    yend = 20.5,
    arrow = arrow(length = unit(0.3, "cm")),
    size = 1.5,
    color = "#d32f2f"
  ) +
  annotate(
    "segment",
    x = 1,
    xend = 1,
    y = 19,
    yend = 2,
    arrow = arrow(length = unit(0.3, "cm")),
    size = 1.5,
    color = "#d32f2f"
  ) +
  annotate(
    "segment",
    x = 2,
    xend = 15,
    y = 11,
    yend = 11,
    arrow = arrow(length = unit(0.3, "cm")),
    size = 1,
    color = "#ff9800"
  ) +

  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "F-Pattern Reading Path Applied to Dashboard Design",
    subtitle = "Users scan top (executive summary), down left (filters), then across (main content)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "#666"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right"
  )

# Save the plot
ggsave(
  "conference-materials/2025_posit-conf/img/f-pattern-heatmap.png",
  f_pattern_plot,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

print(f_pattern_plot)
