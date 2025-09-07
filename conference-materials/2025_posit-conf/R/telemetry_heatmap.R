library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

set.seed(123)

# Define all 18 filters from the original dashboard
filters <- c(
  # Global filters (top 3 - used by 82%)
  "Year",
  "Quarter",
  "Region",

  # Revenue tab filters
  "Product (Rev)",
  "Category",
  "Channel (Rev)",

  # Customer tab filters
  "Segment",
  "Source",
  "Campaign (Cust)",

  # Operations tab filters
  "Team",
  "Priority",
  "Status",

  # Product tab filters
  "Product (Prod)",
  "Compare With",

  # Marketing tab filters
  "Campaign (Mkt)",
  "Channel (Mkt)",

  # Executive tab filters
  "Metric",
  "View Type"
)

# Generate realistic usage data for 1000 sessions
n_sessions <- 1000
session_data <- data.frame()

for (i in 1:n_sessions) {
  # 82% of sessions use only the first 3 filters
  if (i <= 820) {
    # These users interact heavily with top 3 filters
    interactions <- data.frame(
      session_id = i,
      filter = sample(filters[1:3], sample(5:15, 1), replace = TRUE),
      interaction_count = rpois(length(filter), lambda = 3)
    )
  } else {
    # 18% explore other filters (but still use top 3 more)
    n_interactions <- sample(10:25, 1)

    # Mix of top filters and some exploration
    filter_choices <- c(
      sample(filters[1:3], round(n_interactions * 0.6), replace = TRUE),
      sample(filters[4:18], round(n_interactions * 0.4), replace = TRUE)
    )

    interactions <- data.frame(
      session_id = i,
      filter = filter_choices,
      interaction_count = c(
        rpois(round(n_interactions * 0.6), lambda = 3), # More interactions with top 3
        rpois(round(n_interactions * 0.4), lambda = 0.8) # Fewer with others
      )
    )
  }

  session_data <- rbind(session_data, interactions)
}

# Aggregate interaction counts by filter
heatmap_data <- session_data |>
  group_by(filter) |>
  summarize(
    total_interactions = sum(interaction_count),
    sessions_used = n_distinct(session_id),
    avg_interactions_per_session = total_interactions / n_sessions
  ) |>
  mutate(
    usage_rate = sessions_used / n_sessions * 100,
    filter = factor(filter, levels = filters) # Maintain order
  )

# Create the main heatmap
heatmap_plot <- ggplot(heatmap_data, aes(x = 1, y = filter)) +
  geom_tile(aes(fill = usage_rate), color = "white", size = 0.5) +
  geom_text(
    aes(label = sprintf("%.0f%%", usage_rate)),
    color = ifelse(heatmap_data$usage_rate > 50, "white", "black"),
    size = 4,
    fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#f0f0f0",
    mid = "#ff9800",
    high = "#d32f2f",
    midpoint = 50,
    limits = c(0, 100),
    name = "Usage Rate (%)"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title = "Filter Usage Heatmap - 1000 User Sessions",
    subtitle = "82% of users interact with only 3 of 18 available filters",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#666"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 11),
    axis.ticks = element_blank(),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# Add annotations
heatmap_plot <- heatmap_plot +
  annotate(
    "segment",
    x = 0.5,
    xend = 1.5,
    y = 3.5,
    yend = 3.5,
    color = "#d32f2f",
    size = 1,
    linetype = "dashed"
  ) +
  annotate(
    "text",
    x = 1.7,
    y = 2,
    label = "← 82% use only these",
    hjust = 0,
    size = 4,
    color = "#d32f2f",
    fontface = "bold"
  ) +
  annotate(
    "segment",
    x = 1.6,
    xend = 1.55,
    y = 2,
    yend = 2.5,
    color = "#d32f2f",
    size = 0.5,
    arrow = arrow(length = unit(0.2, "cm"), type = "closed")
  ) +
  annotate(
    "text",
    x = 1.7,
    y = 11,
    label = "← Rarely or never used",
    hjust = 0,
    size = 3.5,
    color = "#666",
    fontface = "italic"
  )

# Save the plot
ggsave(
  "conference-materials/2025_posit-conf/img/telemetry-heatmap.png",
  heatmap_plot,
  width = 10,
  height = 8,
  dpi = 300,
  bg = "white"
)

print(heatmap_plot)

# Also create a more detailed session-level heatmap if desired
# This shows individual session patterns
session_matrix <- session_data |>
  group_by(session_id, filter) |>
  summarize(interactions = sum(interaction_count), .groups = "drop") |>
  complete(session_id, filter, fill = list(interactions = 0)) |>
  filter(session_id <= 50) |> # Show first 50 sessions for clarity
  mutate(filter = factor(filter, levels = filters))

session_heatmap <- ggplot(session_matrix, aes(x = session_id, y = filter)) +
  geom_tile(aes(fill = interactions), color = "white", size = 0.1) +
  scale_fill_gradient(
    low = "white",
    high = "#d32f2f",
    name = "Clicks",
    breaks = c(0, 2, 4, 6, 8)
  ) +
  scale_x_continuous(breaks = seq(10, 50, 10), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title = "Individual Session Patterns (First 50 Sessions)",
    subtitle = "Most sessions show concentrated activity on top 3 filters",
    x = "Session ID",
    y = "Filter"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10, color = "#666"),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "right",
    panel.grid = element_blank()
  )

# Save the session-level heatmap
ggsave(
  "conference-materials/2025_posit-conf/img/telemetry-sessions-heatmap.png",
  session_heatmap,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

print(session_heatmap)

# Print summary statistics
cat("\n=== TELEMETRY SUMMARY ===\n")
cat("Total sessions analyzed:", n_sessions, "\n")
cat("Total filters available:", length(filters), "\n\n")

cat("Top 3 most used filters:\n")
top_3 <- heatmap_data |>
  arrange(desc(usage_rate)) |>
  head(3)
for (i in 1:3) {
  cat(sprintf(
    "  %d. %s: %.1f%% of sessions\n",
    i,
    top_3$filter[i],
    top_3$usage_rate[i]
  ))
}

cat("\nBottom 5 least used filters:\n")
bottom_5 <- heatmap_data |>
  arrange(usage_rate) |>
  head(5)
for (i in 1:5) {
  cat(sprintf(
    "  %s: %.1f%% of sessions\n",
    bottom_5$filter[i],
    bottom_5$usage_rate[i]
  ))
}

cat(
  "\n✓ Heatmaps saved to img/telemetry-heatmap.png and img/telemetry-sessions-heatmap.png\n"
)
