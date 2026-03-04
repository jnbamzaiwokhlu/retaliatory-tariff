# =============================================================================
# analyze_descriptive.R
# Descriptive analysis: retaliatory tariff exposure × partisan lean
# =============================================================================

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggrepel)   # install.packages("ggrepel") -- non-overlapping labels
library(scales)
library(readxl)

dir.create("./output/descriptive", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# SECTION A: Load and merge data
# ==============================================================================

# A1. County exposure summary (built by build_cty_exposure.R) ------------------
exposure <- read_csv(
  "./data/county_exposure/county_exposure_summary.csv",
  show_col_types = FALSE
) %>%
  mutate(county_fips = str_pad(as.character(county_fips), 5, pad = "0")) %>%
  # Percentile rank exposure (robust to scale/outlier issues in raw values)
  mutate(
    exposure_pctile = percent_rank(effective_exposure_rate) * 100
  )

# A2. Voting data --------------------------------------------------------------
votes <- read_excel("./data/votes/derived/voting_by_county_real.xlsx") %>%
  mutate(
    county_fips = str_pad(as.character(combined_fips), 5, pad = "0"),
    state_fips  = str_sub(county_fips, 1, 2),
    # Clean county name: strip state suffix if present e.g. "Autauga County, AL"
    county_label = str_remove(county_name, ",.*$")
  )

# A3. Merge --------------------------------------------------------------------
df <- votes %>%
  inner_join(exposure, by = "county_fips") %>%
  filter(!is.na(per_gop_24), !is.na(exposure_pctile))

message(sprintf("Analysis sample: %d counties", nrow(df)))

# A4. GOP vote share quartiles -------------------------------------------------
df <- df %>%
  mutate(
    gop_quartile = ntile(per_gop_24, 4),
    gop_quartile_label = case_when(
      gop_quartile == 1 ~ "Q1: Most Dem\n(0–25th pctile)",
      gop_quartile == 2 ~ "Q2: Lean Dem\n(25–50th pctile)",
      gop_quartile == 3 ~ "Q3: Lean GOP\n(50–75th pctile)",
      gop_quartile == 4 ~ "Q4: Most GOP\n(75–100th pctile)"
    ) %>% factor(levels = c(
      "Q1: Most Dem\n(0–25th pctile)", "Q2: Lean Dem\n(25–50th pctile)",
      "Q3: Lean GOP\n(50–75th pctile)", "Q4: Most GOP\n(75–100th pctile)"
    ))
  )

# ==============================================================================
# SECTION B: Identify label candidates
# ==============================================================================
# For each GOP quartile, label the top 3 counties by exposure percentile

labels_df <- df %>%
  group_by(gop_quartile) %>%
  slice_max(order_by = exposure_pctile, n = 3) %>%
  ungroup() %>%
  mutate(label = paste0(county_label, "\n(", state_abbr, ")"))

# ==============================================================================
# SECTION C: Plots
# ==============================================================================

theme_clean <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    plot.caption     = element_text(size = 8,  color = "grey50"),
    panel.grid.minor = element_blank(),
    legend.position  = "none"
  )

# Color scale: blue -> purple -> red
partisan_gradient <- scale_color_gradient2(
  low      = "#2166ac",
  mid      = "#9970ab",
  high     = "#d6604d",
  midpoint = 0.5,
  guide    = "none"
)

# --- FIGURE 1: Scatter with labeled outliers ----------------------------------
fig1 <- ggplot(df, aes(x = per_gop_24, y = exposure_pctile, color = per_gop_24)) +
  # All counties as faint dots
  geom_point(alpha = 0.25, size = 1.2) +
  # OLS trend line
  geom_smooth(method = "lm", se = TRUE, color = "black",
              linewidth = 0.9, alpha = 0.15) +
  # Label top counties per quartile
  geom_point(data = labels_df, size = 2.5, alpha = 0.9) +
  geom_label_repel(
    data        = labels_df,
    aes(label   = label),
    size        = 2.8,
    color       = "black",
    fill        = alpha("white", 0.85),
    box.padding = 0.4,
    point.padding = 0.3,
    segment.color = "grey60",
    segment.size  = 0.4,
    max.overlaps  = 20,
    force         = 3
  ) +
  partisan_gradient +
  scale_x_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0.1, 0.9, by = 0.2),
    name   = "GOP vote share, 2024"
  ) +
  scale_y_continuous(
    name   = "Tariff exposure index (percentile)",
    breaks = seq(0, 100, by = 25),
    labels = function(x) paste0(x, "th")
  ) +
  labs(
    title    = "Retaliatory tariff exposure vs. GOP vote share, by county",
    subtitle = "Labeled counties are top 3 by exposure within each GOP vote-share quartile",
    caption  = "Exposure = effective retaliatory tariff rate on county export base, ranked by percentile.\nVoting data: 2024 presidential election. Tariff data: current retaliatory schedules."
  ) +
  theme_clean

ggsave("./output/descriptive/fig1_scatter_exposure_vs_gop.png",
       fig1, width = 11, height = 7, dpi = 320)

# --- FIGURE 2: Boxplot of exposure by GOP quartile ----------------------------
# Mean exposure per quartile for annotation
quartile_means <- df %>%
  group_by(gop_quartile_label) %>%
  summarise(mean_pctile = mean(exposure_pctile), .groups = "drop")

fig2 <- ggplot(df, aes(x = gop_quartile_label, y = exposure_pctile,
                       fill = gop_quartile_label)) +
  geom_boxplot(outlier.alpha = 0.2, width = 0.55, show.legend = FALSE) +
  geom_text(
    data = quartile_means,
    aes(x = gop_quartile_label, y = mean_pctile, label = sprintf("mean = %.0fth", mean_pctile)),
    nudge_x  = 0.38,
    size     = 3.2,
    color    = "grey30",
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = c(
    "Q1: Most Dem\n(0–25th pctile)"  = "#2166ac",
    "Q2: Lean Dem\n(25–50th pctile)" = "#74add1",
    "Q3: Lean GOP\n(50–75th pctile)" = "#f4a582",
    "Q4: Most GOP\n(75–100th pctile)"= "#d6604d"
  )) +
  scale_y_continuous(
    name   = "Tariff exposure index (percentile)",
    breaks = seq(0, 100, by = 25),
    labels = function(x) paste0(x, "th")
  ) +
  labs(
    x        = NULL,
    title    = "Distribution of tariff exposure by GOP vote-share quartile",
    subtitle = "Each box shows the spread of county exposure percentiles within the quartile",
    caption  = "Quartiles defined by 2024 GOP presidential vote share."
  ) +
  theme_clean +
  theme(panel.grid.major.x = element_blank())

ggsave("./output/descriptive/fig2_boxplot_exposure_by_gop_quartile.png",
       fig2, width = 10, height = 6, dpi = 320)

# --- FIGURE 3: Same scatter, faceted by GOP quartile -------------------------
# Makes it easier to see within-quartile outliers
fig3 <- ggplot(df, aes(x = per_gop_24, y = exposure_pctile, color = per_gop_24)) +
  geom_point(alpha = 0.3, size = 1.0) +
  geom_smooth(method = "lm", se = TRUE, color = "black",
              linewidth = 0.8, alpha = 0.15) +
  geom_label_repel(
    data          = labels_df,
    aes(label     = label),
    size          = 2.6,
    color         = "black",
    fill          = alpha("white", 0.85),
    box.padding   = 0.35,
    point.padding = 0.2,
    segment.color = "grey60",
    segment.size  = 0.4,
    max.overlaps  = 20
  ) +
  geom_point(data = labels_df, size = 2.5, alpha = 1) +
  facet_wrap(~ gop_quartile_label, scales = "free_x", nrow = 1) +
  partisan_gradient +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     name   = "GOP vote share, 2024") +
  scale_y_continuous(
    name   = "Tariff exposure index (percentile)",
    breaks = seq(0, 100, by = 25),
    labels = function(x) paste0(x, "th")
  ) +
  labs(
    title    = "Tariff exposure vs. GOP vote share, by quartile",
    subtitle = "Labeled counties are the 3 most-exposed within each quartile",
    caption  = "Exposure percentile ranked nationally across all counties."
  ) +
  theme_clean +
  theme(strip.text = element_text(face = "bold", size = 9))

ggsave("./output/descriptive/fig3_faceted_by_gop_quartile.png",
       fig3, width = 14, height = 6, dpi = 320)

# ==============================================================================
# SECTION D: Summary table
# ==============================================================================

summary_table <- df %>%
  group_by(gop_quartile_label) %>%
  summarise(
    n_counties      = n(),
    gop_share_range = sprintf("%.0f%%–%.0f%%",
                              100 * min(per_gop_24), 100 * max(per_gop_24)),
    mean_exposure_pctile = round(mean(exposure_pctile), 1),
    median_exposure_pctile = round(median(exposure_pctile), 1),
    pct_above_75th = round(100 * mean(exposure_pctile >= 75), 1),
    .groups = "drop"
  )

print(summary_table)
write_csv(summary_table, "./output/descriptive/summary_table_by_gop_quartile.csv")

message("\nAll outputs saved to ./output/descriptive/")
