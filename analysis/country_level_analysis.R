# =============================================================================
# analyze_by_country.R
# Retaliatory tariff patterns by imposing country
# =============================================================================

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)

dir.create("./output/by_country", recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# SECTION A: Load and prep data
# ==============================================================================

df <- read_csv(
  "./data/county_exposure/tariff_by_industry/derived/trade_retaliations_by_naics.csv",
  show_col_types = FALSE
) %>%
  mutate(
    etr    = parse_number(tariff) / 100,
    naics2 = str_pad(str_replace(as.character(naics2), "\\.0$", ""), 2, pad = "0"),
    naics2 = if_else(naics2 %in% c("32", "33"), "31", naics2),  # collapse manufacturing
    # Flag high-tariff lines
    high_tariff = etr >= 0.25
  ) %>%
  filter(!is.na(etr), !naics2 %in% c("NA", "91", "92"))

# NAICS2 readable labels (manufacturing collapsed)
naics2_labels <- c(
  "11" = "Agriculture",
  "21" = "Mining & Oil/Gas",
  "22" = "Utilities",
  "23" = "Construction",
  "31" = "Manufacturing",
  "42" = "Wholesale Trade",
  "44" = "Retail Trade",
  "48" = "Transportation",
  "51" = "Information",
  "52" = "Finance & Insurance",
  "54" = "Professional Services",
  "99" = "Unclassified"
)

df <- df %>%
  mutate(sector = recode(naics2, !!!naics2_labels, .default = "Other"))

# Country-level summary
country_summary <- df %>%
  group_by(Country) %>%
  summarise(
    n_lines       = n(),
    n_hs_chapters = n_distinct(str_sub(hs6, 1, 2)),
    mean_etr      = mean(etr, na.rm = TRUE),
    median_etr    = median(etr, na.rm = TRUE),
    max_etr       = max(etr, na.rm = TRUE),
    pct_high      = mean(high_tariff, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_lines))

print(country_summary)

# Shared theme
theme_clean <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    plot.caption     = element_text(size = 8,  color = "grey50"),
    panel.grid.minor = element_blank(),
    legend.position  = "none"
  )

# Country color palette
country_colors <- c(
  "China"  = "#d6604d",
  "Canada" = "#4393c3",
  "Russia" = "#762a83",
  "Turkey" = "#1b7837",
  "India"  = "#e08214"
)

save_fig <- function(p, name, w = 11, h = 6.5, dpi = 320) {
  ggsave(file.path("./output/by_country", name), p, width = w, height = h, dpi = dpi)
  message("Saved: ", name)
}

# ==============================================================================
# FIGURE 1: Overview — breadth vs. severity bubble chart
# Each country is one bubble: x = # tariff lines, y = mean ETR,
# size = # HS chapters covered, color = country
# ==============================================================================

fig1 <- country_summary %>%
  ggplot(aes(x = n_lines, y = mean_etr,
             size = n_hs_chapters, color = Country, label = Country)) +
  geom_point(alpha = 0.85) +
  geom_text(
    nudge_y = 0.012, size = 3.8, fontface = "bold", show.legend = FALSE
  ) +
  scale_size_continuous(range = c(4, 18), name = "HS chapters covered") +
  scale_color_manual(values = country_colors) +
  scale_x_continuous(labels = comma, name = "Number of tariff lines") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     name   = "Mean retaliatory tariff rate") +
  labs(
    title   = "Retaliatory tariff regimes: breadth vs. severity by country",
    subtitle = "Bubble size = number of distinct HS chapters targeted",
    caption  = "China targets the most product lines; Canada imposes the highest average rates."
  ) +
  theme_clean +
  theme(legend.position = "right",
        legend.title    = element_text(size = 9))

save_fig(fig1, "fig1_breadth_vs_severity_bubble.png", h = 6)

# ==============================================================================
# FIGURE 2: ETR distribution by country — violin + boxplot overlay
# Shows the full shape of the rate distribution per country
# ==============================================================================

fig2 <- df %>%
  mutate(Country = fct_reorder(Country, etr, .fun = mean, .desc = TRUE)) %>%
  ggplot(aes(x = Country, y = etr, fill = Country, color = Country)) +
  geom_violin(alpha = 0.3, linewidth = 0.4, trim = TRUE) +
  geom_boxplot(width = 0.15, outlier.alpha = 0.15,
               fill = "white", linewidth = 0.6) +
  scale_fill_manual(values  = country_colors) +
  scale_color_manual(values = country_colors) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     name   = "Retaliatory tariff rate",
                     breaks = seq(0, 1, by = 0.1)) +
  labs(
    x        = NULL,
    title    = "Distribution of retaliatory tariff rates by country",
    subtitle = "Violin shows full distribution; inner box is IQR; countries ordered by mean rate",
    caption  = "Canada's distribution is compressed at high rates; China's is more spread out."
  ) +
  theme_clean

save_fig(fig2, "fig2_etr_distribution_by_country.png")

# ==============================================================================
# FIGURE 3: Sector targeting heatmap
# Rows = sectors, columns = countries, fill = share of lines in that sector
# Reveals which countries targeted which industries
# ==============================================================================

sector_heatmap <- df %>%
  group_by(Country, sector) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Country) %>%
  mutate(share = n / sum(n)) %>%
  ungroup() %>%
  # Keep sectors that are >1% of any country's lines
  group_by(sector) %>%
  filter(max(share) > 0.01) %>%
  ungroup() %>%
  mutate(
    sector  = fct_reorder(sector, share, .fun = max, .desc = FALSE),
    Country = fct_reorder(Country, n, .fun = sum, .desc = TRUE)
  )

fig3 <- sector_heatmap %>%
  ggplot(aes(x = Country, y = sector, fill = share)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = if_else(share >= 0.02, percent(share, accuracy = 1), "")),
            size = 3, color = "white", fontface = "bold") +
  scale_fill_gradient(
    low    = "#f7f7f7",
    high   = "#2166ac",
    labels = percent_format(accuracy = 1),
    name   = "Share of\ntariff lines"
  ) +
  labs(
    x        = NULL,
    y        = NULL,
    title    = "Which industries did each country target?",
    subtitle = "Cell = share of that country's tariff lines falling in each NAICS sector",
    caption  = "Russia concentrated almost entirely on manufacturing; Canada spread across agriculture and manufacturing."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    plot.caption     = element_text(size = 8,  color = "grey50"),
    panel.grid       = element_blank(),
    axis.text.x      = element_text(face = "bold", size = 11),
    axis.text.y      = element_text(size = 10),
    legend.position  = "right"
  )

save_fig(fig3, "fig3_sector_targeting_heatmap.png", h = 6)

# ==============================================================================
# FIGURE 4: Mean ETR by sector × country — grouped bar chart
# Focus on China vs Canada (the two main players) for clarity
# ==============================================================================

sector_etr <- df %>%
  filter(Country %in% c("China", "Canada")) %>%
  group_by(Country, sector) %>%
  summarise(
    mean_etr = mean(etr, na.rm = TRUE),
    n        = n(),
    .groups  = "drop"
  ) %>%
  filter(n >= 5) %>%   # only sectors with meaningful coverage
  mutate(sector = fct_reorder(sector, mean_etr, .fun = max))

fig4 <- sector_etr %>%
  ggplot(aes(x = sector, y = mean_etr, fill = Country)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.9) +
  geom_text(
    aes(label = percent(mean_etr, accuracy = 1)),
    position = position_dodge(width = 0.7),
    hjust = -0.15, size = 2.8
  ) +
  coord_flip() +
  scale_fill_manual(values = c("China" = "#d6604d", "Canada" = "#4393c3")) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.18)),
    name   = "Mean retaliatory tariff rate"
  ) +
  labs(
    x        = NULL,
    fill     = NULL,
    title    = "Mean tariff rate by sector: China vs. Canada",
    subtitle = "Sectors with at least 5 tariff lines per country; ordered by maximum rate across countries",
    caption  = "Canada applies uniformly high rates across all sectors it targets."
  ) +
  theme_clean +
  theme(legend.position = "top",
        legend.text     = element_text(size = 11))

save_fig(fig4, "fig4_mean_etr_sector_china_vs_canada.png", h = 6)

message("\nAll figures saved to ./output/by_country/")
message("  fig1: Breadth vs. severity bubble chart")
message("  fig2: ETR distribution violins by country")
message("  fig3: Sector targeting heatmap")
message("  fig4: Mean ETR by sector, China vs Canada")