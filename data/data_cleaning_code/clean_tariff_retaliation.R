# 0. load relevant packages
library(readr)
library(dplyr)
library(stringr)
install.packages("remotes")
remotes::install_github("insongkim/concordance")
library(concordance)
library(scales)
library(ggplot2)
library(forcats)

# 1. set wd to be main - change as needed 
# setwd("/Users/himalbamzai-wokhlu/Desktop/JBW_Wins/retaliatory-tariff")

# 2. load in cleaned data on tariff edxposure by industry 
tariff_naics <- read_csv("./data/county_exposure/tariff_by_industry/derived/summarized_trade_retaliations.csv") %>%
  rename(
    hs8    = `Foreign National Tariff Line`,
    tariff = `Current Total Estimated Retaliatory Tariff`,
    desc   = `Foreign Tariff Line Description`
  ) %>%
  mutate(
    hs8 = str_pad(hs8, width = 8, side = "left", pad = "0"),
    hs6 = str_sub(hs8, 1, 6),
    naics6 = concord_hs_naics(
      sourcevar = hs6,
      origin = "HS",
      destination = "NAICS",
      dest.digit = 6,
      all = FALSE
    ),
    naics2 = str_sub(naics6, 1, 2),
    naics3 = str_sub(naics6, 1, 3),
    naics4 = str_sub(naics6, 1, 4)
  )

# 94% match rate 
write.csv(tariff_naics, "./data/county_exposure/tariff_by_industry/derived/trade_retaliations_by_naics.csv")

# 0) Output directory
out_dir <- "./output/tariff_by_industry/figures"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# 1) NAICS 2-digit labels (standard sector names)
naics2_labels <- tibble::tribble(
  ~naics2, ~naics2_name,
  "11", "Agriculture, Forestry, Fishing and Hunting",
  "21", "Mining, Quarrying, and Oil and Gas Extraction",
  "22", "Utilities",
  "23", "Construction",
  "31", "Manufacturing",
  "32", "Manufacturing",
  "33", "Manufacturing",
  "42", "Wholesale Trade",
  "44", "Retail Trade",
  "45", "Retail Trade",
  "48", "Transportation and Warehousing",
  "49", "Transportation and Warehousing",
  "51", "Information",
  "52", "Finance and Insurance",
  "53", "Real Estate and Rental and Leasing",
  "54", "Professional, Scientific, and Technical Services",
  "55", "Management of Companies and Enterprises",
  "56", "Administrative and Support and Waste Management",
  "61", "Educational Services",
  "62", "Health Care and Social Assistance",
  "71", "Arts, Entertainment, and Recreation",
  "72", "Accommodation and Food Services",
  "81", "Other Services (except Public Administration)",
  "91", "Public Administration",
  "92", "Public Administration",
  "99", "Unclassified Establishments"
)

# 2) Prep data: parse tariff, attach labels, then collapse 31/32/33 -> 31-33
tariff_plot <- tariff_naics %>%
  filter(!is.na(naics2)) %>%
  mutate(
    etr = readr::parse_number(tariff) / 100
  ) %>%
  left_join(naics2_labels, by = "naics2") %>%
  mutate(
    naics2_name = if_else(is.na(naics2_name), "Unknown / not mapped", naics2_name),
    
    # Collapse manufacturing
    naics2c = if_else(naics2 %in% c("31", "32", "33"), "31-33", naics2),
    naics2c_name = if_else(naics2 %in% c("31", "32", "33"), "Manufacturing", naics2_name),
    naics2c_label = if_else(naics2 %in% c("31", "32", "33"),
                            "31–33 — Manufacturing",
                            paste0(naics2, " — ", naics2_name))
  )

# 3) Summary stats by collapsed NAICS2
summary_naics2c <- tariff_plot %>%
  group_by(naics2c, naics2c_name, naics2c_label) %>%
  summarize(
    n_lines = n(),
    mean_etr = mean(etr, na.rm = TRUE),
    p90_etr  = quantile(etr, 0.9, na.rm = TRUE),
    max_etr  = max(etr, na.rm = TRUE),
    share_ge25 = mean(etr >= 0.25, na.rm = TRUE),
    .groups = "drop"
  )

# 4) A simple, consistent theme
theme_tariff <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title.y = element_text(margin = margin(r = 8)),
    axis.title.x = element_text(margin = margin(t = 8)),
    panel.grid.minor = element_blank()
  )

# Helper for saving
save_fig <- function(p, filename, w = 10, h = 6, dpi = 320) {
  ggsave(file.path(out_dir, filename), p, width = w, height = h, dpi = dpi)
}

# ----------------------------
# FIGURE 1: Incidence (count of tariff lines) by collapsed NAICS2
# ----------------------------
fig1 <- summary_naics2c %>%
  arrange(desc(n_lines)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = fct_reorder(naics2c_label, n_lines), y = n_lines, fill = naics2c)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Number of tariff lines",
    title = "Tariff-line incidence by NAICS sector (2-digit; Manufacturing collapsed)",
    subtitle = "Counts of HS lines mapped to each sector; manufacturing shown as 31–33"
  ) +
  theme_tariff

save_fig(fig1, "fig1_count_tariff_lines_by_naics2_collapsed_mfg.png")

# ----------------------------
# FIGURE 2: Mean ETR by collapsed NAICS2 (top by count)
# ----------------------------
fig2 <- summary_naics2c %>%
  arrange(desc(n_lines)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = fct_reorder(naics2c_label, mean_etr), y = mean_etr, fill = naics2c)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = NULL,
    y = "Mean retaliatory tariff rate",
    title = "Average retaliatory tariff rate by NAICS sector (Manufacturing collapsed)",
    subtitle = "Mean across tariff lines within each sector (top sectors by line count)"
  ) +
  theme_tariff

save_fig(fig2, "fig2_mean_etr_by_naics2_collapsed_mfg.png")

# ----------------------------
# FIGURE 3: 90th percentile ETR by collapsed NAICS2 (robust severity)
# ----------------------------
fig3 <- summary_naics2c %>%
  arrange(desc(p90_etr)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = fct_reorder(naics2c_label, p90_etr), y = p90_etr, fill = naics2c)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = NULL,
    y = "90th percentile retaliatory tariff rate",
    title = "Tariff severity by sector: 90th percentile ETR (Manufacturing collapsed)",
    subtitle = "High-but-not-maximum tariff rate within each sector (top by p90)"
  ) +
  theme_tariff

save_fig(fig3, "fig3_p90_etr_by_naics2_collapsed_mfg.png")

# ----------------------------
# FIGURE 4: Maximum ETR by collapsed NAICS2
# ----------------------------
fig4 <- summary_naics2c %>%
  arrange(desc(max_etr)) %>%
  ggplot(aes(x = fct_reorder(naics2c_label, max_etr), y = max_etr, fill = naics2c)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = NULL,
    y = "Maximum retaliatory tariff rate",
    title = "Highest retaliatory tariff rate by NAICS sector (Manufacturing collapsed)",
    subtitle = "Each bar is the maximum tariff line within the sector"
  ) +
  theme_tariff

save_fig(fig4, "fig4_max_etr_by_naics2_collapsed_mfg.png")

# ----------------------------
# FIGURE 5: Share of lines with ETR >= 25% by collapsed NAICS2
# ----------------------------
fig5 <- summary_naics2c %>%
  arrange(desc(share_ge25)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = fct_reorder(naics2c_label, share_ge25), y = share_ge25, fill = naics2c)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = NULL,
    y = "Share of tariff lines with ETR ≥ 25%",
    title = "Incidence of high retaliatory tariffs by sector (Manufacturing collapsed)",
    subtitle = "Within each sector: fraction of tariff lines with ETR at least 25% (top sectors)"
  ) +
  theme_tariff

save_fig(fig5, "fig5_share_lines_ge25_by_naics2_collapsed_mfg.png")

# ----------------------------
# FIGURE 6: Distribution of ETR by collapsed NAICS2 (boxplot; top by count)
# ----------------------------
top_sectors <- summary_naics2c %>%
  arrange(desc(n_lines)) %>%
  slice_head(n = 12) %>%
  pull(naics2c)

fig6 <- tariff_plot %>%
  filter(naics2c %in% top_sectors) %>%
  ggplot(aes(x = fct_reorder(naics2c_label, etr, .fun = median), y = etr, fill = naics2c)) +
  geom_boxplot(outlier.alpha = 0.25, show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = NULL,
    y = "Retaliatory tariff rate",
    title = "Distribution of retaliatory tariff rates by sector (Manufacturing collapsed)",
    subtitle = "Boxplots for the 12 sectors with the most tariff lines"
  ) +
  theme_tariff

save_fig(fig6, "fig6_etr_distribution_boxplot_top_naics2_collapsed_mfg.png", h = 7)

message("Saved figures to: ", out_dir)




