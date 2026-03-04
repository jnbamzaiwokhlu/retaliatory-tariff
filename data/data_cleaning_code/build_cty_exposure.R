# =============================================================================
# build_cty_exposure.R
# Retaliatory tariff exposure by county
# =============================================================================

# 0. Load packages -------------------------------------------------------------
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)

# Install concordance if needed
if (!requireNamespace("concordance", quietly = TRUE)) {
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("insongkim/concordance")
}
library(concordance)

# 1. Set working directory (edit path as needed) --------------------------------
# setwd("/Users/himalbamzai-wokhlu/Desktop/JBW_Wins/retaliatory-tariff")
# setwd("C:/Users/jubil/OneDrive/Desktop/retaliatory-tariff")

# 2. Create output directories -------------------------------------------------
dir.create("./output/tariff_by_industry/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/tariff_by_industry/qa",      recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# SECTION A: Build tariff × NAICS crosswalk
# ==============================================================================

# A1. Read raw tariff data and map to NAICS ------------------------------------
tariff_naics <- read_csv(
  "./data/county_exposure/tariff_by_industry/derived/summarized_trade_retaliations.csv",
  show_col_types = FALSE
) %>%
  rename(
    hs8    = `Foreign National Tariff Line`,
    tariff = `Current Total Estimated Retaliatory Tariff`,
    desc   = `Foreign Tariff Line Description`
  ) %>%
  mutate(
    hs8 = str_pad(str_replace_all(as.character(hs8), "[^0-9]", ""), 8, pad = "0"),
    hs6 = str_sub(hs8, 1, 6),
    naics6 = concord_hs_naics(
      sourcevar    = hs6,
      origin       = "HS",
      destination  = "NAICS",
      dest.digit   = 6,
      all          = FALSE
    ),
    naics2 = str_sub(naics6, 1, 2),
    naics3 = str_sub(naics6, 1, 3),
    naics4 = str_sub(naics6, 1, 4)
  )

# A2. Save crosswalk -----------------------------------------------------------
write_csv(tariff_naics,
          "./data/county_exposure/tariff_by_industry/derived/trade_retaliations_by_naics.csv")

message(sprintf(
  "Match rate: %.1f%%",
  100 * mean(!is.na(tariff_naics$naics6))
))

# ==============================================================================
# SECTION B: QA — flag and inspect problem rows
# ==============================================================================

# B1. Read back the saved crosswalk and add QA flags --------------------------
#     (We re-read so this section can be run on its own if needed)
x <- read_csv(
  "./data/county_exposure/tariff_by_industry/derived/trade_retaliations_by_naics.csv",
  show_col_types = FALSE
) %>%
  mutate(
    # Clean up NAICS codes that may have come back as e.g. "112920.0" or "NA"
    naics6_chr = na_if(str_replace(as.character(naics6), "\\.0$", ""), "NA"),
    naics2_chr = na_if(str_replace(as.character(naics2), "\\.0$", ""), "NA"),
    naics2_chr = if_else(!is.na(naics2_chr),
                         str_pad(naics2_chr, 2, pad = "0"),
                         naics2_chr),
    
    # QA flags
    unmapped       = is.na(naics6_chr),
    bad_naics6     = !is.na(naics6_chr) & !str_detect(naics6_chr, "^[0-9]{6}$"),
    naics2_mismatch = !is.na(naics6_chr) & !is.na(naics2_chr) &
      str_sub(naics6_chr, 1, 2) != naics2_chr,
    public_admin   = naics2_chr %in% c("91", "92"),
    hs97           = str_starts(hs6, "97"),   # art / antiques chapter — not traded industrially
    hs000          = str_starts(hs6, "000")   # placeholder / unknown HS
  )

# B2. Summarise flag counts ----------------------------------------------------
problem_rows <- x %>%
  filter(unmapped | bad_naics6 | naics2_mismatch | public_admin | hs97 | hs000)

flag_summary <- problem_rows %>%
  transmute(unmapped, bad_naics6, naics2_mismatch, public_admin, hs97, hs000) %>%
  pivot_longer(everything(), names_to = "issue", values_to = "flag") %>%
  filter(flag) %>%
  count(issue, sort = TRUE)

message("\n--- QA flag summary ---")
print(flag_summary, n = Inf)

# B3. Optional: drill into unmapped rows by country / HS chapter ---------------
message("\nUnmapped rows by country:")
print(x %>% filter(unmapped) %>% count(Country, sort = TRUE), n = Inf)

message("\nUnmapped rows by HS chapter (first 2 digits):")
print(x %>% filter(unmapped) %>%
        mutate(hs_chapter = str_sub(hs6, 1, 2)) %>%
        count(hs_chapter, sort = TRUE), n = Inf)

# B4. Write QA output ----------------------------------------------------------
problem_rows %>%
  select(hs8, hs6, naics6 = naics6_chr, naics2 = naics2_chr,
         tariff, desc, Country,
         unmapped, bad_naics6, naics2_mismatch, public_admin, hs97, hs000) %>%
  write_csv("./output/tariff_by_industry/qa/problem_naics_rows.csv")

message("QA file saved to ./output/tariff_by_industry/qa/problem_naics_rows.csv")

# B5. Clean dataset: drop all problem rows before downstream use ---------------
tariff_naics_clean <- x %>%
  filter(!unmapped, !bad_naics6, !naics2_mismatch, !public_admin, !hs97, !hs000) %>%
  # Use the cleaned string versions going forward
  mutate(
    naics6 = naics6_chr,
    naics2 = naics2_chr
  ) %>%
  select(-naics6_chr, -naics2_chr,
         -unmapped, -bad_naics6, -naics2_mismatch, -public_admin, -hs97, -hs000)

message(sprintf(
  "Rows after QA cleaning: %d (dropped %d)",
  nrow(tariff_naics_clean),
  nrow(x) - nrow(tariff_naics_clean)
))

# ==============================================================================
# SECTION C: Figures — tariff exposure by NAICS sector
# ==============================================================================

out_dir <- "./output/tariff_by_industry/figures"

# C1. NAICS 2-digit labels -----------------------------------------------------
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

# C2. Prep plot data -----------------------------------------------------------
tariff_plot <- tariff_naics_clean %>%           # ← uses clean data now
  mutate(etr = parse_number(tariff) / 100) %>%
  left_join(naics2_labels, by = "naics2") %>%
  mutate(
    naics2_name   = if_else(is.na(naics2_name), "Unknown / not mapped", naics2_name),
    naics2c       = if_else(naics2 %in% c("31", "32", "33"), "31-33", naics2),
    naics2c_name  = if_else(naics2 %in% c("31", "32", "33"), "Manufacturing", naics2_name),
    naics2c_label = if_else(naics2 %in% c("31", "32", "33"),
                            "31\u201333 \u2014 Manufacturing",
                            paste0(naics2, " \u2014 ", naics2_name))
  )

# C3. Summary stats ------------------------------------------------------------
summary_naics2c <- tariff_plot %>%
  group_by(naics2c, naics2c_name, naics2c_label) %>%
  summarize(
    n_lines    = n(),
    mean_etr   = mean(etr, na.rm = TRUE),
    p90_etr    = quantile(etr, 0.9, na.rm = TRUE),
    max_etr    = max(etr, na.rm = TRUE),
    share_ge25 = mean(etr >= 0.25, na.rm = TRUE),
    .groups = "drop"
  )

# C4. Shared theme + save helper -----------------------------------------------
theme_tariff <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    axis.title.y  = element_text(margin = margin(r = 8)),
    axis.title.x  = element_text(margin = margin(t = 8)),
    panel.grid.minor = element_blank()
  )

save_fig <- function(p, filename, w = 10, h = 6, dpi = 320) {
  ggsave(file.path(out_dir, filename), p, width = w, height = h, dpi = dpi)
}

# C5. Figure 1 — line count by sector -----------------------------------------
fig1 <- summary_naics2c %>%
  arrange(desc(n_lines)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(fct_reorder(naics2c_label, n_lines), n_lines, fill = naics2c)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = NULL, y = "Number of tariff lines",
       title = "Tariff-line incidence by NAICS sector",
       subtitle = "Manufacturing collapsed to 31\u201333") +
  theme_tariff

save_fig(fig1, "fig1_count_tariff_lines_by_naics2.png")

# C6. Figure 2 — mean ETR ------------------------------------------------------
fig2 <- summary_naics2c %>%
  arrange(desc(n_lines)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(fct_reorder(naics2c_label, mean_etr), mean_etr, fill = naics2c)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Mean retaliatory tariff rate",
       title = "Average retaliatory tariff rate by NAICS sector",
       subtitle = "Top sectors by line count; manufacturing collapsed") +
  theme_tariff

save_fig(fig2, "fig2_mean_etr_by_naics2.png")

# C7. Figure 3 — 90th pct ETR --------------------------------------------------
fig3 <- summary_naics2c %>%
  arrange(desc(p90_etr)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(fct_reorder(naics2c_label, p90_etr), p90_etr, fill = naics2c)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "90th percentile ETR",
       title = "Tariff severity by sector: 90th percentile ETR") +
  theme_tariff

save_fig(fig3, "fig3_p90_etr_by_naics2.png")

# C8. Figure 4 — max ETR -------------------------------------------------------
fig4 <- summary_naics2c %>%
  arrange(desc(max_etr)) %>%
  ggplot(aes(fct_reorder(naics2c_label, max_etr), max_etr, fill = naics2c)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Maximum retaliatory tariff rate",
       title = "Highest retaliatory tariff rate by NAICS sector") +
  theme_tariff

save_fig(fig4, "fig4_max_etr_by_naics2.png")

# C9. Figure 5 — share of lines >= 25% ----------------------------------------
fig5 <- summary_naics2c %>%
  arrange(desc(share_ge25)) %>%
  slice_head(n = 15) %>%
  ggplot(aes(fct_reorder(naics2c_label, share_ge25), share_ge25, fill = naics2c)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Share of lines with ETR \u2265 25%",
       title = "Incidence of high retaliatory tariffs by sector") +
  theme_tariff

save_fig(fig5, "fig5_share_lines_ge25_by_naics2.png")

# C10. Figure 6 — boxplot distribution -----------------------------------------
top_sectors <- summary_naics2c %>%
  arrange(desc(n_lines)) %>%
  slice_head(n = 12) %>%
  pull(naics2c)

fig6 <- tariff_plot %>%
  filter(naics2c %in% top_sectors) %>%
  ggplot(aes(fct_reorder(naics2c_label, etr, .fun = median), etr, fill = naics2c)) +
  geom_boxplot(outlier.alpha = 0.25, show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = NULL, y = "Retaliatory tariff rate",
       title = "Distribution of retaliatory tariff rates by sector",
       subtitle = "Top 12 sectors by line count") +
  theme_tariff

save_fig(fig6, "fig6_etr_distribution_boxplot_top_naics2.png", h = 7)

message("All figures saved to: ", out_dir)

# ==============================================================================
# SECTION D: County-level export exposure
# ==============================================================================
# The county_industry_panel_full.csv file already has county x naics2 export
# exposure pre-allocated by employment share. We just need to attach ETR rates
# from the tariff crosswalk and collapse to county level.

# D1. Load panel ---------------------------------------------------------------
panel <- read_csv(
  "./data/county_exposure/employment_by_county/derived/county_industry_panel_full.csv",
  show_col_types = FALSE
) %>%
  mutate(
    county_fips = str_pad(as.character(county_fips), 5, pad = "0"),
    naics2      = as.character(naics2),
    export_value    = as.numeric(export_value),
    export_exposure = as.numeric(export_exposure)
  )

# D2. Build ETR lookup at naics2 level -----------------------------------------
# Note: tariff file has 31/32/33 for manufacturing; panel collapses all to "31"
# So we average ETR across 31+32+33 and map that to panel naics2 "31"
tariff_xwalk <- read_csv(
  "./data/county_exposure/tariff_by_industry/derived/trade_retaliations_by_naics.csv",
  show_col_types = FALSE
) %>%
  mutate(
    etr    = readr::parse_number(tariff) / 100,
    naics2 = str_pad(str_replace(as.character(naics2), "\\.0$", ""), 2, pad = "0"),
    # Collapse 32/33 into 31 to match panel
    naics2 = if_else(naics2 %in% c("32", "33"), "31", naics2)
  ) %>%
  filter(!is.na(etr), !naics2 %in% c("NA", "91", "92"))  # drop unmapped + public admin

etr_by_naics2 <- tariff_xwalk %>%
  group_by(naics2) %>%
  summarise(mean_etr = mean(etr, na.rm = TRUE), .groups = "drop")

message("ETR by naics2:")
print(etr_by_naics2)

# D3. Join and compute tariff-weighted exposure --------------------------------
panel_exposure <- panel %>%
  left_join(etr_by_naics2, by = "naics2") %>%
  mutate(
    mean_etr                = replace_na(mean_etr, 0),
    tariff_weighted_exports = export_exposure * mean_etr
  )

# D4. Collapse to county level -------------------------------------------------
county_exposure_out <- panel_exposure %>%
  group_by(county_fips, county_name) %>%
  summarise(
    total_export_exposure   = sum(export_exposure,        na.rm = TRUE),
    total_tariff_weighted   = sum(tariff_weighted_exports, na.rm = TRUE),
    effective_exposure_rate = total_tariff_weighted / pmax(total_export_exposure, 1),
    .groups = "drop"
  )

message(sprintf("Counties in exposure output:     %d", n_distinct(county_exposure_out$county_fips)))
message(sprintf("Counties with non-zero exposure: %d", sum(county_exposure_out$effective_exposure_rate > 0)))

write_csv(county_exposure_out, "./data/county_exposure/county_exposure_summary.csv")
write_csv(panel_exposure,      "./data/county_exposure/industry_exports.csv")

message("Saved county_exposure_summary.csv and industry_exports.csv")