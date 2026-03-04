# =============================================================================
# analyze_exposure_partisan.R
# Retaliatory tariff exposure × partisan lean, by county
# =============================================================================

# 0. Packages ------------------------------------------------------------------
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)
library(readxl)
library(fixest)      # fast OLS with fixed effects: install.packages("fixest")
library(tidycensus)  # ACS data via API:            install.packages("tidycensus")

# 1. Directories ---------------------------------------------------------------
dir.create("./output/exposure_partisan/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("./output/exposure_partisan/tables",  recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# SECTION A: Pull ACS controls via tidycensus
# ==============================================================================
# First-time setup: you need a free Census API key.
# 1. Get one at: https://api.census.gov/data/key_signup.html  (instant, free)
# 2. Run this ONCE to save it: census_api_key("YOUR_KEY_HERE", install = TRUE)
# 3. After that, tidycensus picks it up automatically every session.

# Variables we want from ACS 5-year 2022 (county level)
# Full variable list: View(load_variables(2022, "acs5"))
acs_vars <- c(
  pop_total      = "B01003_001",   # total population
  median_hhinc   = "B19013_001",   # median household income
  pop_25plus     = "B15003_001",   # population 25+ (denominator for education)
  bach_plus      = "B15003_022",   # bachelor's degree (we'll add 023-025 for grad)
  masters        = "B15003_023",
  professional   = "B15003_024",
  doctorate      = "B15003_025",
  pop_white      = "B02001_002",   # white alone
  pop_black      = "B02001_003",   # Black alone
  pop_hispanic   = "B03003_003",   # Hispanic or Latino
  employed       = "B23025_004",   # employed in labor force
  labor_force    = "B23025_002",   # civilian labor force
  median_age     = "B01002_001",   # median age
  rural_housing  = "B25001_001"    # housing units (proxy for density)
)

# Pull in "tidy" (long) format so that a suppressed value for one variable
# doesn't silently drop the entire county row (which is what "wide" does).
# We pivot wide ourselves afterward, keeping all counties.
acs_raw <- get_acs(
  geography = "county",
  variables = acs_vars,
  year      = 2022,
  survey    = "acs5",
  output    = "tidy"   # long: one row per county × variable
)

# Quick suppression check — good to know before proceeding
suppressed <- acs_raw %>% filter(is.na(estimate))
message(sprintf(
  "Suppressed cells: %d out of %d (%.1f%%) across all variables",
  nrow(suppressed), nrow(acs_raw),
  100 * nrow(suppressed) / nrow(acs_raw)
))
suppressed %>%
  count(variable, sort = TRUE) %>%
  print(n = Inf)

# Pivot wide — counties with suppressed values get NA for that variable only
acs_wide <- acs_raw %>%
  select(GEOID, NAME, variable, estimate) %>%
  pivot_wider(names_from = variable, values_from = estimate)

acs <- acs_wide %>%
  transmute(
    county_fips   = GEOID,
    pop_total     = pop_total,
    median_hhinc  = median_hhinc,
    share_college = (bach_plus + masters + professional + doctorate) / pop_25plus,
    share_white   = pop_white  / pop_total,
    share_black   = pop_black  / pop_total,
    share_hisp    = pop_hispanic / pop_total,
    unemp_rate    = 1 - (employed / labor_force),
    median_age    = median_age,
    log_pop       = log(pop_total)
  )

message(sprintf("ACS counties before merge: %d", nrow(acs)))

# ==============================================================================
# SECTION B: Load tariff exposure and voting data
# ==============================================================================

# B1. County-level export × tariff exposure ------------------------------------
exports <- read_csv(
  "./data/county_exposure/industry_exports.csv",
  show_col_types = FALSE
) %>%
  mutate(county_fips = str_pad(as.character(county_fips), 5, pad = "0"))

# Build county-level exposure measure:
# We want total export value exposed to retaliatory tariffs, weighted by tariff rate
# tariff_exposure = sum(export_value * etr) / total_exports  → effective exposure rate
# (If you just want raw dollar exposure, use sum(export_value * etr) instead)

tariff_naics <- read_csv(
  "./data/county_exposure/tariff_by_industry/derived/trade_retaliations_by_naics.csv",
  show_col_types = FALSE
) %>%
  mutate(
    etr          = readr::parse_number(tariff) / 100,
    naics6_clean = str_replace_all(as.character(naics6), "[^0-9]", ""),
    naics6_clean = na_if(str_replace(naics6_clean, "\\.0$", ""), ""),
    naics6_key   = str_pad(naics6_clean, 6, pad = "0"),
    naics4_key   = str_sub(naics6_key, 1, 4),
    naics2_key   = str_sub(naics6_key, 1, 2)
  ) %>%
  filter(!is.na(etr), !is.na(naics6_clean))

etr_by_naics6 <- tariff_naics %>%
  group_by(naics6_key) %>%
  summarise(mean_etr = mean(etr, na.rm = TRUE), .groups = "drop")

etr_by_naics4 <- tariff_naics %>%
  group_by(naics4_key) %>%
  summarise(mean_etr_4 = mean(etr, na.rm = TRUE), .groups = "drop")

etr_by_naics2 <- tariff_naics %>%
  group_by(naics2_key) %>%
  summarise(mean_etr_2 = mean(etr, na.rm = TRUE), .groups = "drop")

exports_clean <- exports %>%
  filter(!is.na(county_fips), !is.na(NAICS2017)) %>%
  mutate(
    naics_str  = as.character(NAICS2017),
    naics6_key = if_else(str_detect(naics_str, "^[0-9]{6}$"), naics_str, NA_character_),
    naics4_key = if_else(!is.na(naics6_key), str_sub(naics6_key, 1, 4), NA_character_),
    naics2_key = if_else(!is.na(naics6_key), str_sub(naics6_key, 1, 2), NA_character_)
  )

county_exposure <- exports_clean %>%
  left_join(etr_by_naics6, by = "naics6_key") %>%
  left_join(etr_by_naics4, by = "naics4_key") %>%
  left_join(etr_by_naics2, by = "naics2_key") %>%
  mutate(
    mean_etr                = coalesce(mean_etr, mean_etr_4, mean_etr_2, 0),
    tariff_weighted_exports = export_value_weighted * mean_etr
  ) %>%
  filter(!is.na(county_fips)) %>%
  group_by(county_fips) %>%
  summarise(
    total_exports           = sum(export_value_weighted,   na.rm = TRUE),
    total_exposed_exports   = sum(tariff_weighted_exports, na.rm = TRUE),
    effective_exposure_rate = total_exposed_exports / pmax(total_exports, 1),
    n_naics_matched         = sum(mean_etr > 0),
    .groups = "drop"
  )

message(sprintf("Counties in exposure data: %d", n_distinct(county_exposure$county_fips)))
message(sprintf("Counties with non-zero exposure: %d", sum(county_exposure$effective_exposure_rate > 0)))

# B2. Voting data --------------------------------------------------------------
votes <- read_excel("./data/votes/derived/voting_by_county_real.xlsx") %>%
  mutate(
    county_fips = str_pad(as.character(combined_fips), 5, pad = "0"),
    state_fips  = str_sub(county_fips, 1, 2)
  )

# ==============================================================================
# SECTION C: Define "swinginess"
# ==============================================================================

votes <- votes %>%
  mutate(
    # Margin-based: absolute distance from 50/50 in 2024 (lower = more competitive)
    margin_24     = abs(per_gop_24 - 0.5),
    competitive   = margin_24 < 0.05,          # within 5 points of toss-up
    lean_gop_24   = per_gop_24 > 0.5,
    
    # Flip-based: county voted differently in at least one prior cycle
    flipped       = (lean_gop_24 != (per_gop_20 > 0.5)) |
      (lean_gop_24 != (per_gop_16 > 0.5)),
    
    # Partisan category (useful for plots)
    partisan_cat  = case_when(
      per_gop_24 >= 0.65             ~ "Safe GOP",
      per_gop_24 >= 0.55             ~ "Lean GOP",
      per_gop_24 >= 0.45             ~ "Competitive",
      per_gop_24 >= 0.35             ~ "Lean Dem",
      TRUE                           ~ "Safe Dem"
    ) %>% factor(levels = c("Safe Dem", "Lean Dem", "Competitive",
                            "Lean GOP", "Safe GOP"))
  )

# ==============================================================================
# SECTION D: Merge everything
# ==============================================================================

df_full <- votes %>%
  left_join(county_exposure, by = "county_fips") %>%
  left_join(acs,             by = "county_fips") %>%
  filter(!is.na(effective_exposure_rate), !is.na(per_gop_24))

message(sprintf("Analysis sample: %d counties", nrow(df_full)))

# Merge diagnostics
n_votes    <- n_distinct(votes$county_fips)
n_exposure <- n_distinct(county_exposure$county_fips)
n_acs      <- n_distinct(acs$county_fips)
message(sprintf("  Counties in votes:    %d", n_votes))
message(sprintf("  Counties in exposure: %d", n_exposure))
message(sprintf("  Counties in ACS:      %d", n_acs))
message(sprintf("  Final sample:         %d", nrow(df_full)))
message("Sample FIPS - votes:    ", paste(head(sort(votes$county_fips), 3), collapse=", "))
message("Sample FIPS - exposure: ", paste(head(sort(county_exposure$county_fips), 3), collapse=", "))
message("Sample FIPS - ACS:      ", paste(head(sort(acs$county_fips), 3), collapse=", "))

# ==============================================================================
# SECTION E: Regressions
# ==============================================================================
# Using fixest::feols — fast, handles state FE cleanly, gives clustered SEs

# E1. Baseline: exposure ~ GOP vote share + state FE --------------------------
m1 <- feols(
  effective_exposure_rate ~ per_gop_24 | state_fips,
  data    = df_full,
  cluster = ~state_fips
)

# E2. Add basic controls -------------------------------------------------------
m2 <- feols(
  effective_exposure_rate ~ per_gop_24 + log_pop + median_hhinc + share_college
  | state_fips,
  data    = df_full,
  cluster = ~state_fips
)

# E3. Full controls ------------------------------------------------------------
m3 <- feols(
  effective_exposure_rate ~ per_gop_24 + log_pop + median_hhinc + share_college +
    share_white + unemp_rate + median_age | state_fips,
  data    = df_full,
  cluster = ~state_fips
)

# E4. Competitive vs safe counties ---------------------------------------------
m4 <- feols(
  effective_exposure_rate ~ competitive + per_gop_24 + log_pop + median_hhinc +
    share_college | state_fips,
  data    = df_full,
  cluster = ~state_fips
)

# E5. Flipped counties ---------------------------------------------------------
m5 <- feols(
  effective_exposure_rate ~ flipped + per_gop_24 + log_pop + median_hhinc +
    share_college | state_fips,
  data    = df_full,
  cluster = ~state_fips
)

# Print results ----------------------------------------------------------------
cat("\n===== REGRESSION RESULTS =====\n")
etable(m1, m2, m3, m4, m5,
       title     = "Retaliatory Tariff Exposure ~ Partisan Lean (State FE)",
       headers   = c("Baseline", "+ Basic Controls", "+ Full Controls",
                     "+ Competitive", "+ Flipped"),
       file      = "./output/exposure_partisan/tables/regression_results.txt")

# ==============================================================================
# SECTION F: Descriptive averages (what your sister's project probably leads with)
# ==============================================================================

# F1. Mean exposure by partisan category ---------------------------------------
avg_by_cat <- df_full %>%
  group_by(partisan_cat) %>%
  summarise(
    n_counties         = n(),
    mean_exposure_rate = mean(effective_exposure_rate, na.rm = TRUE),
    mean_total_exports = mean(total_exports, na.rm = TRUE),
    .groups = "drop"
  )

print(avg_by_cat)
write_csv(avg_by_cat, "./output/exposure_partisan/tables/avg_exposure_by_partisan_cat.csv")

# F2. Mean exposure: competitive vs non-competitive ----------------------------
avg_competitive <- df_full %>%
  group_by(competitive) %>%
  summarise(
    n           = n(),
    mean_exp    = mean(effective_exposure_rate, na.rm = TRUE),
    .groups = "drop"
  )
print(avg_competitive)

# F3. Mean exposure: flipped vs stable counties --------------------------------
avg_flipped <- df_full %>%
  group_by(flipped) %>%
  summarise(
    n        = n(),
    mean_exp = mean(effective_exposure_rate, na.rm = TRUE),
    .groups = "drop"
  )
print(avg_flipped)

# ==============================================================================
# SECTION G: Figures
# ==============================================================================

out_dir <- "./output/exposure_partisan/figures"

theme_clean <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey40"),
    panel.grid.minor = element_blank()
  )

save_fig <- function(p, name, w = 10, h = 6, dpi = 300) {
  ggsave(file.path(out_dir, name), p, width = w, height = h, dpi = dpi)
}

# G1. Scatter: exposure vs GOP vote share, colored by partisan category --------
fig1 <- df_full %>%
  ggplot(aes(x = per_gop_24, y = effective_exposure_rate, color = partisan_cat)) +
  geom_point(alpha = 0.35, size = 1.2) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE,
              color = "black", linewidth = 0.9) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     name   = "GOP vote share, 2024") +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     name   = "Effective retaliatory tariff exposure rate") +
  scale_color_manual(
    values = c("Safe Dem" = "#2166ac", "Lean Dem" = "#74add1",
               "Competitive" = "#9970ab",
               "Lean GOP" = "#f4a582", "Safe GOP" = "#d6604d"),
    name = NULL
  ) +
  labs(
    title    = "County-level tariff exposure vs. GOP vote share (2024)",
    subtitle = "Each dot is a county; line is OLS fit across all counties"
  ) +
  theme_clean

save_fig(fig1, "fig1_scatter_exposure_vs_gop.png")

# G2. Bar chart: mean exposure by partisan category ----------------------------
fig2 <- avg_by_cat %>%
  ggplot(aes(x = partisan_cat, y = mean_exposure_rate, fill = partisan_cat)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = percent(mean_exposure_rate, accuracy = 0.1)),
            vjust = -0.4, size = 3.5) +
  scale_y_continuous(labels = percent_format(accuracy = 0.1),
                     expand  = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(
    values = c("Safe Dem" = "#2166ac", "Lean Dem" = "#74add1",
               "Competitive" = "#9970ab",
               "Lean GOP" = "#f4a582", "Safe GOP" = "#d6604d")
  ) +
  labs(
    x        = NULL,
    y        = "Mean effective tariff exposure rate",
    title    = "Average retaliatory tariff exposure by partisan category",
    subtitle = "Mean of (tariff-weighted exports / total exports) across counties in each group"
  ) +
  theme_clean

save_fig(fig2, "fig2_bar_exposure_by_partisan_cat.png")

# G3. Boxplot: exposure by partisan category (shows full distribution) ---------
fig3 <- df_full %>%
  ggplot(aes(x = partisan_cat, y = effective_exposure_rate, fill = partisan_cat)) +
  geom_boxplot(outlier.alpha = 0.2, show.legend = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(
    values = c("Safe Dem" = "#2166ac", "Lean Dem" = "#74add1",
               "Competitive" = "#9970ab",
               "Lean GOP" = "#f4a582", "Safe GOP" = "#d6604d")
  ) +
  labs(
    x        = NULL,
    y        = "Effective retaliatory tariff exposure rate",
    title    = "Distribution of tariff exposure by partisan category",
    subtitle = "Boxplots across counties; dots are outliers"
  ) +
  theme_clean

save_fig(fig3, "fig3_boxplot_exposure_by_partisan_cat.png")

# G4. Competitive vs non-competitive counties ----------------------------------
fig4 <- df_full %>%
  mutate(label = if_else(competitive, "Competitive\n(within 5 pts)", "Non-competitive")) %>%
  ggplot(aes(x = label, y = effective_exposure_rate, fill = label)) +
  geom_boxplot(outlier.alpha = 0.2, show.legend = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Competitive\n(within 5 pts)" = "#9970ab",
                               "Non-competitive" = "#aaaaaa")) +
  labs(
    x        = NULL,
    y        = "Effective tariff exposure rate",
    title    = "Tariff exposure: competitive vs. non-competitive counties",
    subtitle = "'Competitive' = 2024 margin within 5 percentage points of 50/50"
  ) +
  theme_clean

save_fig(fig4, "fig4_competitive_vs_not.png", w = 7)

# G5. Flipped vs stable counties -----------------------------------------------
fig5 <- df_full %>%
  mutate(label = if_else(flipped, "Flipped at least once\n(2016–2024)",
                         "Voted same party\nall three cycles")) %>%
  ggplot(aes(x = label, y = effective_exposure_rate, fill = label)) +
  geom_boxplot(outlier.alpha = 0.2, show.legend = FALSE) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Flipped at least once\n(2016–2024)" = "#e08214",
                               "Voted same party\nall three cycles"  = "#aaaaaa")) +
  labs(
    x        = NULL,
    y        = "Effective tariff exposure rate",
    title    = "Tariff exposure: vote-flipping vs. stable counties",
    subtitle = "'Flipped' = county voted differently in 2024 vs. 2020 or 2016"
  ) +
  theme_clean

save_fig(fig5, "fig5_flipped_vs_stable.png", w = 7)

message("\nAll outputs saved to ./output/exposure_partisan/")