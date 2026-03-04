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
# Read the pre-built county exposure summary from build_cty_exposure.R
# (which correctly joins exports to tariff rates via NAICS3 from Commodity labels)
county_exposure <- read_csv(
  "./data/county_exposure/county_exposure_summary.csv",
  show_col_types = FALSE
) %>%
  mutate(county_fips = str_pad(as.character(county_fips), 5, pad = "0"))

# Build county-level exposure measure:
# We want total export value exposed to retaliatory tariffs, weighted by tariff rate
# tariff_exposure = sum(export_value * etr) / total_exports  → effective exposure rate
# (If you just want raw dollar exposure, use sum(export_value * etr) instead)

# (exposure already loaded above from county_exposure_summary.csv))

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
  filter(!is.na(total_tariff_weighted), !is.na(per_gop_24)) %>%
  mutate(
    # Outcome 1: log dollar exposure (elasticity interpretation)
    # Add 1 before log to handle any zeros gracefully
    log_exposure     = log(total_tariff_weighted + 1),
    
    # Outcome 2: percentile rank (rank-based, robust to outliers)
    exposure_pctile  = percent_rank(total_tariff_weighted) * 100,
    
    # GOP share as 0-100 so coefficient = effect of 1 percentage point
    gop_share_pct    = per_gop_24 * 100,
    log_gop_share    = log(per_gop_24),          # for log-log spec
    log_gop_dem_ratio = log(per_gop_24 / (1 - per_gop_24))  # log odds ratio
  )

# Merge diagnostics
message(sprintf("Final sample: %d counties", nrow(df_full)))
message(sprintf("  Counties in votes:    %d", n_distinct(votes$county_fips)))
message(sprintf("  Counties in exposure: %d", n_distinct(county_exposure$county_fips)))
message(sprintf("  Counties in ACS:      %d", n_distinct(acs$county_fips)))

# ==============================================================================
# SECTION E: Regressions
# ==============================================================================
# Two outcome variables, run in parallel:
#   (A) log_exposure   -> "a 1pp increase in GOP share is associated with X% more exposure"
#   (B) exposure_pctile -> "a 1pp increase in GOP share moves a county Y percentile points"
#
# Five specifications per outcome:
#   1. Baseline: GOP share + state FE
#   2. + population (log)
#   3. + basic economic controls
#   4. + full demographic controls
#   5. Full controls, weighted by county population
#
# SEs clustered by state throughout.

# --- PANEL A: log(exposure) as outcome ----------------------------------------

a1 <- feols(log_exposure ~ gop_share_pct | state_fips,
            data = df_full, cluster = ~state_fips)

a2 <- feols(log_exposure ~ gop_share_pct + log_pop | state_fips,
            data = df_full, cluster = ~state_fips)

a3 <- feols(log_exposure ~ gop_share_pct + log_pop + median_hhinc + share_college
            | state_fips,
            data = df_full, cluster = ~state_fips)

a4 <- feols(log_exposure ~ gop_share_pct + log_pop + median_hhinc + share_college +
              share_white + unemp_rate + median_age | state_fips,
            data = df_full, cluster = ~state_fips)

a5 <- feols(log_exposure ~ gop_share_pct + log_pop + median_hhinc + share_college +
              share_white + unemp_rate + median_age | state_fips,
            data = df_full, cluster = ~state_fips,
            weights = ~pop_total)

# --- PANEL B: exposure percentile as outcome ----------------------------------

b1 <- feols(exposure_pctile ~ gop_share_pct | state_fips,
            data = df_full, cluster = ~state_fips)

b2 <- feols(exposure_pctile ~ gop_share_pct + log_pop | state_fips,
            data = df_full, cluster = ~state_fips)

b3 <- feols(exposure_pctile ~ gop_share_pct + log_pop + median_hhinc + share_college
            | state_fips,
            data = df_full, cluster = ~state_fips)

b4 <- feols(exposure_pctile ~ gop_share_pct + log_pop + median_hhinc + share_college +
              share_white + unemp_rate + median_age | state_fips,
            data = df_full, cluster = ~state_fips)

b5 <- feols(exposure_pctile ~ gop_share_pct + log_pop + median_hhinc + share_college +
              share_white + unemp_rate + median_age | state_fips,
            data = df_full, cluster = ~state_fips,
            weights = ~pop_total)

# --- Print and save -----------------------------------------------------------
# install.packages("modelsummary") if needed
library(modelsummary)

coef_map <- c(
  "gop_share_pct" = "GOP vote share (pp)",
  "log_pop"       = "log(population)",
  "median_hhinc"  = "Median HH income",
  "share_college" = "Share college+",
  "share_white"   = "Share white",
  "unemp_rate"    = "Unemployment rate",
  "median_age"    = "Median age"
)

cat("\n========== PANEL A: log(tariff exposure) ==========\n")
cat("Interpretation: coefficient = % change in exposure per 1pp increase in GOP share\n\n")
modelsummary(
  list("Baseline"      = a1,
       "+ log(pop)"    = a2,
       "+ Econ"        = a3,
       "+ Full"        = a4,
       "+ Full (wtd)"  = a5),
  coef_map  = coef_map,
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  output    = "markdown"
)

cat("\n========== PANEL B: exposure percentile ==========\n")
cat("Interpretation: coefficient = percentile-point shift per 1pp increase in GOP share\n\n")
modelsummary(
  list("Baseline"      = b1,
       "+ log(pop)"    = b2,
       "+ Econ"        = b3,
       "+ Full"        = b4,
       "+ Full (wtd)"  = b5),
  coef_map  = coef_map,
  stars     = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  gof_map   = c("nobs", "r.squared", "adj.r.squared"),
  output    = "markdown"
)

# --- Coefficient plot ---------------------------------------------------------
# Pull key coefficient (gop_share_pct) across all specs for a visual summary
coef_df <- bind_rows(
  # Panel A
  purrr::map2_dfr(
    list(a1, a2, a3, a4, a5),
    c("Baseline", "+ log(pop)", "+ Econ controls", "+ Full controls", "+ Full (pop. wtd)"),
    ~ tibble(
      spec    = .y,
      outcome = "log(tariff exposure)",
      coef    = coef(.x)["gop_share_pct"],
      se      = se(.x)["gop_share_pct"]
    )
  ),
  # Panel B
  purrr::map2_dfr(
    list(b1, b2, b3, b4, b5),
    c("Baseline", "+ log(pop)", "+ Econ controls", "+ Full controls", "+ Full (pop. wtd)"),
    ~ tibble(
      spec    = .y,
      outcome = "Exposure percentile",
      coef    = coef(.x)["gop_share_pct"],
      se      = se(.x)["gop_share_pct"]
    )
  )
) %>%
  mutate(
    ci_lo = coef - 1.96 * se,
    ci_hi = coef + 1.96 * se,
    spec  = factor(spec, levels = rev(c("Baseline", "+ log(pop)", "+ Econ controls",
                                        "+ Full controls", "+ Full (pop. wtd)")))
  )

fig_coef <- ggplot(coef_df, aes(x = coef, y = spec, color = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0.25, linewidth = 0.8) +
  geom_point(size = 3) +
  facet_wrap(~ outcome, scales = "free_x", nrow = 1) +
  scale_color_manual(values = c("log(tariff exposure)" = "#2166ac",
                                "Exposure percentile"  = "#d6604d"),
                     guide = "none") +
  labs(
    x        = "Coefficient on GOP vote share (1 percentage point)",
    y        = NULL,
    title    = "Effect of GOP vote share on retaliatory tariff exposure",
    subtitle = "Coefficient estimates with 95% CIs across specifications; state FE + SEs clustered by state",
    caption  = "Left panel: log($ exposure), right panel: exposure percentile rank (0-100)."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold"),
    plot.subtitle    = element_text(size = 10, color = "grey40"),
    panel.grid.minor = element_blank(),
    strip.text       = element_text(face = "bold")
  )

ggsave("./output/exposure_partisan/figures/fig_coef_plot.png",
       fig_coef, width = 12, height = 5, dpi = 320)

message("\nAll regression outputs saved to ./output/exposure_partisan/")
message("Tables: panel_a_log_exposure.txt, panel_b_pctile_exposure.txt")
message("Figure: fig_coef_plot.png")