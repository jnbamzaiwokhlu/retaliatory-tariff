# retaliatory-tariff
Examining the political salience effects of retaliatory tariffs against the US! 

# Retaliatory Tariff Project — Output Guide

This README explains where all figures, tables, and data files live,
and which script produced them. Run the scripts in the order listed below.

---

## Scripts (run in this order)

| Order | Script | What it does |
|-------|--------|--------------|
| 1 | `build_cty_exposure.R` | Builds the tariff × NAICS crosswalk, runs QA, produces sector figures, and creates the county-level exposure dataset |
| 2 | `analyze_descriptive.R` | Scatter plots and boxplots of exposure vs. GOP vote share |
| 3 | `analyze_exposure_partisan.R` | Regression analysis with state fixed effects, ACS controls, and coefficient plots |

---

## Figures

### Tariff exposure by industry sector
> Folder: `output/tariff_by_industry/figures/`
> Produced by: `build_cty_exposure.R`

| File | What it shows |
|------|---------------|
| `fig1_count_tariff_lines_by_naics2.png` | Number of tariff lines hitting each NAICS sector (manufacturing collapsed to 31–33) |
| `fig2_mean_etr_by_naics2.png` | Average retaliatory tariff rate by sector |
| `fig3_p90_etr_by_naics2.png` | 90th percentile tariff rate by sector (severity measure) |
| `fig4_max_etr_by_naics2.png` | Maximum tariff rate by sector |
| `fig5_share_lines_ge25_by_naics2.png` | Share of tariff lines with rate ≥ 25%, by sector |
| `fig6_etr_distribution_boxplot_top_naics2.png` | Full distribution of tariff rates across top 12 sectors (boxplot) |

---

### Exposure vs. partisan lean — descriptive
> Folder: `output/descriptive/`
> Produced by: `analyze_descriptive.R`

| File | What it shows |
|------|---------------|
| `fig1_scatter_exposure_vs_gop.png` | **Main figure.** Scatter of GOP 2024 vote share vs. tariff exposure percentile, with top-3 most-exposed counties labeled in each GOP quartile |
| `fig2_boxplot_exposure_by_gop_quartile.png` | Distribution of exposure across Dem-leaning → GOP-leaning county quartiles |
| `fig3_faceted_by_gop_quartile.png` | Same scatter as Fig 1, split into four panels by quartile — easier to read within-quartile patterns |

---

### Regression results
> Folder: `output/exposure_partisan/figures/`
> Produced by: `analyze_exposure_partisan.R`

| File | What it shows |
|------|---------------|
| `fig_coef_plot.png` | Coefficient plot showing the effect of GOP vote share on exposure across all specifications, with 95% confidence intervals. Left panel: log dollar exposure. Right panel: exposure percentile rank. |

---

## Tables

### QA
> Folder: `output/tariff_by_industry/qa/`

| File | What it contains |
|------|-----------------|
| `problem_naics_rows.csv` | Tariff lines that failed QA (unmapped, bad NAICS codes, public admin, HS chapter 97/000) — flagged and excluded from analysis |

### Descriptive
> Folder: `output/descriptive/`

| File | What it contains |
|------|-----------------|
| `summary_table_by_gop_quartile.csv` | Mean and median exposure percentile, GOP vote share range, and share of counties above 75th percentile — one row per GOP quartile. Good for putting numbers in text. |

### Regressions
> Printed to console when `analyze_exposure_partisan.R` is run (markdown format).
> To save, change `output = "markdown"` to a file path e.g. `output = "./output/exposure_partisan/tables/panel_a.md"`

| Panel | Outcome variable | Interpretation |
|-------|-----------------|----------------|
| Panel A | log(tariff-weighted export exposure) | A 1 percentage point increase in GOP vote share is associated with X% more tariff exposure |
| Panel B | Exposure percentile rank (0–100) | A 1 percentage point increase in GOP vote share moves a county Y percentile points up the exposure distribution |

Both panels run 5 specifications: baseline (state FE only), + log population, + economic controls, + full demographic controls, and + full controls weighted by county population.

---

## Key derived data files

> These are intermediate files used by the analysis scripts — not final outputs, but useful to keep.

| File | What it contains |
|------|-----------------|
| `data/county_exposure/county_exposure_summary.csv` | **Main analysis file.** One row per county: total export exposure, tariff-weighted exposure, and effective exposure rate |
| `data/county_exposure/industry_exports.csv` | Long file: county × NAICS2 × year, with export values, employment, and ETR rates |
| `data/county_exposure/tariff_by_industry/derived/trade_retaliations_by_naics.csv` | Tariff lines with HS → NAICS6 crosswalk applied |

---

## Notes

- **Exposure measure:** County export exposure is allocated from customs district totals to counties using employment share by NAICS2 sector (from Census CBP data). The tariff rate applied is the mean retaliatory ETR for each sector.
- **Partisan data:** 2024, 2020, and 2016 presidential election results by county. "Competitive" counties are within 5 percentage points of 50/50 in 2024. "Flipped" counties voted differently in 2024 vs. at least one prior cycle.
- **ACS controls:** 2022 5-year estimates. Includes log population, median household income, college share, racial composition, unemployment rate, and median age.
- **Regressions:** OLS with state fixed effects. Standard errors clustered by state throughout.