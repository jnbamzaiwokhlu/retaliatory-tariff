# 0. load relevant packages
library(readr)
library(dplyr)
library(stringr)
install.packages("remotes")
remotes::install_github("insongkim/concordance")
library(concordance)

# 1. set wd to be main - change as needed 
# setwd("/Users/himalbamzai-wokhlu/Desktop/JBW_Wins/retaliatory-tariff")

# 2. load in cleaned data
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
