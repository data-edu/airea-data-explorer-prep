library(tidyverse)
library(haven)
library(arrow)
library(tidycensus)

supply_data <- read_dta("prep/ccrc_cip_comp_aire_6dig.dta")

cz_labels <- read_csv("prep/cz-labels.csv")

sff <- sf::read_sf("prep/CommutingZones2020_County_GIS_files/county20.shp") %>%
  as_tibble() %>%
  select(countycd = GEOID, CZ = CZ20, -geometry) %>%
  mutate(countycd = as.integer(countycd))

supply_data %>%
  glimpse()

supply_data <- supply_data %>%
  select(year, unitid, instnm, State, fips, tribal, rural, cip, aire_relevant, cip2020title, countycd,
         inst_aire_cmplt_tot:inst_aire_cmplt_o,
         aire_ind_title)

supply_data <- supply_data %>% 
  left_join(sff, by = "countycd") %>% 
  left_join(cz_labels, by = "CZ")

supply_data %>% arrow::write_parquet("prep/supply.parquet.gzip", compression = "gzip")

supply_data <- read_parquet("prep/supply.parquet")

demand_data <- read_csv("prep/lightcast-soc-year-county-2025-02-24 7_23pm.csv")

demand_data %>% write_parquet(
  "prep/demand.parquet.gzip",
  compression = "gzip"
)

demand_data <- demand_data %>% 
  filter(!is.na(COUNTY)) %>% 
  rename(countycd = COUNTY) %>% 
  left_join(sff, by = "countycd") %>% 
  left_join(cz_labels, by = "CZ") %>% 
  select(-city_name, -COUNTY_NAME, -CZ)

demand_meta_data <- read_dta("prep/full_soc.dta")

demand_meta_data <- demand_meta_data %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from = aire_ind,
    values_from = value,
    values_fill = list(value = 0)
  ) %>% 
  janitor::clean_names()

demand_data <- demand_data %>% 
  rename(soc = SOC_CODE) %>% 
  left_join(demand_meta_data)

years <- 2010:2023

county_pop_data <- map_dfr(
  years,
  ~ get_acs(
    geography  = "county",
    variables  = "B01003_001",
    year       = .x,
    survey     = "acs5",
    cache_table= TRUE
  ) |>
    mutate(year = .x)
)

county_pop_ts <- county_pop_data %>% 
  select(-moe) %>% 
  select(COUNTY = GEOID, population_estimate = estimate, YEAR = year) %>% 
  rename(countycd = COUNTY) %>% 
  mutate(countycd = as.integer(countycd))

demand_data <- demand_data %>%
  left_join(county_pop_ts, by = c("countycd", "YEAR"))

demand_data %>% 
  arrow::write_parquet("prep/demand.parquet.gzip", compression = "gzip")
