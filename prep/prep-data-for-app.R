library(tidyverse)
library(haven)

supply_data <- read_dta("prep/ccrc_cip_comp_aire_6dig.dta")

demand_data <- read_csv("prep/lightcast-soc-year-county-2025-02-24 7_23pm.csv")

cz_labels <- read_csv("cz-labels.csv")
