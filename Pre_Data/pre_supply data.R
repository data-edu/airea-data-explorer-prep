
##### supply data preparation using data from Matias
library(haven)
library(tidyverse)

# JR edit for new data
# ccrc_cip_comp <- read_dta("ccrc_cip_comp.dta")

ccrc_cip_comp <- read_rds("ccrc_cip_comp_cz.rds")

colnames(ccrc_cip_comp)

str(ccrc_cip_comp)

### transfer ccrc_cip_comp year to numeric
ccrc_cip_comp$year <- as.numeric(ccrc_cip_comp$year)

# Write ccrc_cip_comp to a RDS
saveRDS(ccrc_cip_comp, "ccrc_cip_comp.rds")



