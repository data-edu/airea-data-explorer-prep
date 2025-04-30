
##### supply data preparation using data from Matias
library(haven)
ccrc_cip_comp <- read_dta("ccrc_cip_comp.dta")

colnames(ccrc_cip_comp)

str(ccrc_cip_comp)

### transfer ccrc_cip_comp year to numeric
ccrc_cip_comp$year <- as.numeric(ccrc_cip_comp$year)

# Write ccrc_cip_comp to a RDS
saveRDS(ccrc_cip_comp, "ccrc_cip_comp.rds")



