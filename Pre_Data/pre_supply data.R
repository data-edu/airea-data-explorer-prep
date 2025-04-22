####### IPEDs Supply Data (for institution search)
hdallyears <- readRDS("hdallyears.rds")
ipeds_green_summed <- readRDS("ipeds_green_summed.rds")
# Merge data for institution search
hdallyears_joined <- hdallyears %>%
  left_join(ipeds_green_summed, by = "unitid")


str(hdallyears_joined$longitud)





##### supply data preparation using data from Matias
library(haven)
ccrc_cip_comp <- read_dta("ccrc_cip_comp.dta")

colnames(ccrc_cip_comp)

str(ccrc_cip_comp)

### transfer ccrc_cip_comp year to numeric
ccrc_cip_comp$year <- as.numeric(ccrc_cip_comp$year)

unique(ccrc_cip_comp$longitud)


unique(ccrc_cip_comp$mfreq_green_cip_stitle1)

unique(ccrc_cip_comp$instnm)

#按照A到Z打印instnm
ccrc_cip_comp$instnm <- sort(ccrc_cip_comp$instnm)


# Write ccrc_cip_comp to a RDS
saveRDS(ccrc_cip_comp, "ccrc_cip_comp.rds")






library(dplyr)

# 定义年份范围
all_years <- 2010:2023

# 按年份分组，提取每年所有唯一的学校名称，返回一个列表（每个元素对应一个年份）
schools_by_year <- ccrc_cip_comp %>%
  filter(year %in% all_years) %>%
  group_by(year) %>%
  summarise(schools = list(unique(instnm))) %>%
  ungroup() %>%
  pull(schools)

# 计算所有年份间的交集：即仅包含那些每年都存在的学校名称
common_schools <- Reduce(intersect, schools_by_year)

# 打印结果
print(common_schools)
