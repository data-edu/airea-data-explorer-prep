## Commuting Zone level data
### read the commuting zone data
library(readr)
library(dplyr)



######### demand side data preparation
#### read the CZ2020 SHP data
library(sf)

CZ2020 <- st_read("cz20/cz20.shp")

county20<- st_read("county20/county20.shp")

county20 <- county20 %>%
  select(1,2,4)

cz20<- st_read("cz20/cz20.shp")





#### this is updated Soc data from Josh
lightcast_soc_year_county_green <- read_csv("lightcast-soc-year-county-2025-02-28.csv")
View(lightcast_soc_year_county_green)

unique(lightcast_soc_year_county_green$COUNTY)


library(dplyr)

green_jobs <- lightcast_soc_year_county_green %>%
  filter(GREEN == 1, YEAR >= 2010, YEAR <= 2024) %>%
  group_by(COUNTY, COUNTY_NAME, YEAR, SOC_CODE) %>%
  summarize(green_job_posting_count = sum(JOB_POSTING_COUNT, na.rm = TRUE)) %>%
  ungroup()

other_jobs <- lightcast_soc_year_county_green %>%
  filter(GREEN == 0, YEAR >= 2010, YEAR <= 2024) %>%
  group_by(COUNTY, COUNTY_NAME, YEAR, SOC_CODE) %>%
  summarize(other_job_posting_count = sum(JOB_POSTING_COUNT, na.rm = TRUE)) %>%
  ungroup()

unique_counties <- length(unique(green_jobs$COUNTY))
print(unique_counties)

### calculate the total green job posts for each county acorss years
green_job_posts <- green_jobs %>%
  group_by(COUNTY,COUNTY_NAME,YEAR) %>%
  summarize(green_job_posts = sum(green_job_posting_count, na.rm = TRUE)) %>%
  ungroup()


#### calculate the total other job posts for each county acorss years
other_job_posts <- other_jobs %>%
  group_by(COUNTY,COUNTY_NAME, YEAR) %>%
  summarize(other_job_posts = sum(other_job_posting_count, na.rm = TRUE)) %>%
  ungroup()


## write the green job posts and other_job posts to rds file
write_rds(green_job_posts, "green_job_posts.rds")
write_rds(other_job_posts, "other_job_posts.rds")




#### merge the green job posts with the county20 data

#  Convert COUNTY in green_job_posts to 5-bit string
green_job_posts1 <- green_job_posts %>%
  mutate(COUNTY = sprintf("%05d", COUNTY))


cz_with_jobs <- green_job_posts1 %>%
  left_join(county20, by = c("COUNTY" = "GEOID"))


### Calculate the total number of green jobs per CZ
cz_with_jobs_summed <- cz_with_jobs %>%
  group_by(CZ20, YEAR) %>%
  summarise(green_job_postings = sum(green_job_posts, na.rm = TRUE)) %>%
  ungroup()



#### merge the cz_with_jobs_summed with the CZ2020 by CZ20
CZ_jobpostings_final <- left_join(cz_with_jobs_summed, CZ2020, by = "CZ20")


library(sf)
CZ_jobpostings_final_sf <- st_as_sf(CZ_jobpostings_final)

CZ_jobpostings_final1 <- st_transform(CZ_jobpostings_final_sf, crs = 4326)

### The parameter dTolerance is adjusted according to your data range and desired precision.
CZ_jobpostings_final1 <- st_simplify(CZ_jobpostings_final1, dTolerance = 0.05, preserveTopology = TRUE)

### sava as rds file
write_rds(CZ_jobpostings_final1, "CZ_job_post.rds")





# -------------------------------
# CZ_Job Posting Data version 2025-03-02
# -------------------------------

library(readr)
test_0302 <- read_csv("cz_postings-2025-03-02.csv")
#### 0302 Josh data
library(readr)
cz_0302 <- read_csv("cz_postings-2025-03-02.csv")

cz_0302 <- cz_0302 %>%
  filter(is_green_soc == 1) 

cz_0302 <- cz_0302 %>%
  select(-2)

cz_0302_geo <- left_join(cz_0302, cz20, by = c("cz" = "CZ20"))

cz_0302_geo <- cz_0302_geo %>%
     rename(CZ20 = cz,YEAR =year, green_job_postings = num_postings)


### delete year 2024
cz_0302_geo <- cz_0302_geo %>%
  filter(YEAR != 2024)


cz_0302_geo_sf <- st_as_sf(cz_0302_geo)

cz_0302_geo_sf1 <- st_transform(cz_0302_geo_sf, crs = 4326)

cz_0302_geo_sf1 <- st_simplify(cz_0302_geo_sf1, dTolerance = 0.05, preserveTopology = TRUE)


write_rds(cz_0302_geo_sf1, "CZ_job_post.rds")













