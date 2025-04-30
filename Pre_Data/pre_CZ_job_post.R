# -------------------------------
# Commuting Zone level data pre
# -------------------------------



# ------------------------------------------------------------------------
# county population estimate data from tidycensus across year 2010 to 2023
# ------------------------------------------------------------------------

library(tidycensus)
library(dplyr)

Years <- 2010:2023
all_counties <- list()

# get data for each year
for (yr in Years) {
  message("Fetching ACS5 for year ", yr, " …")
  df <- get_acs(
    geography = "county",
    variables = "B01003_001E",  # total population
    year      = yr,
    survey    = "acs5"
  ) %>% 
    select(GEOID, NAME, estimate, moe) %>% 
    mutate(year = yr)
  
  all_counties[[as.character(yr)]] <- df
}

# merge all years into one data frame
county_pop_ts <- bind_rows(all_counties)
head(county_pop_ts)


# -------------------------------
# read the CZ2020 SHP data
# -------------------------------
library(readr)
library(dplyr)
library(sf)
county20<- st_read("county20/county20.shp")
county20 <- county20 %>%
  select(1,2,4)
cz20<- st_read("cz20/cz20.shp")


# -------------------------------
# CZ_Job Posting Data version 2025-03-02
# -------------------------------

#### 0302 Josh data
library(readr)
cz_0302 <- read_csv("cz_postings-2025-03-02.csv")


# 1. CZ-level population ---------------------------------------------------
cz_pop <- county_pop_ts %>%
  # Join with county-to-commuting-zone lookup to get CZ20 for each county
  inner_join(
    county20a %>% select(GEOID, CZ20),
    by = "GEOID"
  ) %>%
  # Sum county population estimates by CZ and year
  group_by(CZ20, year) %>%
  summarise(
    pop = sum(estimate, na.rm = TRUE),
    .groups = "drop"
  )

# 2. CZ-level job postings ------------------------------------------------
cz_postings <- cz_0302 %>%
  # Aggregate total and green job postings by CZ and year
  group_by(cz, year) %>%
  summarise(
    total_postings    = sum(num_postings, na.rm = TRUE),
    green_job_posting = sum(num_postings[is_green_soc == 1], na.rm = TRUE),
    .groups = "drop"
  )

# 3. Merge and compute indicators ------------------------------------------
cz_data <- cz_postings %>%
  # Join population data to postings data
  left_join(
    cz_pop,
    by = c("cz" = "CZ20", "year" = "year")
  ) %>%
  # Calculate percent green and green postings per 1,000 people
  mutate(
    pct_green = green_job_posting / total_postings * 100,
    per1000   = green_job_posting / (pop / 1000)
  )

# 4. Reattach CZ boundary geometries and prepare sf object -----------------
cz_sf <- cz20 %>%
  # Merge spatial CZ boundaries with the computed CZ data
  left_join(cz_data, by = c("CZ20" = "cz")) %>%
  # Rename for consistency and drop any future year (2024)
  rename(YEAR = year) %>%
  filter(YEAR != 2024)

# 5. Convert to sf object --------------------------------------------------
cz_sf1 <- cz_sf %>%
  # Convert to simple features, project to WGS84, and simplify geometry
  st_as_sf() %>%
  st_transform(crs = 4326) %>%
  st_simplify(dTolerance = 0.05, preserveTopology = TRUE)

# 6. Save as RDS ------------------------------------------------------------
# Save the final sf object for later use
library(readr)
write_rds(cz_sf1, "CZ_job_post.rds")


