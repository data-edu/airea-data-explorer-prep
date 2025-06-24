library(tidyverse)
library(haven)
library(tidycensus)
library(treemapify)

# prep

years <- 2010:2023            # change range as needed

county_pop_ts <- map_dfr(
  years,
  ~ get_acs(
    geography  = "county",
    variables  = "B01003_001",
    year       = .x,
    survey     = "acs5",
    cache_table= TRUE
  ) |>
    mutate(year = .x)         # tag each row with its ACS year
)

county_pop_ts %>% 
  select(-moe)

# demand

onet <- read_dta("prep/relevant_soc_airea.dta")

d <- read_csv("prep/lightcast-soc-year-county-2025-02-24 7_23pm.csv")

cz_labels <- read_csv("cz-labels.csv")

sff <- sf::read_sf("prep/CommutingZones2020_County_GIS_files/county20.shp") %>%
  select(COUNTY = GEOID, CZ = CZ20) %>%
  # mutate(COUNTY = as.integer(COUNTY)) %>%
  as_tibble() %>%
  select(-geometry)

d <- d %>%
  mutate(airea = if_else(SOC_CODE %in% onet$soc, 1, 0))

# change COUNTY into a character string with a leading 0 if there is not one
d <- d %>%
  mutate(COUNTY = str_pad(as.character(COUNTY), width = 5, side = "left", pad = "0"))

county_pop_tj <- select(county_pop_ts, COUNTY = GEOID, population_estimate = estimate, YEAR = year)

d_prepped  <- d %>% 
  left_join(county_pop_tj) %>% 
  left_join(sff)

d_prepped

# cz_tw
bls <- read_dta("prep/bls_ed_requirement.dta")

bls <- bls %>% 
  rename(SOC_CODE = soc) %>% 
  select(-3)

cz_tw <- d_prepped %>% 
  mutate(JOB_POSTS_P1000 = JOB_POSTING_COUNT/population_estimate) %>% 
  group_by(CZ, airea, SOC_CODE, YEAR) %>% 
  summarize(TOTAL_JOB_POSTS = sum(as.integer(JOB_POSTING_COUNT)),
            JOB_POSTS_P1000 = sum(JOB_POSTS_P1000)) %>% 
  ungroup() %>%
  select(CZ, everything()) %>% 
  arrange(CZ) %>% 
  left_join(select(cz_labels, CZ, CZ_label))

cz_tw %>% 
  left_join(bls)

cz_air1 <- cz_tw %>%                     # starting data
  group_by(CZ_label, YEAR) %>%           # one row per CZ-year
  summarise(
    ## (1) count of airea == 1 postings
    posts_air1       = sum(TOTAL_JOB_POSTS[airea == 1], na.rm = TRUE),
    
    ## (2) airea == 1 postings per 1 000 residents
    # -- If JOB_POSTS_P1000 is already “per-1 000” *for each row*,
    #    summing is fine.  Otherwise replace with your own ratio:
    posts_air1_p1000 = sum(JOB_POSTS_P1000[airea == 1], na.rm = TRUE),
    
    ## total postings (airea 0 + 1) – needed to compute the share
    posts_all        = sum(TOTAL_JOB_POSTS,                   na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ## (3) airea-1 share of all postings
    posts_air1_pct = posts_air1 / posts_all * 100
  ) %>%
  select(CZ_label, YEAR,
         airea_posts = posts_air1,
         airea_posts_p1000 = posts_air1_p1000,
         airea_pct = posts_air1_pct)

cz_air1 %>% write_csv("cz_air1.csv") # for the table and the time series plot

cz_air1

cz_to_plot <- "Aberdeen, SD CZ"

soc_totals <- cz_tw %>%                                  # full table
  filter(CZ_label == cz_to_plot) %>%                     # one CZ
  group_by(SOC_CODE) %>%                                 # ignore year
  summarise(posts = sum(TOTAL_JOB_POSTS, na.rm = TRUE),
            .groups = "drop") %>% 
  rename(soc = SOC_CODE) %>% 
  left_join(select(onet, soc, soc_title = soc2018title)) %>% 
  arrange(desc(posts)) %>% 
  slice_head(n = 15) %>%                                 # top 15 codes
  mutate(soc_title = str_wrap(soc_title, 24))            # gentle wrapping

onet

# ggplot(soc_totals) +
#   geom_treemap(aes(area = posts, fill = posts)) +
#   geom_fit_text(
#     aes(area = posts, label = soc_title),  # map area + label again
#     stat      = "treemap",                 # << key linelib
#     grow      = TRUE,
#     reflow    = TRUE,
#     min.size  = 6,                         # hide tiny unreadable labels
#     colour    = "white",
#     padding.x = grid::unit(1, "mm"),
#     padding.y = grid::unit(1, "mm")
#   ) +
#   scale_fill_continuous(type = "viridis") +
#   labs(title = glue("Job-posting volume by SOC code ({cz_to_plot})"),
#        fill  = "Job posts") +
#   theme(legend.position = "bottom")

# CZ label by year by SOC, with SOC labels and SOC AIREA status
# and postings, %, and by 1000 residents, as well as job requirement

# supply

institutions <- read_rds("ccrc_cip_comp_cz.rds")

institutions

institutions %>% 
  select(instnm, year, hbcu, tribal, rural)

# institution by year by CIP, with CIP labels and CIP AIREA status
# and with rural, tribal, HBCU, and entry level education flags, and completions and %