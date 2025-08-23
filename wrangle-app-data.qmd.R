library(tidyverse)
library(haven)
library(arrow)
library(scales)

supply <- read_parquet("prep/supply.parquet.gzip")
soc <- read_dta("prep/full_soc.dta")

supply_table <- supply %>% 
  group_by(instnm) %>% 
  summarize(mean_completions = mean(total_completions, na.rm = TRUE),
            mean_airea_completions = mean(airea_completions, na.rm = TRUE),
            mean_students_enrolled = mean(total_students_enrolled, na.rm = TRUE),
            rural = first(rural),
            tribal = first(tribal),
            cz_label = first(cz_label)) %>% 
  select(institution = instnm,
         cz_label,
         mean_completions, 
         mean_airea_completions,
         mean_students_enrolled,
         rural,
         tribal) %>% 
  mutate(pct_airea_completions = mean_airea_completions/mean_completions)

write_csv(supply_table, "prep/supply-table.csv")

open_ds <- open_dataset("prep/supply_partitioned", format = "parquet")

selected_instnm <- open_ds %>%
  filter(instnm == "Santa Barbara City College") %>% 
  collect()

selected_instnm %>% 
  group_by(year) %>% 
  summarize(total_completions = sum(total_completions, na.rm = TRUE),
            total_students_enrolled = sum(total_students_enrolled, na.rm = TRUE),
            total_airea_completions = sum(airea_completions, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = total_completions)) +
  geom_line(linewidth = 1.1, color = "#0072B2") +
  geom_point(size = 2, color = "#0072B2") +
  theme_bw() +
  scale_x_continuous(breaks = seq(2010, 2022, 1)) +
  labs(
    title = "AIREA Completions Over Time",
    x = NULL,
    y = "AIREA Completions"
  ) +
  theme_minimal(base_size = 13)



selected_instm_year <- open_ds %>%
  filter(instnm == "Santa Barbara City College", year == 2012L) %>% 
  collect()

selected_instm_year %>% 
  group_by(ciptitle, award_level) %>% 
  summarize(total_airea_completions = sum(airea_completions, na.rm = TRUE),
            .groups = "drop") %>% 
  filter(total_airea_completions > 0) %>%
  mutate(
    # Use the value labels as factor levels in the legend
    award_level = haven::as_factor(award_level, levels = "labels"),
    award_level = fct_rev(award_level)
    # If you prefer "code – label", use: levels = "both"
  ) %>% 
  ggplot(aes(x = reorder(ciptitle, total_airea_completions),
             y = total_airea_completions,
             fill = award_level)) +
  geom_col(position = "fill") +
  coord_flip() +
  theme_bw() +
  labs(y = "Total AIREA Completions", x = NULL, fill = "Award level") +
  # reverse the order of the legend
  scale_fill_brewer("", palette = "Set1") +
  theme(legend.position = "top")

demand_tab <- arrow::read_parquet("prep/demand.parquet.gzip")

cz_year <- demand_tab %>%
  select(cz_label, year, mean_population, total_job_postings, airea) %>%
  group_by(cz_label, year) %>%
  summarise(
    # If total_job_postings is a CZ-year total repeated across rows,
    # use max() instead of sum() here.
    posts_total = sum(total_job_postings, na.rm = TRUE),
    posts_airea = sum(total_job_postings[airea == 1], na.rm = TRUE),
    cz_pop_year = mean(mean_population, na.rm = TRUE),
    .groups = "drop"
  )

cz_summary <- cz_year %>%
  group_by(cz_label) %>%
  summarise(
    cz_population   = mean(cz_pop_year, na.rm = TRUE),                 # CZ population (avg over years)
    mean_job_posts  = mean(posts_total, na.rm = TRUE),                 # mean job posts over time
    airea_job_posts = mean(posts_airea, na.rm = TRUE),                 # AIREA category job posts over time
    airea_pct_posts = if_else(mean_job_posts > 0,
                              airea_job_posts / mean_job_posts, NA_real_),  # AIREA % posts over time
    posts_per_1000  = if_else(cz_population > 0,
                              (mean_job_posts / cz_population) * 1000, NA_real_), # posts per 1,000 people
    .groups = "drop"
  ) %>%
  arrange(desc(mean_job_posts))

cz_table <- cz_summary %>%
  arrange(desc(mean_job_posts)) %>%
  select(
    `CZ label`        = cz_label,
    `tot job posts`   = mean_job_posts,
    `AIREA job posts` = airea_job_posts,
    `AIREA %`         = airea_pct_posts,
    `posts per 1,000` = posts_per_1000,
    `CZ population`   = cz_population
  )

cz_table %>% 
  write_csv("prep/cz-summary-table.csv")

demand_ds <- open_dataset("prep/demand_partitioned", format = "parquet")

demand_selected_inst <- demand_ds %>%
  filter(cz_label == "Knoxville, TN") %>% 
  collect() %>% 
  filter(year != 2025) %>% 
  group_by(year) %>%
  summarise(
    posts_total = sum(total_job_postings, na.rm = TRUE),
    posts_airea = sum(total_job_postings[airea == 1], na.rm = TRUE),
    pop_year    = mean(mean_population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    posts_per_1000 = (posts_airea / pop_year) * 1000
  )

demand_selected_inst %>% 
  ggplot(aes(x = year, y = posts_airea)) +
  geom_line(linewidth = 1.1, color = "#0072B2") +
  geom_point(size = 2, color = "#0072B2") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "AIREA Job Posts Over Time — Santa Barbara City College",
    x = NULL,
    y = "AIREA job posts"
  ) +
  theme_minimal(base_size = 13)

plot_df <- demand_ds %>%
  filter(cz_label == "Knoxville, TN", year != 2025) %>%
  collect() %>% 
  filter(!is.na(soc_title)) %>% 
  filter(airea == 1) %>% 
  # use the latest year present in this subset
  select(soc_title, ed_req, total_job_postings) %>%
  mutate(
    # ed_req is <dbl+lbl>; turn labels into factor for a nice legend
    ed_req = haven::as_factor(ed_req, levels = "labels"),
    ed_req = fct_rev(ed_req),                         # reverse legend order (optional)
    ed_req = fct_na_value_to_level(ed_req, "Missing")       # make NAs visible if any
  ) %>%
  group_by(soc_title, ed_req) %>%
  summarise(total_postings = sum(total_job_postings, na.rm = TRUE), .groups = "drop") %>%
  # keep top N occupations by total postings
  group_by(soc_title) %>%
  mutate(total_soc = sum(total_postings)) %>%
  ungroup() %>%
  slice_max(order_by = total_soc, n = 20, with_ties = FALSE) %>%
  mutate(
    # order bars by total postings
    soc_title = fct_reorder(soc_title, total_soc)
  )

ggplot(plot_df, aes(x = soc_title, y = total_postings, fill = ed_req)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(
    x = NULL,
    y = "Number of job postings",
    fill = "Education requirement",
    title = "Education requirement share by occupation"
  ) +
  theme_bw() +
  theme(legend.position = "top") +
  scale_fill_brewer("", palette = "Set1") +
  theme(legend.position = "top") +
  guides(fill = guide_legend(nrow = 3, byrow = TRUE))
