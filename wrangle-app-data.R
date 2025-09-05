library(tidyverse)
library(haven)
library(arrow)
library(scales)

supply <- read_parquet("prep/supply.parquet.gzip")

supply %>% 
  count(award_level)

supply %>% 
  glimpse()

supply_nat_ave <- supply %>% 
  group_by(instnm, year) %>% 
  summarize(tot_completions = sum(total_completions, na.rm = TRUE),
            tot_airea_completions = sum(airea_completions, na.rm = TRUE),
            first_students_enrolled = first(total_students_enrolled)) %>% 
  group_by(year) %>%
  summarize(mean_completions = mean(tot_completions, na.rm = TRUE),
            mean_airea_completions = mean(tot_airea_completions, na.rm = TRUE),
            mean_students_enrolled = mean(first_students_enrolled, na.rm = TRUE)) %>% 
  mutate(
    pct_airea_completions = mean_airea_completions / mean_completions
  ) %>% 
  select(year, mean_airea_completions, pct_airea_completions)

write_csv(supply_nat_ave, "prep/supply-nat-ave.csv")

# soc <- read_dta("prep/full_soc.dta")

supply_table <- supply %>% 
  group_by(instnm, year) %>% 
  summarize(tot_completions = sum(total_completions, na.rm = TRUE),
            tot_airea_completions = sum(airea_completions, na.rm = TRUE),
            first_students_enrolled = first(total_students_enrolled),
            rural = first(rural),
            tribal = first(tribal),
            cz_label = first(cz_label)) %>% 
  # group_by(instnm) %>%
  # summarize(mean_completions = mean(tot_completions, na.rm = TRUE),
  #           mean_airea_completions = mean(tot_airea_completions, na.rm = TRUE),
  #           mean_students_enrolled = mean(first_students_enrolled, na.rm = TRUE),
  #           rural = first(rural),
  #           tribal = first(tribal),
  #           cz_label = first(cz_label)) %>% 
  mutate(pct_airea_completions = tot_airea_completions/tot_completions)

supply_table <- supply_table %>% 
  mutate(rural = if_else(rural == 1, "Rural", "Non-rural")) %>% 
  mutate(tribal = if_else(tribal == 1, "Tribal", "Non-tribal"))

write_csv(supply_table, "prep/supply-table-for-app.csv")

supply_table %>% 
  ungroup() %>% 
  count(rural)

supply_table %>% 
  ungroup() %>% 
  count(tribal)

write_csv(supply_table, "prep/supply-table.csv")

open_ds <- open_dataset("prep/supply_partitioned", format = "parquet")

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
  filter(instnm == "", year == 2012L) %>% 
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

demand_tab

library(dplyr)
library(stringr)
library(ggplot2)
library(tibble)

# 1) Lookup: 2-digit SOC -> short group label
soc_groups <- tribble(
  ~code, ~soc_group_short,
  "11","Management",
  "13","Business & Financial Ops",
  "15","Computer & Math",
  "17","Architecture & Engineering",
  "19","Life/Physical/Social Sci",
  "21","Community & Social Service",
  "23","Legal",
  "25","Education & Library",
  "27","Arts, Media & Sports",
  "29","Healthcare Practitioners & Tech",
  "31","Healthcare Support",
  "33","Protective Service",
  "35","Food Prep & Serving",
  "37","Building & Grounds Maint",
  "39","Personal Care & Service",
  "41","Sales & Related",
  "43","Office & Admin Support",
  "45","Farming/Fishing/Forestry",
  "47","Construction & Extraction",
  "49","Install/Maint/Repair",
  "51","Production",
  "53","Transportation & Material Moving"
)

library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(ggrepel)

#library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(ggrepel)

# Build df as you already do
df <- demand_tab %>%
  filter(airea == 1, year <= 2023) %>%
  mutate(
    soc_clean = str_replace_all(as.character(soc), "\\D", ""),
    soc2      = str_sub(str_pad(soc_clean, width = 6, side = "left", pad = "0"), 1, 2)
  ) %>%
  left_join(soc_groups, by = c("soc2" = "code")) %>%
  group_by(year, soc2, soc_group_short) %>%
  summarise(total_job_postings_sum = sum(total_job_postings, na.rm = TRUE), .groups = "drop") %>%
  tidyr::drop_na(soc_group_short)

last_year <- max(df$year, na.rm = TRUE)

# One point per group at the last year for labels
lab_df <- df %>%
  group_by(soc_group_short) %>%
  filter(year == max(year)) %>%
  slice_tail(n = 1) %>%
  ungroup()

ggplot(df, aes(x = year, y = total_job_postings_sum,
               color = soc_group_short, group = soc_group_short)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.6) +
  # Direct labels at the end of each line
  geom_text_repel(
    data = lab_df,
    aes(label = soc_group_short),
    direction = "y", hjust = 0, nudge_x = 0.35,
    size = 3, segment.alpha = 0.35, segment.size = 0.25,
    box.padding = 0.2, point.padding = 0.15,
    show.legend = FALSE
  ) +
  # Room for labels beyond last year + commas on y
  scale_x_continuous(limits = c(min(df$year), last_year + 0.8),
                     breaks = pretty_breaks(n = 10),
                     expand = expansion(mult = c(0.02, 0.15))) +
  scale_y_continuous(labels = comma) +
  scale_color_viridis_d(option = "plasma", end = 0.92) +
  labs(x = NULL, y = "Job Postings") +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 80, 5, 5) # room on right for labels
  ) +
  ggtitle("Job Postings by SOC Code Group")

ggsave("job postings by soc.png", width = 9, height = 6)


demand_tab %>% write_csv("demand-data-for-researchers.csv")

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

cz_year

cz_year %>% 
  group_by(year) %>% 
  summarize(
    mean_job_posts = mean(posts_total, na.rm = TRUE),
    mean_airea_posts = mean(posts_airea, na.rm = TRUE),
    pct_airea_posts = if_else(mean_job_posts > 0,
                          mean_airea_posts / mean_job_posts, NA_real_),
    mean_pop_year = mean(cz_pop_year, na.rm = TRUE),
    posts_per_1000 = if_else(mean_pop_year > 0,
                              (mean_job_posts / mean_pop_year) * 1000, NA_real_)
  ) %>% 
  write_csv("prep/demand-nat-ave.csv")

cz_year %>% 
  mutate(airea_pct_posts = if_else(posts_total > 0,
                              posts_airea / posts_total, NA_real_)) %>% 
  mutate(posts_per_1000 = if_else(cz_pop_year > 0,
                              (posts_total / cz_pop_year) * 1000, NA_real_)) %>% 
  write_csv("prep/demand-table-for-app.csv")

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

cz_summary

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

demand_ds <- open_dataset("prep/demand_partitioned", format = "parquet")
demand_ds

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
