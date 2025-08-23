library(tidyverse)
library(haven)

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

supply_table

supply %>% 
  filter(instnm == "Adirondack Community College") %>% # input$selected_institution
  group_by(year) %>% 
  summarize(total_completions = sum(total_completions, na.rm = TRUE),
            total_students_enrolled = sum(total_students_enrolled, na.rm = TRUE),
            total_airea_completions = sum(airea_completions, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = total_completions)) +
  geom_point() +
  geom_line() +
  theme_bw() +
  scale_x_continuous(breaks = seq(2010, 2022, 1)) +
  xlab(NULL)

supply %>% 
  filter(year == 2020,
         str_detect(instnm, "Adirondack Community College")) %>% 
  group_by(ciptitle, award_level) %>% 
  summarize(total_airea_completions = sum(airea_completions, na.rm = TRUE),
            .groups = "drop") %>% 
  filter(total_airea_completions > 0) %>%
  mutate(
    # Use the value labels as factor levels in the legend
    award_level = haven::as_factor(award_level, levels = "labels")
    # If you prefer "code – label", use: levels = "both"
  ) %>% 
  ggplot(aes(x = reorder(ciptitle, total_airea_completions),
             y = total_airea_completions,
             fill = award_level)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  labs(y = "Total AIREA Completions", x = NULL, fill = "Award level") +
  scale_fill_brewer("", palette = "Set1") +
  theme(legend.position = "top")
  
demand <- read_parquet("prep/demand.parquet.gzip")
demand
