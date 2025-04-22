##### this is for app map plolty Pane pre plotly file.

library(tidyverse)
library(haven)
library(gghighlight)

onet <- read_dta("greenjobs4.dta")

View(onet)

## write out the onet data as rds data
write_rds(onet, "onet.rds")

d <- read_csv("lightcast-soc-year-county-2025-02-28.csv")

# Create a new variable using mutate and if_else
d <- d %>%
    mutate(green = if_else(SOC_CODE %in% onet$soc, 1, 0))

sff <- sf::read_sf("county20/county20.shp")

sff <- sff %>% 
    select(COUNTY = GEOID, CZ = CZ20) %>% 
    mutate(COUNTY = as.integer(COUNTY)) %>% 
    as_tibble() %>% 
    select(-geometry)

#    select(COUNTY, COUNTY_NAME, YEAR, SOC_CODE, GREEN = green, JOB_POSTING_COUNT)
#    write_csv("lightcast-soc-year-county-2025-02-28.csv")
dd <- d %>% 
    group_by(YEAR, green) %>% 
    summarize(sum_n = sum(as.integer(JOB_POSTING_COUNT))) %>% 
    filter(YEAR != 2025)

dd %>%
    pivot_wider(names_from = green, values_from = sum_n) %>%
    mutate(prop = `1` / `0`) %>%
    select(YEAR, prop) %>%
    ggplot(aes(x = YEAR, y = prop)) +
    geom_line(color = "gray") +
    geom_point(color = "gray") +
    theme_bw() +
    scale_x_continuous(breaks = seq(2010, 2024, 1)) +
    xlab(NULL) +
    ylab("Proportion of Green Jobs")


dd_c <- d %>% 
    group_by(YEAR, green, COUNTY) %>% 
    summarize(sum_n = sum(as.integer(JOB_POSTING_COUNT))) %>% 
    filter(YEAR != 2025)

# dd_c <- dd_c %>% 
#     ungroup() %>% 
#     left_join(sff) %>% 
#     group_by(CZ, green, YEAR) %>% 
#     summarize(sum_n = sum(sum_n)) %>% 
#     ungroup() %>% 
#     rename(cz = CZ, is_green_soc = green, year = YEAR, num_postings = sum_n)
# 
# dd_c
# 
# dd_c %>% 
#     write_csv("cz_postings-2025-03-02.csv")

d %>%
    filter(green == 1) %>%
    group_by(COUNTY, COUNTY_NAME) %>%
    summarize(TOTAL_GREEN_JOB_POSTS = sum(as.integer(JOB_POSTING_COUNT))) %>%
    left_join(sff) %>%
    ungroup() %>%
    select(CZ, everything()) %>%
    write_csv("cz_postings.csv")



dd_c %>% 
    filter(!is.na(sum_n)) %>% 
    pivot_wider(names_from = green, values_from = sum_n) %>%
    rename(not_green = `0`, green = `1`) %>%
    mutate(prop = green / (not_green + green)) %>% 
    group_by(YEAR) %>% 
    summarize(mean_prop = mean(prop, na.rm = TRUE))

total_jobs <- dd_c %>% 
    group_by(YEAR) %>% 
    summarize(total_jobs = sum(sum_n))
    
dd_c %>% 
    mutate(green = as.factor(green)) %>%
    group_by(green, YEAR) %>% 
    filter(green == 1) %>% 
    summarize(sum_n = sum(sum_n)) %>%
    left_join(total_jobs) %>% 
    ungroup() %>% 
    select(-green) %>% 
    gather(key, val, -YEAR) %>% 
    ggplot(aes(x = YEAR, y = val, group = key, color = key)) +
    geom_line(linewidth = 1) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(2010, 2024, 1)) +
    xlab(NULL) +
    ggtitle("Jobs Postings Over Time") +
    ylab(NULL) +
    labs(caption = "Source: Lightcast postings data grouped by green and non-green SOC codes") +
    scale_color_manual(
        name = NULL, 
        values = c("darkgreen", "gray"), 
        labels = c("Green Jobs", "All Jobs")
    ) +
    scale_y_continuous(labels = scales::comma)

ggsave("jobs-over-time.pdf", width = 7, height = 5, dpi = 300, units = "in")

dd_c %>% 
    mutate(green = as.factor(green)) %>%
    group_by(green, YEAR) %>% 
    filter(green == 1) %>% 
    summarize(sum_n = sum(sum_n)) %>% 
    ggplot(aes(x = YEAR, y = sum_n)) +
    geom_line(linewidth = 1, color = "darkgreen") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(2010, 2024, 1)) +
    xlab(NULL) +
    ggtitle("Green Jobs Postings Over Time") +
    ylab(NULL) +
    labs(caption = "Source: Lightcast postings data grouped by green and non-green SOC codes") +
    scale_color_manual(
        name = NULL, 
        values = c("darkgreen"), 
        labels = c("Green Job")
    ) +
    scale_y_continuous(labels = scales::comma)

ggsave("only-green-jobs-over-time.pdf", width = 7, height = 5, dpi = 300, units = "in")


p_CZ <- d %>% 
    filter(green == 1) %>% 
    filter(YEAR != 2025) %>% 
    group_by(COUNTY, YEAR) %>%
    summarize(TOTAL_POSTS = sum(as.integer(JOB_POSTING_COUNT)), .groups = "drop") %>% 
    left_join(sff) %>% 
    filter(!is.na(CZ)) %>% 
    mutate(CZ = as.character(CZ)) %>% 
    group_by(CZ, YEAR) %>% 
    summarize(TOTAL_POSTS = sum(TOTAL_POSTS), .groups = "drop") %>% 
    ungroup() %>% 
    ggplot(aes(x = YEAR, y = TOTAL_POSTS, color = CZ)) +
    geom_line(alpha = 0.5, linewidth = .75) +
    gghighlight(max(TOTAL_POSTS) > 125000, use_direct_label = FALSE) +  # Highlight selected CZs
    theme_minimal() +
    scale_x_continuous(breaks = seq(2010, 2024, 1)) +
    xlab(NULL) +
    ggtitle("Green Jobs Postings By Select Commuting Zones") +
    ylab(NULL) +
    labs(caption = "Source: Lightcast postings data") +
    scale_y_continuous(labels = scales::comma) +
    scale_color_brewer("Commuting Zone", palette = "Set1")

p_CZ
p_CZ_plotly <- ggplotly(p_CZ)
p_CZ_plotly
saveRDS(p_CZ_plotly, "p_CZ_plotly.rds")

ggsave("green-jobs-by-cz.pdf", width = 7, height = 5, dpi = 300, units = "in")

p_SOC <- d %>% 
    filter(green == 1) %>% 
    filter(YEAR != 2025) %>% 
    group_by(SOC_CODE, YEAR) %>% 
    summarize(TOTAL_POSTS = sum(JOB_POSTING_COUNT), .groups = "drop") %>% 
    left_join(rename(onet, SOC_CODE = soc)) %>% 
    ggplot(aes(x = YEAR, y = TOTAL_POSTS, color = Occupation)) +
    geom_line(alpha = 0.5, linewidth = .75) +
    gghighlight(max(TOTAL_POSTS) > 210000, use_direct_label = TRUE) +  # Highlight selected CZs
    theme_minimal() +
    scale_x_continuous(breaks = seq(2010, 2024, 1)) +
    xlab(NULL) +
    ggtitle("Green Jobs Postings By SOC Code") +
    ylab(NULL) +
    labs(caption = "Source: Lightcast postings data") +
    scale_y_continuous(labels = scales::comma) +
    scale_color_brewer(palette = "Set1") +
    theme(legend.position = "bottom")

p_SOC

library(plotly)
p_SOC_plotly <- ggplotly(p_SOC)
p_SOC_plotly
saveRDS(p_SOC_plotly, "p_SOC_plotly.rds")



ggsave("green-jobs-by-soc.pdf", width = 7, height = 5, dpi = 300, units = "in")

# 
# p1 <- dd_c %>%
#     pivot_wider(names_from = green, values_from = sum_n) %>%
#     rename(not_green = `0`, green = `1`) %>%
#     mutate(prop = green / (not_green + green)) %>%
#     ggplot(aes(x = YEAR, y = prop, group = CZ, color = CZ == target_CZ)) +
#     geom_line(aes(alpha = CZ == target_CZ)) +  # Adjust transparency
#     geom_smooth(aes(group = 1), color = "darkgreen", se = FALSE) +  # Overall trend line
#     theme_minimal() +
#     scale_x_continuous(breaks = NULL) +
#     xlab(NULL) +
#     ylab("Proportion of Green Jobs") +
#     ggtitle("Green Jobs Postings Over Time") +
#     labs(subtitle = "Lines represent the proportion of jobs coded as green in a commuting zone") +
#     scale_color_manual(values = c("gray", "yellow")) +  # Gray for background, red for highlight
#     scale_alpha_manual(values = c(0.1, 1)) +  # Transparent background, solid highlight
#     theme(legend.position = "none")  # Hide legend if not needed
# 
# 
# p2 <- dd_c %>% 
#     mutate(green = as.factor(green)) %>% 
#     ggplot(aes(x = YEAR, y = sum_n, fill = green)) +
#     geom_col() +
#     theme_minimal() +
#     scale_x_continuous(breaks = seq(2010, 2024, 1)) +
#     xlab(NULL) +
#     ylab("Proportion of Green Jobs") +
#     labs(caption = "Source: Lightcast postings data grouped by green and non-green SOC codes") +
#     scale_fill_manual("Green Job Status", values = c("gray", "darkgreen")) +
#     scale_y_continuous(labels = scales::comma) +
#     theme(legend.position = "none") +
#     annotate("text", x = 2014, y = 8500000, label = "Green Jobs", color = "darkgreen", size = 5, fontface = "bold")
# 
# library(gghighlight)
# library(patchwork)
# 
# p1 + p2 + plot_layout(ncol = 1)
# 
# ggsave("cz.png", height = 9, width = 7, units = "in", dpi = 300)
# 
# dd_c %>%
#     filter(green == 1) %>% 
#     ggplot(aes(x = YEAR, y = sum_n, group = CZ)) +
#     # geom_point(color = "gray", alpha = 0.01) +  # Make points slightly transparent
#     geom_line(color = "gray", alpha = 0.1) +  # Lower alpha for density effect
#     geom_smooth(aes(group = 1), color = "darkgreen", se = FALSE) +  # Mean trend line
#     theme_minimal() +
#     scale_x_continuous(breaks = seq(2010, 2024, 1)) +
#     xlab(NULL) +
#     ylab("Proportion of Green Jobs") +
#     ggtitle("By Commuting Zones") +
#     labs(subtitle = "Lines represent the propotion of jobs coded as green in a commuting zone") +
#     labs(caption = "Source: Lightcast postings data grouped by green and non-green SOC codes")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# dd_c %>%
#     pivot_wider(names_from = green, values_from = sum_n) %>%
#     rename(not_green = `0`, green = `1`) %>%
#     mutate(prop = green / (not_green + green)) %>%
#     ggplot(aes(x = YEAR, y = prop, group = COUNTY)) +
#     # geom_point(color = "gray", alpha = 0.01) +  # Make points slightly transparent
#     geom_line(color = "gray", alpha = 0.1) +  # Lower alpha for density effect
#     geom_smooth(aes(group = 1), color = "black", se = FALSE) +  # Mean trend line
#     theme_bw() +
#     scale_x_continuous(breaks = seq(2010, 2024, 1)) +
#     xlab(NULL) +
#     ylab("Proportion of Green Jobs")
# 
# library(ggridges)
# 
# dd_c %>%
#     pivot_wider(names_from = green, values_from = sum_n) %>%
#     rename(not_green = `0`, green = `1`) %>%
#     mutate(prop = green / (not_green + green)) %>%
#     ggplot(aes(x = prop, y = factor(YEAR))) +
#     geom_density_ridges(alpha = 0.6, scale = 1) +
#     theme_minimal() +
#     xlab("Proportion of Green Jobs") +
#     ylab("Year") +
#     theme(legend.position = "none")
# 
# dd_c %>%
#     pivot_wider(names_from = green, values_from = sum_n) %>%
#     rename(not_green = `0`, green = `1`) %>%
#     mutate(prop = green / (not_green + green)) %>%
#     ggplot(aes(x = factor(YEAR), y = prop)) +
#     geom_boxplot(outlier.color = "red", fill = "gray", alpha = 0.5) +
#     theme_minimal() +
#     xlab("Year") +
#     ylab("Proportion of Green Jobs") +
#     coord_flip()  # Flip for better readability if needed
# 
# dd_m <- dd_c %>%
#     pivot_wider(names_from = green, values_from = sum_n) %>%
#     rename(not_green = `0`, green = `1`) %>%
#     mutate(prop = green / (not_green + green))
# 
# dd_m
# 
# m1 <- lmer(prop ~ YEAR + (1 | COUNTY), data = dd_m)
# summary(m1)
# 
# dd_m$YEAR_scaled <- scale(dd_m$YEAR, center = TRUE, scale = TRUE)
# 
# model <- glmer(
#     cbind(green, not_green) ~ 1 + (1 | COUNTY),
#     data = dd_m,
#     family = binomial,
#     control = glmerControl(optimizer = "bobyqa")
# )
# 
# summary(model)
# 
# ranefs <- ranef(model) %>% 
#     as_tibble() %>% 
#     arrange(desc(condval)) %>% 
#     rename(COUNTY = grp) %>% 
#     select(COUNTY, condval)
# 
# d %>%
#     distinct(COUNTY, COUNTY_NAME) %>% 
#     mutate(COUNTY = as.factor(COUNTY)) %>%
#     right_join(ranefs) %>% 
#     arrange(desc(condval))



