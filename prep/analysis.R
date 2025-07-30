# ─────────────────────────────────────────────────────────────────────────────
# 0. Load libraries
# ─────────────────────────────────────────────────────────────────────────────
library(tidyverse)
library(haven)
library(gghighlight)
library(tigris)
library(tidycensus)
library(sf)

# ─────────────────────────────────────────────────────────────────────────────
# 1. Read in your data
# ─────────────────────────────────────────────────────────────────────────────
onet <- read_dta("prep/greenjobs4.dta")
d   <- read_csv("prep/lightcast-soc-year-county-2025-02-24 7_23pm.csv")

# Mark green jobs
d <- d %>%
    mutate(green = if_else(SOC_CODE %in% onet$soc, 1, 0))

# ─────────────────────────────────────────────────────────────────────────────
# 2. Read in the CZ shapefile (county → CZ) WITHOUT geometry (for later joins)
# ─────────────────────────────────────────────────────────────────────────────
sff <- sf::read_sf("CommutingZones2020_County_GIS_files/county20.shp") %>%
    select(COUNTY = GEOID, CZ = CZ20) %>%
    mutate(COUNTY = as.integer(COUNTY)) %>%
    as_tibble() %>%
    select(-geometry)

# We also keep a second copy (sfcz) that still has geometry (for the spatial join).
sfcz <- sf::read_sf("CommutingZones2020_County_GIS_files/county20.shp")

# ─────────────────────────────────────────────────────────────────────────────
# 3. Read in all US cities (from Geonames) and split Coordinates into lat/lon
# ─────────────────────────────────────────────────────────────────────────────
cities <- read_delim("geonames-all-cities-with-a-population-1000.csv",
                     delim = ";", quote = "\"", trim_ws = TRUE)

cities <- cities %>% 
    select(
        Name,
        `Alternate Names`,
        Coordinates,
        Population,
        State = `Admin1 Code`,   # two‐letter state code e.g. "TX"
        `Admin2 Code`,            # county name (optional)
        County = `Admin3 Code`    # county FIPS code as string, zero‐padded
    ) %>% 
    separate(Coordinates, into = c("lat", "lon"), sep = ",") %>% 
    mutate(
        lon = str_trim(lon),
        lat = str_trim(lat)
    )  

# ─────────────────────────────────────────────────────────────────────────────
# 4. Prepare county geometry (2020) as an sf object with FIPS → integer
# ─────────────────────────────────────────────────────────────────────────────
counties_sf <- counties(year = 2020, class = "sf") %>%
    transmute(
        COUNTY   = as.integer(GEOID),    # "01001" → 1001
        geometry = geometry
    )

# ─────────────────────────────────────────────────────────────────────────────
# 5. Make sure sfcz (the CZ polygons) is in the same CRS as our city points
#    (We will create cities_sf next, so let’s just assume EPSG:4326 for now.)
# ─────────────────────────────────────────────────────────────────────────────
# First we’ll build cities_sf (so we know its CRS = 4326) then reproject sfcz.

cities_sf <- 
    cities %>%
    mutate(
        lat = as.numeric(lat),
        lon = as.numeric(lon)
    ) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326)

# Now transform sfcz to EPSG:4326 so it matches cities_sf:
sfcz <- 
    sfcz %>%
    st_transform(crs = st_crs(cities_sf)) %>%
    rename(COUNTY = GEOID) %>%
    mutate(COUNTY = as.integer(COUNTY))

# (Optional check: both are now EPSG:4326)
# st_crs(cities_sf)
# st_crs(sfcz)

# ─────────────────────────────────────────────────────────────────────────────
# 6. Spatially join each city (point) to its county→CZ polygon:
# ─────────────────────────────────────────────────────────────────────────────
cities_with_cz <- 
    st_join(
        cities_sf,
        sfcz %>% select(COUNTY, CZ20),
        join = st_within
    )

cities_with_cz_tbl <- 
    cities_with_cz %>%
    as_tibble() %>%
    select(-geometry)

# Check for any NAs in CZ20 (i.e. city not assigned to any polygon)
cities_with_cz_tbl %>% filter(is.na(CZ20)) %>% glimpse()

# ─────────────────────────────────────────────────────────────────────────────
# NEW BLOCK: “Top 3 + Threshold” logic for CZ labelling
# ─────────────────────────────────────────────────────────────────────────────

library(dplyr)

# (A) For each CZ, pull out up to the top 3 largest‐population cities
top3_per_cz <- 
    cities_with_cz_tbl %>%
    filter(!is.na(CZ20)) %>%
    group_by(CZ20) %>%
    # slice_max(n = 3) picks the three highest‐population cities (ties = FALSE to break ties)
    slice_max(order_by = Population, n = 3, with_ties = FALSE) %>%
    arrange(CZ20, desc(Population)) %>%
    summarise(
        # 1st city
        first_city   = Name[1],
        first_pop    = Population[1],
        first_state  = State[1],
        
        # 2nd city (if it exists; else NA)
        second_city  = ifelse(n() >= 2, Name[2], NA_character_),
        second_pop   = ifelse(n() >= 2, Population[2], NA_real_),
        second_state = ifelse(n() >= 2, State[2], NA_character_),
        
        # 3rd city (if it exists; else NA)
        third_city   = ifelse(n() >= 3, Name[3], NA_character_),
        third_pop    = ifelse(n() >= 3, Population[3], NA_real_),
        third_state  = ifelse(n() >= 3, State[3], NA_character_),
        
        .groups = "drop"
    )

# (B) Define your “closeness” threshold (e.g. within 10% of the top city)
threshold <- 0.10

# (C) Build the CZ label, checking 1st vs. 2nd vs. 3rd:
cz_name_lookup <- 
    top3_per_cz %>%
    mutate(
        CZ_label = case_when(
            # 1) Only one city in this CZ:
            is.na(second_pop) ~ 
                paste0(first_city, ", ", first_state, " CZ"),
            
            # 2) Three cities exist AND all three are within threshold of the largest:
            !is.na(third_pop) &
                (first_pop - second_pop) / first_pop < threshold &
                (first_pop - third_pop) / first_pop  < threshold ~ 
                paste0(first_city, "-", second_city, "-", third_city, " CZ"),
            
            # 3) Two exist AND both 1st vs. 2nd are within threshold (but no 3rd OR 3rd is too small):
            (first_pop - second_pop) / first_pop < threshold ~ 
                paste0(first_city, "-", second_city, " CZ"),
            
            # 4) Otherwise, default to just the 1st city:
            TRUE ~ 
                paste0(first_city, ", ", first_state, " CZ")
        )
    ) %>%
    transmute(
        CZ20,
        city_name = first_city,
        State     = first_state,
        CZ_label
    )

# Inspect a few rows to check the new labels
cz_name_lookup %>% slice(1:10) %>% print(n = 10)

# ─────────────────────────────────────────────────────────────────────────────
# 8. Join the new labels back onto your CZ‐list (sff) or onto cities
# ─────────────────────────────────────────────────────────────────────────────

names_for_czs <- 
    sff %>% 
    distinct(CZ) %>% 
    left_join(rename(cz_name_lookup, CZ = CZ20), by = "CZ") %>% 
    mutate(
        CZ_label = ifelse(
            is.na(city_name), 
            str_c("CZ #", CZ),    # fallback if no city was ever assigned
            CZ_label
        )
    )

# View the final CZ‐names
names_for_czs %>% 
    arrange(CZ) %>% 
    print(n = 20)

names_for_czs %>% 
    write_csv("names-for-czs.csv")



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
    select(CZ, everything())



cz_tw <- d %>%
    filter(green == 1) %>%
    group_by(COUNTY, COUNTY_NAME, YEAR) %>%
    summarize(TOTAL_GREEN_JOB_POSTS = sum(as.integer(JOB_POSTING_COUNT))) %>%
    left_join(sff) %>%
    ungroup() %>%
    select(CZ, everything())

cz_tw %>% 
    write_csv("cz-postings-by-year.csv")

d %>%
    group_by(COUNTY, YEAR, green) %>% 
    summarize(JOB_POSTING_COUNT = sum(as.integer(JOB_POSTING_COUNT))) %>%
    left_join(sff) %>%
    select(-COUNTY) %>% 
    ungroup() %>% 
    group_by(CZ, YEAR, green) %>% 
    summarize(TOTAL_GREEN_JOB_POSTS = sum(JOB_POSTING_COUNT)) %>% 
    select(CZ, everything()) %>% 
    spread(green, TOTAL_GREEN_JOB_POSTS) %>% 
    rename(not_green = `0`, green = `1`) %>% 
    ungroup() %>% 
    mutate(total = not_green + green) %>% 
    select(CZ, YEAR, green, not_green, total) %>% 
    mutate(propotion_green = green / total) %>% 
    write_csv("cz-year-green-total.csv")
   

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


highlighted_CZs <- c(254, 37, 10, 190)
cz_labels <- c("254" = "Greater Boston", "37" = "Greater Los Angeles", "10" = "Greater Huntsville", "190" = "Greater Omaha")

p_CZ <- d %>%
    filter(green == 1, YEAR != 2025) %>%
    group_by(COUNTY, YEAR) %>%
    summarize(TOTAL_POSTS = sum(as.integer(JOB_POSTING_COUNT)), .groups = "drop") %>%
    left_join(sff, by = "COUNTY") %>%
    filter(!is.na(CZ)) %>%
    mutate(CZ = as.integer(CZ)) %>%
    group_by(CZ, YEAR) %>%
    summarize(TOTAL_POSTS = sum(TOTAL_POSTS), .groups = "drop") %>%
    mutate(CZ_label = cz_labels[as.character(CZ)]) %>%
    ggplot(aes(x = YEAR, y = TOTAL_POSTS, group = CZ)) +
    geom_line(alpha = 0.5, linewidth = .75) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(2010, 2024, 1)) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_brewer(palette = "Set1") +
    labs(
        title = "Green Jobs Postings By Select Commuting Zones",
        caption = "Source: Lightcast postings data",
        color = "Commuting Zone"
    ) +
    xlab(NULL) +
    ylab(NULL)

p_CZ %>% plotly::ggplotly()

ggsave("green-jobs-by-cz.pdf", width = 7, height = 5, dpi = 300, units = "in")

mean_data <- d %>%
    filter(YEAR != 2025) %>% 
    group_by(YEAR, green) %>% 
    summarize(sum_n = sum(as.integer(JOB_POSTING_COUNT))) %>% 
    mutate(sum_n = sum_n / 121) %>% # 121 SOCs
    filter(green == 1) %>% 
    ungroup()

# d %>% 
#     filter(YEAR != 2025) %>% 
#     filter(green == 1) %>% 
#     count(SOC_CODE)

library(ggplot2)
library(plotly)

# your main data → p_SOC
d_SOC <- d %>%
    filter(green == 1, YEAR != 2025) %>%
    group_by(SOC_CODE, YEAR) %>%
    summarize(TOTAL_POSTS = sum(JOB_POSTING_COUNT), .groups = "drop") %>%
    left_join(rename(onet, SOC_CODE = soc))

saveRDS(d_SOC, "d_SOC.rds")

pp_SOC <- d_SOC %>% 
    ggplot(aes(
        x      = YEAR,
        y      = TOTAL_POSTS,
        group  = Occupation,
        colour = Occupation,
        text   = paste0(
            "Year: ", YEAR, "<br>",
            "Total Posts: ", scales::comma(TOTAL_POSTS), "<br>",
            "Occupation: ", Occupation
        )
    )) +
    geom_line() +
    scale_x_continuous(breaks = 2010:2024) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(
        title   = "Green Jobs Postings By SOC Code",
        caption = "Source: Lightcast postings data",
        x       = NULL,
        y       = NULL
    ) +
    
    # Now add your overall‐mean line:
    geom_line(
        data = mean_data,
        inherit.aes = FALSE,
        aes(
            x    = YEAR,
            y    = sum_n,
            text = paste0(
                "Year: ", YEAR, "<br>",
                "Average Posts: ", scales::comma(sum_n)
            )
        ),
        colour   = "black",
        linewidth = 1.2
    ) +
    theme(legend.position = "none")

saveRDS(p_SOC, "p_SOC.rds")

p_SOC
# Finally, convert with only the `text` tooltip:
plotly::ggplotly(p_SOC, tooltip = "text")


ggsave("green-jobs-by-soc.pdf", width = 7, height = 5, dpi = 300, units = "in")

create_small_plot <- function(d, cz, cz_name, my_lim, my_set) {
    d %>% 
        filter(green == 1) %>% 
        filter(YEAR != 2025) %>% 
        group_by(SOC_CODE, YEAR, COUNTY) %>% 
        summarize(TOTAL_POSTS = sum(JOB_POSTING_COUNT), .groups = "drop") %>% 
        left_join(sff, by = "COUNTY") %>%
        left_join(rename(onet, SOC_CODE = soc)) %>% 
        group_by(YEAR, CZ, Occupation) %>% 
        summarize(TOTAL_POSTS = sum(TOTAL_POSTS), .groups = "drop") %>% 
        filter(CZ == cz) %>% 
        ggplot(aes(x = YEAR, y = TOTAL_POSTS, group = Occupation, color = Occupation)) +
        geom_line(alpha = 0.5, linewidth = .75) +
        gghighlight(max(TOTAL_POSTS) > my_lim, use_direct_label = TRUE) +
        theme_minimal() +
        scale_x_continuous(breaks = seq(2010, 2024, 2)) +
        xlab(NULL) +
        labs(subtitle = cz_name) +
        ylab(NULL) +
        # labs(caption = "Source: Lightcast postings data") +
        scale_y_continuous(labels = scales::comma) +
        scale_color_manual(values = my_set) +
        theme(legend.position = "bottom")
}

# highlighted_CZs <- c(254, 37, 10, 190)
# cz_labels <- c("254" = "Greater Boston", "37" = "Greater Los Angeles", "10" = "Greater Huntsville", "190" = "Greater Omaha")

p1 <- create_small_plot(d, 37, "Greater Los Angeles", 13250, c("#1B9E77", "#D95F02" ,"#7570B3"))
p2 <- create_small_plot(d, 254, "Greater Boston", 5500, c("#E7298A","#66A61E", "#E6AB02"))
p3 <- create_small_plot(d, 10, "Greater Huntsville", 1300, c("#A6761D", "#666666", "#1B9E77"))
p4 <- create_small_plot(d, 190, "Greater Omaha", 1050, c("#1B9E77", "#D95F02", "#7570B3"))

library(patchwork)

p1 + p2 + p3 + p4 + plot_layout(ncol = 2) + plot_annotation(title = "Green Jobs Postings By SOC Code")

ggsave("soc-by-cz.pdf", width = 10, height = 10, dpi = 300, units = "in")

# ggsave("green-jobs-by-soc.pdf", width = 7, height = 5, dpi = 300, units = "in")


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
