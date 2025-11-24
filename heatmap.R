### this is for supply/demand ratio heat map

## read cz_sf_new2
cz_sf_new2 <- readRDS("prep/cz_sf_new2.rds")

## read mappsupply
mapsupply <- readRDS("prep/mapsupply.rds")

str(mapsupply)
str(cz_sf_new2)





library(dplyr)
library(sf)
library(ggplot2)
library(scales)

# -----------------------------
# 1. Supply aggregation (mapsupply)
# -----------------------------
# mapsupply structure:
# year, inst_id, instnm, cz_label, airea_completions

supply_cz <- mapsupply %>%
  group_by(cz_label) %>%
  summarise(
    supply_total = sum(airea_completions, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# 2. Demand aggregation (cz_sf_new2)
# -----------------------------
# cz_sf_new2 structure:
# CZ20, YEAR, airea_job_posting, cz_label, geometry

demand_cz <- cz_sf_new2 %>%
  group_by(CZ20, cz_label) %>% 
  summarise(
    demand_total = sum(airea_job_posting, na.rm = TRUE),
    geometry = st_union(geometry),
    .groups = "drop"
  ) %>%
  st_as_sf()

# -----------------------------
# 3. Merge supply & demand
# -----------------------------
cz_joined <- demand_cz %>%
  left_join(supply_cz, by = "cz_label") %>%
  mutate(
    ratio = supply_total / demand_total,
    ratio = ifelse(is.infinite(ratio), NA, ratio),  # 处理 demand=0
    ratio = ifelse(demand_total == 0, NA, ratio)    # 没有 demand 的也设 NA
  )

# -----------------------------
# 4. mapping
# -----------------------------
library(dplyr)
library(sf)
library(ggplot2)

# =======================
# 1. Define breaks: <1 finer, >1 unified
# =======================
breaks <- c(0, 0.25, 0.5, 1, Inf)
labels <- c("0–0.25", "0.25–0.5", "0.5–1", ">1")

cz_joined$ratio_binned <- cut(
  cz_joined$ratio,
  breaks = breaks,
  labels = labels,
  include.lowest = TRUE,
  right = FALSE
)

# =======================
# 2. Color palette
# =======================
palette_values <- c(
  "0–0.25" = "#ffffcc",
  "0.25–0.5" = "#ffeda0",
  "0.5–1" = "#feb24c",
  ">1" = "#f03b20"      # unified color for >1
)

# =======================
# 3. Plot, removing Hawaii via x-limits
# =======================
p <- ggplot(cz_joined) +
  geom_sf(aes(fill = ratio_binned), color = "gray60", size = 0.1) +
  scale_fill_manual(
    values = palette_values,
    drop = FALSE,
    na.value = "white",
    name = "Supply/Demand Ratio"
  ) +
  coord_sf(
    xlim = c(-125, -66),  # Cut out Hawaii and focus on continental US
    ylim = c(24, 50)
  ) +
  labs(
    title = "AIREA Alignment: CZ-level Supply/Demand Ratio (2010–2023)",
    subtitle = "CZs without community colleges are shown in white"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 14)
  )

print(p)


library(classInt)

# Remove NA to compute breaks
vals <- cz_joined$ratio[!is.na(cz_joined$ratio)]

# Compute Jenks breaks
jenks <- classIntervals(vals, n = 5, style = "jenks")

# Break values:
jenks$brks





library(dplyr)
library(sf)
library(ggplot2)
library(classInt)
library(RColorBrewer)

# ================================
# 1. using Jenks breaks
# ================================
breaks <- jenks$brks  
breaks

# create breaks labels
labels <- paste0(
  format(round(breaks[-length(breaks)], 2), nsmall = 2),
  " – ",
  format(round(breaks[-1], 2), nsmall = 2)
)

# ================================
# 2. Cut into bins
# ================================
cz_joined$ratio_binned <- cut(
  cz_joined$ratio,
  breaks = breaks,
  labels = labels,
  include.lowest = TRUE,
  right = FALSE
)

# ================================
# 3. Choose palette
# ================================
palette_values <- brewer.pal(6, "YlOrRd")

# ================================
# 4. Plot map (removing Hawaii)
# ================================
p <- ggplot(cz_joined) +
  geom_sf(aes(fill = ratio_binned), color = "gray60", size = 0.1) +
  scale_fill_manual(
    values = palette_values,
    drop = FALSE,
    na.value = "white",
    name = "AIREA Supply/Demand Ratio"
  ) +
  coord_sf(
    xlim = c(-125, -66),   #remove Hawaii
    ylim = c(24, 50)
  ) +
  labs(
    title = "AIREA Alignment: CZ-level Supply/Demand Ratio (2010–2023)",
    caption = "CZs without community colleges are shown in white"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 14)
  )

print(p)

## save the P as Png file
#ggsave("cz_supply_demand_ratio_heatmap.png", plot = p, width = 10, height = 6, dpi = 300)
ggsave("cz_supply_demand_ratio_heatmap.png", p,
       width = 11, height = 7.5, dpi = 300)




#### updates: CCRC blue

library(dplyr)
library(sf)
library(ggplot2)
library(classInt)

# ================================
# 1. using Jenks breaks
# ================================
breaks <- jenks$brks

labels <- paste0(
  format(round(breaks[-length(breaks)], 2), nsmall = 2),
  " – ",
  format(round(breaks[-1], 2), nsmall = 2)
)

# ================================
# 2. Cut into bins
# ================================
cz_joined$ratio_binned <- cut(
  cz_joined$ratio,
  breaks = breaks,
  labels = labels,
  include.lowest = TRUE,
  right = FALSE
)

# ================================
# 3. CCRC Blue Theme Colors
# ================================
palette_values <- c(
  "#e0f2ff",  # very light blue
  "#b3ddf6",  # soft blue
  "#66b2d9",  # medium blue
  "#0065A4",  # CCRC blue
  "#003f6b"   # deep navy
)

# ================================
# 4. Plot map (remove Hawaii)
# ================================
p1 <- ggplot(cz_joined) +
  geom_sf(aes(fill = ratio_binned), color = "gray60", size = 0.1) +
  scale_fill_manual(
    values = palette_values,
    drop = FALSE,
    na.value = "white",
    name = "AIREA Supply/Demand Ratio"   # <<<<<<<< updated legend title
  ) +
  coord_sf(
    xlim = c(-125, -66),
    ylim = c(24, 50)
  ) +
  labs(
    title = "AIREA Alignment: CZ-level Supply/Demand Ratio (2010–2023)",
    caption = "CZs without community colleges are shown in white"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    legend.position = "right",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 14)
  )

print(p1)
## save the P1 as Png file
ggsave("cz_supply_demand_ratio_heatmap_ccrc_blue.png", plot = p1, width = 11, height = 7.5, dpi = 300)
