### this is for supply/demand ratio heat map

## read cz_sf_new2
#cz_sf_new2 <- readRDS("data/cz_sf_new2.rds")

## read mappsupply
#mappsupply <- readRDS("data/mappsupply.rds")

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

