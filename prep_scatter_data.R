# ============================================================
# Data Preparation for Scatter Plot: CZ-level AIREA Analysis
# ============================================================
# This script prepares data for a scatter plot comparing AIREA supply 
# (% of completions) vs AIREA demand (% of job posts) at the CZ level

library(dplyr)

# Load the data
supply <- readRDS("prep/supply-institutions-raw-data.rds")
demand <- readRDS("prep/demand-jobs-raw-data.rds")

# Load CZ mapping data (has institution to CZ mappings)
cz_mapping <- readRDS("ccrc_cip_comp.rds")

# ============================================================
# Create Institution to CZ Mapping
# ============================================================

# Create a unique mapping of institutions to CZs
inst_to_cz <- cz_mapping %>%
  select(instnm, CZ, CZ_label) %>%
  distinct()

print("Institution to CZ mapping created:")
print(paste("Number of unique institutions:", nrow(inst_to_cz)))

# ============================================================
# Prepare Supply Data (Aggregate institutions to CZ level)
# ============================================================

# Join supply data with CZ mapping
supply_with_cz <- supply %>%
  left_join(inst_to_cz, by = "instnm") %>%
  filter(!is.na(CZ), !is.na(CZ_label))  # Remove institutions without CZ mapping

print("Supply data with CZ mapping:")
print(paste("Institutions with CZ mapping:", nrow(supply_with_cz)))

# Aggregate supply data to CZ level (across all years)
supply_by_cz <- supply_with_cz %>%
  group_by(CZ, CZ_label) %>%
  summarise(
    total_completions = sum(inst_cmplt_tot, na.rm = TRUE),
    total_airea_completions = sum(inst_cmplt_tot * inst_perc_acea_tot, na.rm = TRUE),
    num_institutions = n_distinct(instnm),
    .groups = "drop"
  ) %>%
  mutate(
    airea_completion_percentage = (total_airea_completions / total_completions) * 100
  ) %>%
  filter(total_completions > 0)  # Remove CZs with no completions

# ============================================================
# Prepare Demand Data (Aggregate to CZ level)
# ============================================================

# Aggregate demand data to CZ level (exclude 2025, across all other years)
demand_by_cz <- demand %>%
  filter(YEAR <= 2024) %>%
  group_by(CZ_label) %>%
  summarise(
    total_posts = sum(TOTAL_JOB_POSTS, na.rm = TRUE),
    total_airea_posts = sum(TOTAL_JOB_POSTS[AIREA == 1], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    airea_posting_percentage = (total_airea_posts / total_posts) * 100
  ) %>%
  filter(total_posts > 0)  # Remove CZs with no posts

# ============================================================
# Combine Supply and Demand Data
# ============================================================

# Join supply and demand data by CZ
scatter_data <- supply_by_cz %>%
  inner_join(demand_by_cz, by = "CZ_label") %>%
  select(
    CZ,
    CZ_label,
    num_institutions,
    total_completions,
    total_airea_completions,
    airea_completion_percentage,
    total_posts,
    total_airea_posts,
    airea_posting_percentage
  ) %>%
  # Remove any CZs with missing or extreme values
  filter(
    !is.na(airea_completion_percentage),
    !is.na(airea_posting_percentage),
    airea_completion_percentage >= 0,
    airea_posting_percentage >= 0,
    airea_completion_percentage <= 100,
    airea_posting_percentage <= 100
  ) %>%
  # Add size categories for better visualization
  mutate(
    size_category = case_when(
      total_completions < 1000 ~ "Small (< 1,000)",
      total_completions < 5000 ~ "Medium (1,000 - 5,000)",
      TRUE ~ "Large (> 5,000)"
    )
  )

# ============================================================
# Save the prepared data
# ============================================================

# Save the scatter plot data
saveRDS(scatter_data, "scatter_plot_data.rds")

# Print summary statistics
print("Scatter plot data summary:")
print(paste("Number of CZs with both supply and demand data:", nrow(scatter_data)))
print("Supply (AIREA completion %) summary:")
print(summary(scatter_data$airea_completion_percentage))
print("Demand (AIREA posting %) summary:")
print(summary(scatter_data$airea_posting_percentage))

# Print first few rows
print("First few rows of scatter data:")
print(head(scatter_data))

print("Data preparation complete! Scatter plot data saved to 'scatter_plot_data.rds'") 