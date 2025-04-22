library(dplyr)

# Filter data for the year 2023
data_2023 <- ccrc_cip_comp %>% 
  filter(year == 2023)

# 2) Simple example: display only the first level without subdividing parent nodes
# To use CIP major categories, you need to define them yourself or classify based on the first two digits of the CIP code
treemap_data <- data_2023 %>%
  group_by(mfreq_green_cip_stitle1) %>%
  summarise(total_completions = sum(mfreq_green_cip_cmplt1, na.rm = TRUE)) %>%
  ungroup()

# 3) Specify the parent node (here we use a single layer, so set it uniformly as "All Green Programs" or another constant)
treemap_data <- treemap_data %>%
  mutate(
    label   = mfreq_green_cip_stitle1,
    parent  = "All Green Programs",  
    value   = total_completions
  )

library(plotly)

fig <- plot_ly(
  data = treemap_data,
  type = "treemap",
  labels = ~label,
  parents = ~parent,
  values = ~value,
  # Display percentages relative to parent nodes
  textinfo = "label+value+percent parent",
  # You can also set percentages relative to the root node: "label+value+percent root"
  branchvalues = "total"   # or "remainder"
)

fig


library(dplyr)
library(plotly)

# Assume the 'year' field in ccrc_cip_comp data represents the year
years <- sort(unique(ccrc_cip_comp$year))  # e.g., 2010:2023

# Create an empty list to store plotly figures for each year
plotly_list <- list()

for (yr in years) {
  # Filter data for the current year
  data_year <- ccrc_cip_comp %>% 
    filter(year == yr)
  
  # Aggregate by green-related program title
  treemap_data <- data_year %>%
    group_by(mfreq_green_cip_stitle1) %>%
    summarise(total_completions = sum(mfreq_green_cip_cmplt1, na.rm = TRUE)) %>%
    ungroup() %>%
    # Set a single-level treemap, with the parent node uniformly "All Green Programs"
    mutate(
      label  = mfreq_green_cip_stitle1,
      parent = "All Green Programs",
      value  = total_completions
    )
  
  # Generate the plotly treemap figure
  fig <- plot_ly(
    data = treemap_data,
    type = "treemap",
    labels = ~label,
    parents = ~parent,
    values = ~value,
    textinfo = "label+value+percent parent",
    branchvalues = "total"
  )
  
  # Store the figure in the list, using the year as the key
  plotly_list[[as.character(yr)]] <- fig
}

# Save the list of all plotly figures as an RDS file
saveRDS(plotly_list, "Green_degree_treemap_plotly_list.rds")

message("RDS file created: Green_degree_treemap_plotly_list.rds")


##### Top 10 + "Other" treemap

library(dplyr)
library(plotly)

# Assume the 'year' field in ccrc_cip_comp data represents the year
years <- sort(unique(ccrc_cip_comp$year))

# Create an empty list to store plotly figures for each year
plotly_list <- list()

for (yr in years) {
  # Filter data for the current year
  data_year <- ccrc_cip_comp %>% 
    filter(year == yr)
  
  # Aggregate by green-related program title and compute total completions
  aggregated_data <- data_year %>%
    group_by(mfreq_green_cip_stitle1) %>%
    summarise(total_completions = sum(mfreq_green_cip_cmplt1, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(total_completions))
  
  # If there are more than 10 categories, keep only the top 10 and combine the rest into "Other"
  if(nrow(aggregated_data) > 10) {
    top10 <- aggregated_data[1:10, ]
    others <- aggregated_data[-(1:10), ]
    others_sum <- sum(others$total_completions)
    treemap_data <- bind_rows(
      top10,
      tibble(mfreq_green_cip_stitle1 = "Other", total_completions = others_sum)
    )
  } else {
    treemap_data <- aggregated_data
  }
  
  # Set the necessary columns for treemap_data
  treemap_data <- treemap_data %>%
    mutate(
      label  = mfreq_green_cip_stitle1,
      parent = "All Green Programs",
      value  = total_completions
    )
  
  # Generate the plotly treemap figure
  fig <- plot_ly(
    data = treemap_data,
    type = "treemap",
    labels = ~label,
    parents = ~parent,
    values = ~value,
    textinfo = "label+value+percent parent",
    branchvalues = "total"
  )
  
  # Store the figure in the list, using the year as the key
  plotly_list[[as.character(yr)]] <- fig
}

# Save the list of all plotly figures as an RDS file
saveRDS(plotly_list, "Green_degree_treemap_plotly_list.rds")

message("RDS file created: Green_degree_treemap_plotly_list.rds")
