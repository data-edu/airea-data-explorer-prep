library(dplyr)
library(geojsonio)

# 1. Ensure cz_sf1 is an sf object containing geometry, YEAR, green_job_postings, pct_green, and per1000

# 2. Extract all unique years
years <- sort(unique(cz_sf1$YEAR))

# 3. Export a GeoJSON file for each year, including green_job_postings, pct_green, and per1000
for (yr in years) {
  # Subset data for the current year and add a unique feature ID (optional)
  subset_data <- cz_sf1 %>%
    filter(YEAR == yr) %>%
    mutate(id = row_number())
  
  # Define the output filename
  filename <- paste0("CZData_", yr, ".json")
  
  # Write the subset to GeoJSON
  geojson_write(subset_data, file = filename)
  
  # Print a message confirming the file was saved
  message("Saved file: ", filename)
}
