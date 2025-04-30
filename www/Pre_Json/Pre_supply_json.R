library(dplyr)
library(sf)
library(geojsonio)

# Ensure longitude and latitude fields are present
# Use "longitud" and "latitude" as coordinate columns and convert to an sf object
institutes_sf <- ccrc_cip_comp %>%
  filter(!is.na(longitud), !is.na(latitude)) %>%
  st_as_sf(coords = c("longitud", "latitude"), crs = 4326)

# If desired, select only the needed fields: year, institution name (instnm), and green completion percentage (inst_perc_green_tot)
institutes_sf1 <- institutes_sf %>%
  select(year, instnm, inst_perc_green_tot, inst_green_cmplt_tot)

# Define the list of years to export (e.g., 2010 through 2013)
years <- sort(unique(ccrc_cip_comp$year))

# Loop over each year, filter the data, and write out a GeoJSON file
for (yr in years) {
  subset_data <- institutes_sf1 %>%
    filter(year == yr)
  
  # Build the output filename, e.g., "InstituteData_2010.json"
  filename <- paste0("InstituteData_", yr, ".json")
  
  # Export the filtered data as GeoJSON
  geojson_write(subset_data, file = filename)
  message("Saved file: ", filename)
}
