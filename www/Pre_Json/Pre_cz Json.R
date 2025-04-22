library(dplyr)
library(geojsonio)

# get all years
years <- sort(unique(CZ_jobpostings_final1$YEAR))

# get data for each year and save as GeoJSON
for (yr in years) {
  subset_data <- CZ_jobpostings_final1 %>%
    filter(YEAR == yr)
  filename <- paste0("CZData_", yr, ".json")
  geojson_write(subset_data, file = filename)
  message("Saved file: ", filename)
}


cz_0302_geo_sf1
unique(cz_0302_geo_sf1$YEAR)


library(dplyr)
library(geojsonio)

# get all years
years <- sort(unique(cz_0302_geo_sf1$YEAR))

# get data for each year and save as GeoJSON
for (yr in years) {
  subset_data <- cz_0302_geo_sf1 %>%
    filter(YEAR == yr)%>% mutate(id = row_number())
  filename <- paste0("CZData_", yr, ".json")
  geojson_write(subset_data, file = filename)
  message("Saved file: ", filename)
}