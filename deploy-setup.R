# ============================================================
# Deployment Setup Script for AIREA Data Explorer
# ============================================================
# This script creates a clean app folder with only the files needed 
# for deployment to avoid the 1GB size limit

# Create app directory
if (dir.exists("app")) {
  unlink("app", recursive = TRUE)
}
dir.create("app")
dir.create("app/prep")

# ============================================================
# Copy core application files
# ============================================================
print("Copying core application files...")

# Core Shiny files
file.copy("ui.R", "app/ui.R")
file.copy("server.R", "app/server.R") 
file.copy("mapboxtoken_setup.R", "app/mapboxtoken_setup.R")
file.copy("custom-style.css", "app/custom-style.css")

# Core mapbox token file
file.copy(".Renviron", "app/.Renviron")

# ============================================================
# Copy essential data files
# ============================================================
print("Copying essential data files...")

# Main data files
file.copy("CZ_job_post.rds", "app/CZ_job_post.rds")
file.copy("scatter_plot_data.rds", "app/scatter_plot_data.rds")
file.copy("Green_degree_treemap_plotly_list.rds", "app/Green_degree_treemap_plotly_list.rds")
file.copy("ccrc_cip_comp.rds", "app/ccrc_cip_comp.rds")

# Prep folder - only essential files
file.copy("prep/supply-institutions-raw-data.rds", "app/prep/supply-institutions-raw-data.rds")
file.copy("prep/demand-jobs-raw-data.rds", "app/prep/demand-jobs-raw-data.rds")
file.copy("prep/camssoc&ciplist.dta", "app/prep/camssoc&ciplist.dta")

# ============================================================
# Copy www folder (needed for map functionality)
# ============================================================
print("Copying www folder...")
file.copy("www", "app", recursive = TRUE)

# ============================================================
# Check final size
# ============================================================
print("Checking final app size...")
system("du -sh app")

print("")
print("Deployment setup complete!")
print("The 'app' folder contains all files needed for deployment.")
print("")
print("To deploy:")
print("1. cd into the 'app' folder")
print("2. Run your deployment command from there")
print("3. All file paths in the Shiny app are relative, so they should work correctly")
print("")
print("Setup complete! Your app folder is ready for deployment.") 