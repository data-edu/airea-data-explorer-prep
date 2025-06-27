README
================
Wei Wang
2025-04-22

# AIREA Data Explorer

A Shiny web application for interactive mapping of Advanced Infrastructure, Energy, and Agriculture (AIREA) job postings across U.S. Commuting Zones (CZs).

## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Installation](#installation)
- [Data Preparation](#data-preparation)
- [Usage](#usage)
- [File Structure](#file-structure)
- [Dependencies](#dependencies)
- [Contributing](#contributing)
- [License](#license)
- [Authors](#authors)

## Overview

This project provides an interactive dashboard to visualize the
distribution and trends of green job postings by U.S. Commuting Zone. It
leverages: - **Mapbox GL JS** for dynamic mapping - **Shiny** for the
web interface - **GeoJSON** for spatial data - **Plotly** for
interactive charts

## Features

- **Choropleth Map**: Displays green job postings intensity per CZ
- **Year Selector**: Filter data from 2010 through the latest available
  year
- **Hover & Popup**: View detailed CZ metrics on hover and click
- **Trend Plot**: See national green job posting trends over time
- **CZ Plot**: View CZ-specific job postings trend when selected
- **Institution Search**: Locate and highlight a specific institution on
  the map

## Installation

1.  **Clone the repository**:

    ``` bash
    git clone https://github.com/data-edu/CCRC_GreenSeek-Mapping.git
    cd CCRC_Mapping_JS
    ```

2.  **Install R dependencies**:

    ``` r
    install.packages(c("shiny", "jsonlite", "geojsonio", "dplyr", "sf", "ggplot2", "plotly"))
    install.packages(c("rnaturalearth", "rnaturalearthdata")) # For mask polygon script
    ```

3.  **Set up Mapbox token**:

    - Add to your .Renviron the following:
      `MAPBOX_ACCESS_TOKEN=YOUR ACCESS TOKEN`
      
      Where YOUR ACCESS TOKEN is copied from https://console.mapbox.com/
      
4. **Download Private Data**
    
    - Presently, we are storing the required private data in a [Sharepoint folder]. 
    We plan to make this public in the future. This folder contains:
      - `cz_job_post.rds`

## Data Preparation

The repository includes helper scripts to generate required GeoJSON
files: - **prepare_mask_polygon.R**: Fetches U.S. state boundaries and
creates a global mask polygon (world minus U.S.), outputting
`mask_polygon.geojson`. - **pre_cz jason.R**: Reads
`CZ_jobpostings_final1` and writes yearly CZ GeoJSON files
(`CZData_<YEAR>.json`).

Place the generated files in the `www/` folder:

    www/
    ├── mask_polygon.geojson
    ├── CZData_2010.json
    ├── CZData_2011.json
    └── ...

Ensure the following data files exist in the app root: -
`CZ_job_post.rds` (with columns: CZ20, YEAR, green_job_postings,
geometry, id) - `P_CZ_plotly.rds/p_SOC_plotly.rds` (with columns: YEAR,
total_green)

## Usage

Launch the Shiny app by running in R:

``` r
shiny::runApp("app.R")
```

Or open `app.R` in RStudio and click **Run App**.

## File Structure

    ├── mapbox.js                           # Mapbox initialization and layer logic
    ├── app.R                               # Shiny UI & server code
    ├── prepare_mask_polygon.R              # Script to generate mask_polygon.geojson
    ├── pre_cz jason.R                      # Script to generate CZData_<YEAR>.json files
    ├── mapboxtoken_setup.R                 # (Not committed) Defines mapbox_token
    ├── CZ_job_post.rds                     # CZ job postings data
    ├── P_CZ_plotly.rds/p_SOC_plotly.rds    # National green job posting trends
    ├── www/                                # Static assets
    │   ├── style.css
    │   ├── mask_polygon.geojson
    │   ├── CZData_2010.json
    │   └── ...
    └── README.md                  # Project documentation

## Dependencies

- **R packages**: `shiny`, `jsonlite`, `geojsonio`, `dplyr`, `sf`,
  `ggplot2`, `plotly`, `rnaturalearth`, `rnaturalearthdata`
- **JavaScript libraries**: Mapbox GL JS v2.14.1, Turf.js
- **Data**: U.S. Commuting Zone boundaries, green job postings dataset

## How to deploy

- First, run `deploy-setup.R`
- Then, deploy only the contents of the `app` folder

## Contributing

Contributions are welcome! Please open an issue or submit a pull
request.

## Authors

- Wei Wang
- Joshua Rosenberg
- Cameron Sublet
- Bret Staudt Willet

*Built with the Community College Research Center at Teachers College,
Columbia University. Funded by JPMorgan Chase.*
