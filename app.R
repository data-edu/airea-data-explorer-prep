# ============================================================
# Load Required Libraries and Initialize Settings
# ============================================================

library(shiny)         # Shiny framework for interactive web applications
library(jsonlite)      # For processing JSON data
library(geojsonio)     # For reading and writing GeoJSON data
library(dplyr)         # Data manipulation and transformation
library(sf)            # For handling spatial data
library(ggplot2)       # Data visualization
library(scales)        # Formatting for plots
library(plotly)        # For interactive plotting
source("mapboxtoken_setup.R")  # Loads mapbox_token used for Mapbox access

# ============================================================
# Load Supply Data (for Institution Search)
# ============================================================
# Read supply data from an RDS file; this data is used for filtering/searching institutions.
ccrc_cip_comp <- readRDS("ccrc_cip_comp.rds")


# ============================================================
# Load Demand Data: Commuting Zone Job Postings
# ============================================================
# Read the Commuting Zone (CZ) job postings data. This RDS file should contain fields:
# - CZ20: Commuting zone identifier
# - YEAR: Year of the data
# - green_job_postings: Number of green job postings in that CZ
# - geometry: Spatial information for mapping
CZ_job_post <- readRDS("CZ_job_post.rds")
# Ensure each commuting zone feature gets a unique ID (for Mapbox feature-state management like hover events)
CZ_job_post <- CZ_job_post %>% 
  mutate(id = row_number())


# ============================================================
# User Interface (UI) Design
# ============================================================
ui <- fluidPage(
  
  # -------------------------------
  # Page Style: Include Custom CSS and Hide the Map Until Fully Loaded
  # -------------------------------
  tags$head( includeCSS("www/style.css") ),
  tags$style(HTML("
    #map { 
      visibility: hidden;
    }
  ")),
  
  # Page Title
  titlePanel("CCRC Green Seek"),
  
  # -------------------------------
  # Layout: Using fluidRow to Divide the Page into Left and Right Columns
  # Left Column (width = 4): Contains the year selector and treemap plot
  # Right Column (width = 8): Contains the institution search controls and map container
  # -------------------------------
  fluidRow(
    # Left Column: Filters and Treemap Plot
    column(4,
           wellPanel(
             # Year Selector: Filter CZ data based on year; choices sorted in descending order.
             selectInput("selected_year",
                         "Select Year:",
                         choices = sort(unique(CZ_job_post$YEAR), decreasing = TRUE),
                         selected = max(CZ_job_post$YEAR))
           ),
           wellPanel(
             # Display the Treemap Plot directly below the year selector.
             # This plot (a plotly object) is pre-generated and will be updated based on the selected year.
             plotlyOutput("treemapPlot", height = "600px")
           )
    ),
    # Right Column: Institution Search Controls and Map
    column(8,
           wellPanel(
             fluidRow(
               # Search Input: A selectizeInput for institution names.
               # Initially, choices are empty and will be updated in the server.
               column(8,
                      selectizeInput("search_term", "Search by Institution:",
                                     choices = NULL,  # Initially empty; to be updated dynamically by the server.
                                     options = list(
                                       placeholder = "Type institution name...",
                                       openOnFocus = FALSE,
                                       maxOptions = 10
                                     ),
                                     width = "100%")
               ),
               # Search Button and Clear Button: User clicks to trigger search or clear the map.
               column(4,
                      div(style = "margin-top: 25px;",
                          actionButton("search_btn", "Search"),
                          tags$button("Clear", onclick = "clearMap()", 
                                      style = "margin-left: 10px;", class = "btn btn-default")
                      )
               )
             )
           ),
           # Map Container: The map will render inside this div (ID "map"), with a height of 700px.
           div(id = "map", style = "height:700px;")
    )
  ),
  
  # -------------------------------
  # Additional Plot Panels: CZ Plot and Trend Plot
  # -------------------------------
  fluidRow(
    # Left Pane: CZ Plot showing commuting zone data
    column(6,
           wellPanel(
             plotlyOutput("cz_plot", height = "300px")
           )
    ),
    # Right Pane: Trend Plot showing green job postings trends (from 2010 to 2024)
    column(6,
           wellPanel(
             plotlyOutput("trendPlot", height = "300px")
           )
    )
  ),
  
  # -------------------------------
  # Footer: Display Credits, Source Code Link, and Acknowledgment
  # -------------------------------
  fluidRow(
    column(12, align = "center",
           tags$footer(
             style = "margin-top: 20px; padding: 10px; font-size: 12px; background-color: #f8f9fa; border-top: 1px solid #e9ecef;",
             HTML("Created by Wei Wang, Joshua Rosenberg, Cameron Sublet and Bret Staudt Willet with the Community College Research Center at Teachers College, Columbia.<br>Source code at: <a href='https://github.com/wwang93/CCRC_Mapping_JS' target='_blank'>GitHub</a>.<br>Thanks to funding from JC Morgan Chase.")
           )
    )
  ),
  
  # -------------------------------
  # Include Front-End Dependencies (Mapbox GL JS, Turf.js, Custom JS)
  # -------------------------------
  tags$head(
    # Include Mapbox GL JS CSS file for styling the map.
    tags$link(href = "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.css", rel = "stylesheet"),
    # Include Mapbox GL JS library.
    tags$script(src = "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.js"),
    # Include the custom JS file that loads static data and handles front-end interactions.
    tags$script(src = "mapbox.js"),
    # Pass the mapbox_token from R to the front-end as a global variable (mapboxToken).
    tags$script(HTML(paste0("const mapboxToken = '", mapbox_token, "';"))),
    # Include Turf.js library for geographic processing on the front-end (if needed).
    tags$script(src = "https://cdn.jsdelivr.net/npm/@turf/turf@6/turf.min.js")
  )
)


# ============================================================
# Server Logic
# ============================================================
server <- function(input, output, session) {
  
  # -------------------------------
  # Respond to Year Selection Changes
  # -------------------------------
  # When the selected year changes:
  #   - Update the institution search dropdown based on the chosen year.
  #   - Notify the front-end to load the corresponding Institute data.
  #   - Send a message to load the corresponding Commuting Zone data.
  observe({
    req(input$selected_year)
    
    # Extract institution names from supply data for the selected year and sort them.
    inst_choices <- sort(unique(ccrc_cip_comp$instnm[ccrc_cip_comp$year == input$selected_year]))
    
    # Dynamically update the selectizeInput with the institution choices.
    updateSelectizeInput(session, "search_term", choices = inst_choices, server = TRUE)
    
    # Use onFlushed to send a message to load institution data once the UI is fully updated.
    session$onFlushed(function() {
      session$sendCustomMessage("loadInstituteYear", isolate(input$selected_year))
    }, once = TRUE)
    
    # Send the selected year to the front-end to load the corresponding Commuting Zone data.
    session$sendCustomMessage("loadYear", input$selected_year)
  })
  
  
  # -------------------------------
  # Institution Search Logic
  # -------------------------------
  # When the search button is clicked:
  #   - Validate the input.
  #   - Filter the supply data based on the institution name (ignoring case) and selected year.
  #   - If a match is found, construct a popup content and send the coordinates to the front-end for display.
  observeEvent(input$search_btn, {
    req(input$search_term)
    req(input$selected_year)
    
    search_result <- ccrc_cip_comp %>%
      filter(grepl(input$search_term, instnm, ignore.case = TRUE),
             year == input$selected_year) %>%
      head(1)  # Take only the first match
    
    if (nrow(search_result) > 0) {
      # Construct HTML content for the popup including institution name, year, and formatted green completion percentage.
      popup_text <- paste0(
        "<strong>", search_result$instnm, "</strong><br>",
        "Year: ", search_result$year, "<br>",
        "Total Community College Green Completion Percentage: ", sprintf("%.1f%%", search_result$inst_perc_green_tot * 100)
      )
      # Create a list with longitude, latitude, and the popup HTML content.
      coords <- list(
        lng = search_result$longitud,
        lat = search_result$latitude,
        popup = popup_text
      )
      # Send a custom message to the front-end with the search results to display a popup and fly to that location.
      session$sendCustomMessage(type = "updateSearch", coords)
    } else {
      # Show an error notification if no matching institution is found.
      showNotification("No Institution Found This Year!", type = "error")
    }
  })
  
  
  # -------------------------------
  # Render Treemap Plot (Plotly)
  # -------------------------------
  # Load a list of pre-generated treemap plots from an RDS file.
  treemap_list <- readRDS("Green_degree_treemap_plotly_list.rds")
  
  output$treemapPlot <- renderPlotly({
    req(input$selected_year)
    # Extract the treemap plot corresponding to the selected year.
    treemap_list[[as.character(input$selected_year)]]
  })
  
  
  # -------------------------------
  # Render Trend Plot and Commuting Zone (CZ) Plot
  # -------------------------------
  # These plots are pre-generated Plotly objects stored in RDS files and are rendered in their respective output panels.
  output$trendPlot <- renderPlotly({
    readRDS("p_SOC_plotly.rds")
  })
  
  output$cz_plot <- renderPlotly({
    readRDS("p_CZ_plotly.rds")
  })
}

# ============================================================
# Launch the Shiny Application
# ============================================================
shinyApp(ui, server)
