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
library(shinythemes)
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
#CZ_job_post <- CZ_job_post %>% 
 # mutate(id = row_number())


# ============================================================
# User Interface (UI) Design
# ============================================================
# ============================================================
# Updated UI: Use Tab Panels for Different Pages
# ============================================================

ui <- fluidPage(theme = shinytheme("cerulean"),
  # tags$head(includeCSS("www/style.css")),
  tags$style(HTML("
    #map { 
      visibility: hidden;
      width: 100%;
    }
  ")),
  titlePanel("CCRC Green Seek"),
  
  # year selector
  wellPanel(
    selectInput("selected_year", "Select Year:",
                choices = sort(unique(CZ_job_post$YEAR), decreasing = TRUE),
                selected = max(CZ_job_post$YEAR))
  ),
  
  # tabsetPanel specify id=“tabs” and give each tab a value
  tabsetPanel(
    id = "tabs",
    # ----- Tab 1: Main Map -----
    tabPanel("Map",    value = "mainmap",   
             fluidRow(
               column(12,
                      wellPanel(
                        fluidRow(
                          column(8,
                                 selectizeInput("search_term", "Search by Institution:",
                                                choices = NULL, options = list(), width = "100%")
                          ),
                          column(4,
                                 div(style="margin-top:25px;",
                                     actionButton("search_btn","Search"),
                                     tags$button("Clear", onclick="clearMap()", class="btn btn-default")
                                 )
                          )
                        )
                      ),
                      div(id="map", style="height:700px; width:100%;")
               )
             )
    ),
    # ----- Tab 2: Supply -----
    tabPanel("Supply from Community Colleges", value = "treemap",
             fluidRow(
               column(12,
                      wellPanel(
                        plotlyOutput("treemapPlot", height = "600px")
                      )
               )
             )
    ),
    
    # ----- Tab 3: Demand -----
    tabPanel("Demand from Employers", value = "demand",
             fluidRow(
               column(6,
                      wellPanel(
                        plotlyOutput("cz_plot", height = "300px")
                      )
               ),
               column(6,
                      wellPanel(
                        plotlyOutput("trendPlot", height = "300px")
                      )
               )
             )
    ),

  ),
  
  
  # -------------------------------
  # Footer
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
  # Front-End Dependencies (Mapbox, Turf.js, Custom JS)
  # -------------------------------
  tags$head(
    tags$link(href = "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.css", rel = "stylesheet"),
    tags$script(src = "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.js"),
    tags$script(src = "mapbox.js"),
    tags$script(HTML(paste0("const mapboxToken = '", mapbox_token, "';"))),
    tags$script(src = "https://cdn.jsdelivr.net/npm/@turf/turf@6/turf.min.js")
  )
)





# ============================================================
# Server Logic
# ============================================================
server <- function(input, output, session) {
  
  # Send a custom message “resizeMap” when the user switches to the Main Map tab.
  observeEvent(input$tabs, {
    if (input$tabs == "mainmap") {
      session$sendCustomMessage("resizeMap", list())
    }
  })
  
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
        "<strong>Year:</strong> ", search_result$year, "<br>",
        "<strong>Green Degrees:</strong> ",
        format(search_result$inst_green_cmplt_tot, big.mark = ",", scientific = FALSE), "<br>",
        "<strong>Green Degree Rate:</strong> ",
        sprintf("%.1f%%", search_result$inst_perc_green_tot * 100)
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
  
  # JR added just as a placeholder until we get the data from Matias/Cameron
  # output$table <- renderTable( {
  #   
  #   read_rds("my-table.rds")
  #   
  # })
  
  output$cz_plot <- renderPlotly({
    readRDS("p_CZ_plotly.rds")
  })
}

# ============================================================
# Launch the Shiny Application
# ============================================================
shinyApp(ui, server)
