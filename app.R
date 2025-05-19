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
# Define the UI for the Shiny application using fluidPage layout.

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(
    tags$style(HTML("
      #desc-box {
        background-color: rgba(255,255,255,0.6) !important;
        margin-top: 20px;
        padding: 10px;
        border-radius: 4px;
      }
      #map { visibility: hidden; width: 100%; }
    "))
  ),
  
  titlePanel("CCRC Green Seek"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Controls shown only on the Map tab
      conditionalPanel(
        "input.tabs == 'mainmap'",
        selectInput("selected_year_map", "Select Year:",
                    choices = sort(unique(CZ_job_post$YEAR), decreasing = TRUE),
                    selected = max(CZ_job_post$YEAR)),
        selectInput("cz_metric", "Color by:", 
                    choices = c(
                      "Total green job postings"      = "green_job_posting",
                      "Pct. of postings that are green" = "pct_green",
                      "Green jobs per 1,000 residents" = "per1000"
                    )),
        selectizeInput("search_term", "Search by Institution:",
                       choices = NULL, options = list(), width="100%"),
        fluidRow(
          column(6, actionButton("search_btn","Search", class="btn-primary", width="100%")),
          column(6, tags$button("Clear", onclick="clearMap()", class="btn btn-default", style="width:100%;"))
        ),
        wellPanel(id="desc-box",
                  "This interactive map shows how community college green program completions align with green-sector job postings by commuting zone. ",
                  tags$br(),
                  "Use the controls above to change year, color metric, or locate a specific institution."
        )
      ),
      
      # Controls shown only on the Supply tab
      conditionalPanel(
        "input.tabs == 'treemap'",
        selectInput("selected_year_supply", "Select Year:",
                    choices = sort(unique(CZ_job_post$YEAR), decreasing = TRUE),
                    selected = max(CZ_job_post$YEAR))
      )
      
      # No controls needed on the Demand tab
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs", type = "tabs",
        
        tabPanel("Map", value = "mainmap",
                 div(id="map", style="height:700px;width:100%;")
        ),
        
        tabPanel("Supply from Community Colleges", value = "treemap",
                 plotlyOutput("treemapPlot", height="600px")
        ),
        
        tabPanel("Demand from Employers", value = "demand",
                 fluidRow(
                   column(6, plotlyOutput("cz_plot", height="300px")),
                   column(6, plotlyOutput("trendPlot", height="300px"))
                 )
        )
      )
    )
  ),
  
  # -------------------------------
  # Footer
  # -------------------------------
  fluidRow(
    column(12, align = "center",
           tags$footer(
             style = "margin-top: 20px; padding: 10px; font-size: 12px; background-color: #f8f9fa; border-top: 1px solid #e9ecef;",
             HTML("Created by Wei Wang, Joshua Rosenberg, Cameron Sublet and Bret Staudt Willet with the Community College Research Center at Teachers College, Columbia.<br>Source code at: <a href='https://github.com/data-edu/CCRC_GreenSeek-Mapping' target='_blank'>GitHub</a>.<br>Thanks to funding from JC Morgan Chase.")
           )
    )
  ),
  
  
  # Mapbox & JS Include Mapbox GL JS and custom scripts
  tags$link(rel="stylesheet", 
            href="https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.css"),
  tags$script(src="https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.js"),
  tags$script(src="mapbox.js"),
  tags$script(HTML(paste0("const mapboxToken = '", mapbox_token, "';"))),
  tags$script(src="https://cdn.jsdelivr.net/npm/@turf/turf@6/turf.min.js")
)



# ============================================================
# Server Logic
# ============================================================
server <- function(input, output, session) {
  
  observeEvent(input$cz_metric, {
    session$sendCustomMessage("updateCZMetric", input$cz_metric)
  })
  
  
  # --- 1. Map tab's year selection & search ---
  observe({
    req(input$selected_year_map)
    
    # Update institution dropdown for the selected year
    inst_choices <- sort(
      unique(ccrc_cip_comp$instnm[ccrc_cip_comp$year == input$selected_year_map])
    )
    updateSelectizeInput(
      session, "search_term",
      choices = inst_choices,
      server  = TRUE
    )
    
    # After UI updates, trigger data load in JS
    session$onFlushed(function() {
      session$sendCustomMessage(
        "loadInstituteYear",
        isolate(input$selected_year_map)
      )
    }, once = TRUE)
    
    # Notify frontend to load CZ data and redraw map
    session$sendCustomMessage("loadYear", input$selected_year_map)
  })
  # Handle institution search button click
  observeEvent(input$search_btn, {
    req(input$search_term)
    req(input$selected_year_map)
    
    search_result <- ccrc_cip_comp %>%
      filter(
        grepl(input$search_term, instnm, ignore.case = TRUE),
        year == input$selected_year_map
      ) %>%
      slice(1)
    
    if (nrow(search_result) > 0) {
      popup_text <- paste0(
        "<strong>", search_result$instnm, "</strong><br>",
        "<strong>Year:</strong> ", search_result$year, "<br>",
        "<strong>Green Degrees:</strong> ",
        format(search_result$inst_green_cmplt_tot, big.mark = ",", scientific = FALSE), "<br>",
        "<strong>Green Degree Rate:</strong> ",
        sprintf("%.1f%%", search_result$inst_perc_green_tot * 100)
      )
      coords <- list(
        lng   = search_result$longitud,
        lat   = search_result$latitude,
        popup = popup_text
      )
      session$sendCustomMessage("updateSearch", coords)
    } else {
      showNotification("No Institution Found This Year!", type = "error")
    }
  })
  
  # --- 2. Year selection on Supply tab ---
  treemap_list <- readRDS("Green_degree_treemap_plotly_list.rds")
  
  output$treemapPlot <- renderPlotly({
    req(input$selected_year_supply)
    treemap_list[[ as.character(input$selected_year_supply) ]]
  })
  
  # --- 3. Demand tab ---
  output$trendPlot <- renderPlotly({
    readRDS("p_SOC_plotly.rds")
  })
  
  output$cz_plot <- renderPlotly({
    readRDS("p_CZ_plotly.rds")
  })
  
  # --- 4. Force map resize when switching to Map tab ---
  observeEvent(input$tabs, {
    if (input$tabs == "mainmap") {
      session$sendCustomMessage("resizeMap", list())
    }
  })
  
  
  
  
  
  
  # -------------------------------
  # Render Treemap Plot (Plotly)
  # -------------------------------
  # Load a list of pre-generated treemap plots from an RDS file.
  treemap_list <- readRDS("Green_degree_treemap_plotly_list.rds")
  
  output$treemapPlot <- renderPlotly({
    req(input$selected_year_supply)  # Ensure the selected year is available
    # Extract the treemap plot corresponding to the selected year.
    treemap_list[[as.character(input$selected_year_supply)]]
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
