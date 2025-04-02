library(shiny)
library(jsonlite)
library(geojsonio)
library(dplyr)
library(sf)
library(ggplot2)
library(scales)
library(plotly)
source("mapboxtoken_setup.R")  # This file defines the mapbox_token variable

####### Supply Data (for institution search)
hdallyears <- readRDS("hdallyears.rds")
ipeds_green_summed <- readRDS("ipeds_green_summed.rds")
# Merge data for institution search
hdallyears_joined <- hdallyears %>%
  left_join(ipeds_green_summed, by = "unitid")

####### Demand Data: Commuting Zone job postings
# Load the commuting zone data (CZ_job_post.rds should include CZ20, YEAR, green_job_postings, and geometry)
CZ_job_post <- readRDS("CZ_job_post.rds")
# Ensure the CZ data has unique IDs for Mapbox feature-state
CZ_job_post <- CZ_job_post %>%
  mutate(id = row_number())

# Load the trend data (trend_data.rds should include YEAR and total_green)
trend_data <- readRDS("trend_data.rds")

#### Plot Pane data



ui <- fluidPage(
  # Set initial CSS style: hide the map until fully loaded
  tags$style(HTML("
    #map { 
      visibility: hidden;
    }
  ")),
  titlePanel("CCRC Green Job Seek Mapping (Commuting Zones)"),
  
  # Layout: Left column for filters; right column for search controls and map container
  fluidRow(
    column(3,
           wellPanel(
             # Year selector for filtering CZ data
             selectInput("selected_year",
                         "Select Year:",
                         choices = sort(unique(CZ_job_post$YEAR),decreasing = TRUE),
                         selected = max(CZ_job_post$YEAR)),
             # Existing supply category selector (optional)
             selectInput("selected_green_category",
                         "Select Supply Category:",
                         choices = c("Green New & Emerging", 
                                     "Green Enhanced Skills", 
                                     "Green Increased Demand")),
             # Filter: Climate Degree
             selectInput("climate_degree",
                         "Filter: Climate Degree",
                         choices = c("All Degrees", 
                                     "Energy & Climate Connected Degree", 
                                     "Non-Energy & Climate Connected Degree")),
             # Filter: Career Cluster
             selectInput("career_cluster",
                         "Filter: Career Cluster",
                         choices = c("All Programs",
                                     "Advanced Manufacturing",
                                     "Agriculture",
                                     "Arts, Entertainment & Design",
                                     "Construction",
                                     "Digital Technology",
                                     "Education",
                                     "Energy & Natural Resources",
                                     "Financial Services",
                                     "Healthcare & Human Services",
                                     "Hospitality, Events & Tourism",
                                     "Management & Entrepreneurship",
                                     "Marketing & Sales",
                                     "Public Service & Safety",
                                     "Supply Chain & Transportation")),
             # Filter: Entry Education Level
             selectInput("entry_education",
                         "Filter: Entry Education Level",
                         choices = c("Less than BA", "BA and Higher"))
           )
    ),
    column(9,
           wellPanel(
             fluidRow(
               column(8,
                      textInput("search_term", "Search by Institution:",
                                placeholder = "Type institution here...", width = "100%")
               ),
               column(4,
                      div(style = "margin-top: 25px;",
                          actionButton("search_btn", "Search"),
                          tags$button("Clear", onclick = "clearMap()", 
                                      style = "margin-left: 10px;", class = "btn btn-default")
                      )
               )
             )
           ),
           div(id = "map", style = "height:700px;")
    )
  ),
  
  # New pane: left is the CZ plot, right is the trend plot
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
  ),
  
  # Footer
  fluidRow(
    column(12, align = "center",
           tags$footer(
             style = "margin-top: 20px; padding: 10px; font-size: 12px; background-color: #f8f9fa; border-top: 1px solid #e9ecef;",
             HTML("Created by Wei Wang, Joshua Rosenberg, Cameron Sublet and Bret Staudt Willet with the Community College Research Center at Teachers College, Columbia.<br>Source code at: <a href='https://github.com/wwang93/CCRC_Mapping_JS' target='_blank'>GitHub</a>.<br>Thanks to funding from JC Morgan Chase.")
           )
    )
  ),
  
  tags$head(
    # Include Mapbox GL JS CSS and JS library
    tags$link(href = "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.css", rel = "stylesheet"),
    tags$script(src = "https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.js"),
    # Include the custom JS file (which loads static data on the front end)
    tags$script(src = "mapbox.js"),
    # Pass the mapboxToken variable to the front end
    tags$script(HTML(paste0("const mapboxToken = '", mapbox_token, "';"))),
    # Load Turf.js library (if needed)
    tags$script(src = "https://cdn.jsdelivr.net/npm/@turf/turf@6/turf.min.js")
  )
)

server <- function(input, output, session) {
  # When the selected year changes, send the selected year to the front end.
  observe({
    req(input$selected_year)
    # Send a custom message "loadYear" with the selected year
    session$sendCustomMessage("loadYear", input$selected_year)
  })
  
  
  # Institution search: Filter data by institution name and supply category (optional)
  observeEvent(input$search_btn, {
    req(input$search_term)
    req(input$selected_green_category)
    
    # Filter data by institution name (instnm) and supply category (greencat)
    search_result <- hdallyears_joined %>%
      filter(grepl(input$search_term, instnm, ignore.case = TRUE),
             greencat == input$selected_green_category) %>%
      head(1)
    
    if (nrow(search_result) > 0) {
      # Construct the popup content: display the institution name, selected category, and the size value
      popup_text <- paste0(
        "<strong>", search_result$instnm, "</strong><br>",
        "Category: ", input$selected_green_category, "<br>",
        "Size: ", search_result$size
      )
      coords <- list(
        lng = search_result$longitud,
        lat = search_result$latitude,
        popup = popup_text
      )
      # Send search results to the front end
      session$sendCustomMessage(type = "updateSearch", coords)
    } else {
      showNotification("No Institution Found!", type = "error")
    }
  })
  
  # Render trend plot for green job postings from 2010 to 2024
  output$trendPlot <- renderPlotly({
    readRDS("p_SOC_plotly.rds")
  })
  output$cz_plot <-renderPlotly({
    readRDS("p_CZ_plotly.rds")
  })
}

shinyApp(ui, server)
