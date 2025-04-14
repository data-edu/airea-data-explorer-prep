library(shiny)
library(jsonlite)
library(geojsonio)
library(dplyr)
library(sf)
library(ggplot2)
library(scales)
library(plotly)
source("mapboxtoken_setup.R")  # This file defines the mapbox_token variable, which is used for Mapbox access


####### Supply Data (for institution search)####################
ccrc_cip_comp <- readRDS("ccrc_cip_comp.rds")
###############################################################


##############Demand Data: Commuting Zone job postings########
# Load the commuting zone data (CZ_job_post.rds should include CZ20, YEAR, green_job_postings, and geometry)
CZ_job_post <- readRDS("CZ_job_post.rds")
# Ensure the CZ data has unique IDs for Mapbox feature-state
CZ_job_post <- CZ_job_post %>%
  mutate(id = row_number())
##############################################################



######### UI  Design#################

ui <- fluidPage(
  tags$head( includeCSS("www/style.css") ),
  # Set initial CSS style: hide the map until fully loaded
  tags$style(HTML("
    #map { 
      visibility: hidden;
    }
  ")),
  titlePanel("CCRC Green Seek"),
  
  # Layout: Left column for filters; right column for search controls and map container
  fluidRow(
    column(4,
           wellPanel(
             # Year selector for filtering CZ data
             selectInput("selected_year",
                         "Select Year:",
                         choices = sort(unique(CZ_job_post$YEAR),decreasing = TRUE),
                         selected = max(CZ_job_post$YEAR))
             ),
             wellPanel(
               # Display the treemap image directly below the year selector.
               plotlyOutput("treemapPlot", height = "600px")
           )
    ),
    column(8,
           wellPanel(
             fluidRow(
               column(8,
                      selectizeInput("search_term", "Search by Institution:",
                                     choices = NULL,  # Initially empty, will be updated in the server
                                     options = list(
                                       placeholder = "Type institution name...",
                                       openOnFocus = FALSE,
                                       maxOptions = 10
                                     ),
                                     width = "100%")
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
  
######## Footer
  fluidRow(
    column(12, align = "center",
           tags$footer(
             style = "margin-top: 20px; padding: 10px; font-size: 12px; background-color: #f8f9fa; border-top: 1px solid #e9ecef;",
             HTML("Created by Wei Wang, Joshua Rosenberg, Cameron Sublet and Bret Staudt Willet with the Community College Research Center at Teachers College, Columbia.<br>Source code at: <a href='https://github.com/wwang93/CCRC_Mapping_JS' target='_blank'>GitHub</a>.<br>Thanks to funding from JC Morgan Chase.")
           )
    )
  ),
  
######## Include the Mapbox GL JS CSS and JS library

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




######## Server Logic ######################

server <- function(input, output, session) {
  # When the selected year changes, send the selected year to the front end.
  observe({
    req(input$selected_year)
    # Extract the name of the institution for the year
    inst_choices <- sort(unique(ccrc_cip_comp$instnm[ccrc_cip_comp$year == input$selected_year]))
    # Update options for search box
    updateSelectizeInput(session, "search_term", choices = inst_choices, server = TRUE)
    
    # This can be sent directly because observe is already a reactive consumer.
    session$sendCustomMessage("loadYear", input$selected_year)
    
    # Use isolate() to get the current input$selected_year value and make sure you don't get errors inside onFlushed.
    session$onFlushed(function() {
      session$sendCustomMessage("loadInstituteYear", isolate(input$selected_year))
    }, once = TRUE)
  })
  
  
  # Institution search: Filter data by institution name and selected year
  observeEvent(input$search_btn, {
    req(input$search_term)
    req(input$selected_year)
    
    # Filter data by institution name (instnm) and selected_year
    
    search_result <- ccrc_cip_comp %>%
      filter(grepl(input$search_term, instnm, ignore.case = TRUE),
             year == input$selected_year)%>%
      head(1)
    
    if (nrow(search_result) > 0) {
      # Construct the popup content: display the institution name, selected category, and the size value
      popup_text <- paste0(
        "<strong>", search_result$instnm, "</strong><br>",
        "Year: ", search_result$year, "<br>",
        "Total Community College Green Completion Percentage: ", sprintf("%.1f%%", search_result$inst_perc_green_tot * 100)
      )
      coords <- list(
        lng = search_result$longitud,
        lat = search_result$latitude,
        popup = popup_text
      )
      # Send search results to the front end
      session$sendCustomMessage(type = "updateSearch", coords)
    } else {
      showNotification("No Institution Found This Year!", type = "error")
    }
  })
  
  # Reads a list of pre-generated treemap graphs
  treemap_list <- readRDS("Green_degree_treemap_plotly_list.rds")
  
  # Returns a treemap according to the selected year.
  output$treemapPlot <- renderPlotly({
    req(input$selected_year)
    treemap_list[[as.character(input$selected_year)]]
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
