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
ccrc_cip_comp <- readRDS("ccrc_cip_comp_cz.rds") # JR added _cz

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
  
  titlePanel("Advanced Energy and Resilient Infrastructure (AERI) Jobs Postings and Community College Graduates"),
  tags$p("This interactive map shows how community college green program completions align with green-sector job postings by commuting zone. Use the controls above to change year, color metric, or locate a specific institution."),
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
                      "AERI Job Postings"      = "green_job_posting",
                      "% AERI Postings" = "pct_green",
                      "AERI Jobs / 1,000 Residents" = "per1000"
                    )),
        selectizeInput("search_term", "Search by Institution:",
                       choices = NULL, options = list(), width="100%"),
        fluidRow(
          column(6, actionButton("search_btn","Search", class="btn-primary", width="100%")),
          column(6, tags$button("Clear", onclick="clearMap()", class="btn btn-default", style="width:100%;"))
        )
      ),
      
      # Controls shown only on the Supply tab
      conditionalPanel(
        "input.tabs == 'treemap'",
        selectInput("selected_year_supply", "Select Year:",
                    choices = sort(unique(CZ_job_post$YEAR), decreasing = TRUE),
                    selected = max(CZ_job_post$YEAR))
      ),
      
      # controls shown only on demand tab
      
      conditionalPanel(
        "input.tabs == 'demand'",
        checkboxGroupInput(
          "selected_SOCs", 
          "Select SOC:",
          choices = c(
            "11-1011" = "Management",
            "13-1199" = "Business and Financial Operations",
            "15-1212" = "Information Security Analysts",
            "17-2199" = "Engineers",
            "19-2041" = "Environmental Scientists and Specialists",
            "19-4099" = "Life, Physical, and Social Science Technicians",
            "27-1024" = "Graphic Designers",
            "27-3031" = "Public Relations Specialists"
          ),
          
        )
      )
    ),  # ← here we close sidebarPanel() and then put a comma
    
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "tabs", type = "tabs",
        
        tabPanel("Map", value = "mainmap",
                 div(id="map", style="height:700px;width:100%;")
        ),
        
        tabPanel("Supply from Community Colleges", value = "treemap",
                 # AERI Definition (as per screenshot)
                 tags$p(strong("AERI"), " = Advanced Energy and Resilient Infrastructure", 
                        style="margin-top: 15px; margin-bottom: 15px;"),
                 
                 # Top Row: Table Output
                 fluidRow(
                   column(12,
                          # The screenshot shows "Institution" as a key part of this section.
                          # You can add a specific title for the table if needed, e.g., tags$h4("Institution Data").
                          DT::dataTableOutput("supply_table") 
                   )
                 ),
                 
                 tags$hr(), # Visual separator
                 
                 # Bottom Row: Treemap (left) and Bar chart (right)
                 fluidRow(
                   column(7, # Adjust width as needed (e.g., 6 for equal split)
                          tags$h5("Tree map showing frequent Degrees (CIPs)"), # Title for the treemap
                          plotlyOutput("treemapPlot", height = "600px") # Your existing treemap plot
                   ),
                   
                   column(5, # Adjust width as needed
                          tags$h5("Degrees (Lines represent individual CIP codes)"), # Title for the bar chart
                          plotOutput("degrees_over_time") # Placeholder for your bar chart output
                   )
                 )
        ),
        
        tabPanel("Demand from Employers", value = "demand",
                 
                 plotlyOutput("trendPlot")
                 
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
             HTML("Created by Wei Wang, Joshua Rosenberg, Cameron Sublet and Bret Staudt Willet with the <a href='https://ccrc.tc.columbia.edu/' target='_blank'>Community College Research Center at Teachers College, Columbia University</a>. Source code at: <a href='https://github.com/data-edu/CCRC_GreenSeek-Mapping' target='_blank'>GitHub</a>. Thanks to JPMorgan Chase for supporting this w.")
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
    
    d_SOC <- readRDS("d_SOC.rds")
    
    p_SOC <- d_SOC %>% 
      ggplot(aes(
        x      = YEAR,
        y      = TOTAL_POSTS,
        group  = Occupation,
        colour = Occupation,
        text   = paste0(
          "Year: ", YEAR, "<br>",
          "Total Posts: ", scales::comma(TOTAL_POSTS), "<br>",
          "Occupation: ", Occupation
        )
      )) +
      geom_line() +
      scale_x_continuous(breaks = 2010:2024) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(
        title   = "Green Jobs Postings By SOC Code",
        caption = "Source: Lightcast postings data",
        x       = NULL,
        y       = NULL
      ) +
      theme(legend.position = "none")
    
    plotly::ggplotly(p_SOC, tooltip = "text")
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
  
  # render table
  
  output$supply_table <- renderDataTable( {
    
    ccrc_cip_comp %>% 
      filter(year == input$selected_year_supply) %>% 
      select(Year = year, Name = instnm, `Commuting Zone` = CZ_label, completions = inst_cmplt_o) 
      # Wei, I need help here - how to select the variable(s) of note here - completions, %, and per 1000
    
  })
  
  # create graph of degrees
  output$degrees_over_time <- renderPlot({
    yearly_totals <- ccrc_cip_comp %>%
      group_by(year) %>%
      summarize(total_completions = sum(inst_cmplt_o), .groups = "drop")
    
    ggplot(yearly_totals, aes(x = year, y = total_completions)) +
      geom_line(color = "grey70") +
      geom_point(
        data = dplyr::filter(yearly_totals, year == input$selected_year_supply),
        aes(x = year, y = total_completions),
        color = "red",
        size = 3
      ) +
      theme_bw() +
      ylab("Total Completions") +
      xlab("Year") +
      scale_y_continuous(labels = scales::comma)
    
  })
  
}

# ============================================================
# Launch the Shiny Application
# ============================================================
shinyApp(ui, server)
