# ==============================================================================
# Load Required Libraries and Initialize Settings
# ==============================================================================

library(shiny)                 # Shiny framework for interactive web applications
library(plotly)                # For interactive plotting
source("mapboxtoken_setup.R")  # Loads Mapbox_token used for Mapbox access



# ==============================================================================
# Load Demand Data: Commuting Zone Job Postings
# ==============================================================================

# Read the Commuting Zone (CZ) job postings data. This RDS file should contain fields:
# - CZ20: Commuting zone identifier
# - YEAR: Year of the data
# - green_job_postings: Number of green job postings in that CZ
# - geometry: Spatial information for mapping
CZ_job_post <- readRDS("CZ_job_post.rds")



# ==============================================================================
# Launch the Shiny app and Navigation Bar
# ==============================================================================

navbarPage(
  
  # ============================================================================
  # CSS for styling
  # ============================================================================
  
  tags$head(
    includeCSS("custom-style.css"),
    
    # Mapbox & JS Include Mapbox GL JS and custom scripts
    tags$link(rel="stylesheet", 
              href="https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.css"),
    tags$script(src="https://api.mapbox.com/mapbox-gl-js/v2.14.1/mapbox-gl.js"),
    tags$script(src="mapbox.js"),
    tags$script(HTML(paste0("const mapboxToken = '", mapbox_token, "';"))),
    tags$script(src="https://cdn.jsdelivr.net/npm/@turf/turf@6/turf.min.js")
  ),
  
  
  
  # ============================================================================
  # Title
  # ============================================================================
  
  title = 
    tags$div(
      tags$h1(img(src = "airea-logo-draft.png", height = "28px"),
              "Advanced Infrastructure, Energy, and Agriculture (AIREA) Data Explorer")
    ),
  
  
  
  # ============================================================================
  # Header: Navigation Bar
  # ============================================================================

  header = 
    tags$div(
      style = "background-color: #f2f8f2; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
      tags$h4("These interactive maps and data visualizations are for exploring how community college green program completions align with AIREA-sector job postings by commuting zone."),
      tags$hr(),
      tags$h3("How to use the AIREA Data Explorer:"),
      tags$ul(
        style = "margin-bottom: 0;",
        tags$li(tags$strong("Map Tab"), "— Use the controls on the map to change year and color metric, or search for a specific institution. You can click and drag to reposition the input box anywhere on the map."),
        tags$li(tags$strong("Degree Completions Tab"), "— Click on an institution in the table to see its AIREA completion trends and top programs."),
        tags$li(tags$strong("Job Postings Tab"), "— Click on a commuting zone in the table to see its AIREA job posting trends and top occupations."),
        tags$li(tags$strong("Supply vs Demand Tab"), "— View scatter plots for AIREA supply and demand by commuting zone and AIREA raw counts by commuting zone"),
      ),
      tags$hr()
    ),

  
    
  # ============================================================================
  # Footer
  # ============================================================================
  
  footer = 
    tags$div(
      class = "app-footer",
      tags$hr(),
      tags$p("Created by Wei Wang, Joshua Rosenberg, Cameron Sublet and Bret Staudt Willet,",
             "in partnership with the",
             tags$a(href="https://ccrc.tc.columbia.edu/", 
                    "Community College Research Center"),
             "at Teachers College, Columbia University.",
             "Source code available on ",
             tags$a(href="https://github.com/data-edu/CCRC_GreenSeek-Mapping",
                    "GitHub."),
             "Thanks to JPMorgan Chase for their financial support."
      )
    ),
  
  
  
  
  
  # ============================================================================
  # Panel 1: Map
  # ============================================================================
  
  tabPanel("Map", value = "mainmap",
           
           # Map container
           fluidRow(
             column(12,
                    div(id="map", style="height:600px; width:100%;")
             )
           ),
           
           # Interactive input container
           absolutePanel(id = "controls", 
                         class = "panel panel-default", 
                         fixed = TRUE,
                         draggable = TRUE, 
                         top = "50%",
                         left = 40,
                         width = 360, height = "auto",
                         
                         selectInput("selected_year_map",
                                     tags$div(style = "margin-top: 15px;", tags$h3("Select Year:")),
                                     choices = sort(unique(CZ_job_post$YEAR), decreasing = TRUE),
                                     selected = max(CZ_job_post$YEAR)
                         ),
                         selectInput("cz_metric", 
                                     tags$h3("Color by:"), 
                                     choices = c(
                                       "AIREA Job Postings" = "airea_job_posting",
                                       "% AIREA Postings" = "pct_green",
                                       "AIREA Jobs / 1,000 Residents" = "per1000"
                                     )
                         ),
                         selectizeInput("search_term", 
                                        tags$h3("Search by Institution:"),
                                        choices = NULL, 
                                        options = list(), 
                                        width="100%"
                         ),
                         
                         actionButton("search_btn", 
                                      "Search", 
                                      class = "btn-primary"),
                         tags$button("Clear", 
                                     onclick = "clearMap()",
                                     class = "btn btn-clear", 
                                     style = "margin-left: 10px;")
           )
  ),
  
           
  
  
  
  # ============================================================================
  # Panel 2: Degree Completions
  # ============================================================================
  
  tabPanel("Degree Completions", value = "treemap",
           
           tags$h2("Institutions by Total Completions (All Years)"),
           
           # Top Row: Table Output
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      tags$h4(
                        icon("hand-point-up"),
                        "Click on any institution row in the table below to see its AIREA completion trends and top programs."),
                      tags$br(),
                      tags$p(icon("info-circle"), 
                             "Showing all institutions with data aggregated across all years, sorted by total completions. The top institution is selected by default.")
                    ),
                    tags$style(HTML(
                      "table.dataTable tr.active td, table.dataTable tr.active {background-color: #31a2b6  !important;}")),
                    DT::dataTableOutput("supply_table")
             )
           ),
           
           tags$hr(),
           
           # Second Row: Time series plot (full width)
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      tags$h4(
                        icon("hand-point-up"),
                        "Click on an institution above to see its AIREA completion percentage over time")),
                    plotOutput("supply_degrees_by_institution", height = "400px")
             )
           ),
           
           tags$hr(),
           
           # Third Row: Treemap
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      tags$h4(
                        icon("hand-point-up"),
                        "Click on an institution above to see its top 5 AIREA CIPs for the selected year")),
                    plotlyOutput("supply_treemap", height = "500px")
             )
           )
  ),
  
  
  
  
  
  # ============================================================================
  # Panel 3: Job Postings
  # ============================================================================
  
  tabPanel("Job Postings", value = "demand",
           
           tags$h2("Commuting Zones by AIREA Job Posting Percentage (All Years)"),
           
           # Top Row: Table Output
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      tags$h4(
                        icon("hand-point-up"),
                        "Click on any Commuting Zone row in the table below to see its AIREA job posting trends and top occupations!"),
                      tags$br(),
                      tags$p(icon("info-circle"),
                             "Showing all commuting zones, sorted by AIREA percentage. The top Commuting Zone is selected by default.")
                    ),
                    tags$style(HTML(
                      "table.dataTable tr.active td, table.dataTable tr.active {background-color: #31a2b6  !important;}")),
                    DT::dataTableOutput("demand_table") 
             )
           ),
           
           tags$hr(),
           
           # Second Row: Time series plot (full width)
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      tags$h4(
                        icon("hand-point-up"),
                        "Click on a Commuting Zone above to see its AIREA job posting percentage over time")),
                    plotOutput("demand_cz_trend", height = "400px")
             )
           ),
           
           tags$hr(),
           
           # Third Row: Treemap
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                    tags$h4(
                      icon("hand-point-up"),
                      "Click on a Commuting Zone above to see its top 10 SOCs")),
                    plotlyOutput("demand_treemap", height = "500px")
             )
           )
  ),
  
  
  
  
  
  # ============================================================================
  # Panel 4: Supply vs Demand
  # ============================================================================
  
  tabPanel("Supply vs Demand", value = "scatter",
           
           tags$h2("AIREA Supply vs Demand by Commuting Zone"),
           
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      tags$p(icon("info-circle"),
                             "This scatter plot compares AIREA supply (% of completions from community colleges) vs AIREA demand (% of job postings) at the commuting zone level. Each point represents a commuting zone, with size indicating total completions.")),
                    plotlyOutput("scatter_plot", height = "600px")
             )
           ),
           
           tags$hr(),
           
           # Second scatter plot for raw counts
           tags$h2("AIREA Raw Counts by Commuting Zone"),
           fluidRow(
             column(12,
                    tags$div(
                      style = "background-color: #dff3f6; padding: 15px; border-radius: 5px; margin-bottom: 15px;",
                      tags$p(icon("info-circle"),
                             "This scatter plot shows the absolute numbers of AIREA completions vs AIREA job postings by commuting zone.")),
                    plotlyOutput("scatter_plot_counts", height = "600px")
             )
           )
  )
  
  
)
