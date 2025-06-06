# ============================================================
# Load Supply Data (for Institution Search)
# ============================================================
# Read supply data from an RDS file; this data is used for filtering/searching institutions.
ccrc_cip_comp <- readRDS("ccrc_cip_comp_cz.rds") # JR added _cz

# Ensure each commuting zone feature gets a unique ID (for Mapbox feature-state management like hover events)
#CZ_job_post <- CZ_job_post %>% 
# mutate(id = row_number())

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
        "<strong>", search_result$CZ_label, "</strong><br>",
        "<strong>Year:</strong> ", search_result$year, "<br>",
        "<strong>AIREA Degrees:</strong> ",
        format(search_result$inst_green_cmplt_tot, big.mark = ",", scientific = FALSE), "<br>",
        "<strong>AIREA Degree Rate:</strong> ",
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
  
  # render supply table
  
  output$supply_table <- DT::renderDT( {
    
    my_df <- ccrc_cip_comp %>% 
      filter(year == input$selected_year_supply) %>% 
      select(Year = year, Name = instnm, `Commuting Zone` = CZ_label, completions = inst_cmplt_o) %>% 
      arrange(desc(completions))
    
    # Wei, I need help here - how to select the variable(s) of note here - completions, %, and per 1000
    DT::datatable(my_df,
                  options = list(
                    pageLength = 5,       # show 5 rows per page
                    lengthChange = FALSE  # (optional) hide the “Show X entries” dropdown
                  ))
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
  
  # render demand table
  
  # d_SOC_data <- reactive( {
  #   req(input$tabs == "demand")
  #   readRDS("d_SOC.rds") 
  #   
  # })
  
  output$demand_table <- DT::renderDT( {
    
    my_df_demand <-  readRDS("d_SOC.rds") %>% # need to make reactive (see above)
      filter(YEAR == input$selected_year_demand) %>% 
      select(YEAR, Occupation, SOC_CODE, TOTAL_POSTS) %>% 
      arrange(desc(TOTAL_POSTS))
    
    DT::datatable(my_df_demand,
                  options = list(
                    pageLength = 5,       # show 5 rows per page
                    lengthChange = FALSE  # (optional) hide the “Show X entries” dropdown
                  ))
  })
  
  # render demand tab
  
  output$trendPlot <- renderPlotly({
    
    p_SOC <- readRDS("d_SOC.rds") %>% # need to make reactive (see above)
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
  
  
}