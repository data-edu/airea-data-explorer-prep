# ============================================================
# Load Data
# ============================================================
# Load supply data (institution completions)
supply <- readRDS("supply-institutions-raw-data.rds")

# Load demand data (job postings)
demand <- readRDS("demand-jobs-raw-data.rds")

# Load CZ job post data for map functionality
CZ_job_post <- readRDS("CZ_job_post.rds")

# ============================================================
# Server Logic
# ============================================================

server <- function(input, output, session) {
  
  # Map functionality
  observeEvent(input$cz_metric, {
    session$sendCustomMessage("updateCZMetric", input$cz_metric)
  })
  
  # --- 1. Map tab's year selection & search ---
  observe({
    req(input$selected_year_map)
    
    # Update institution dropdown for the selected year
    inst_choices <- sort(
      unique(supply$instnm[supply$year == input$selected_year_map])
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
    
    search_result <- supply %>%
      filter(
        grepl(input$search_term, instnm, ignore.case = TRUE),
        year == input$selected_year_map
      ) %>%
      slice(1)
    
    if (nrow(search_result) > 0) {
      popup_text <- paste0(
        "<strong>", search_result$instnm, "</strong><br>",
        "<strong>Year:</strong> ", search_result$year, "<br>",
        "<strong>Total Completions:</strong> ",
        format(search_result$inst_cmplt_tot, big.mark = ",", scientific = FALSE), "<br>",
        "<strong>ACEA Completions:</strong> ",
        format(search_result$inst_perc_acea_tot * search_result$inst_cmplt_tot, big.mark = ",", scientific = FALSE), "<br>",
        "<strong>ACEA Percentage:</strong> ",
        sprintf("%.1f%%", search_result$inst_perc_acea_tot * 100)
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
  
  # --- 2. Force map resize when switching to Map tab ---
  observeEvent(input$tabs, {
    if (input$tabs == "mainmap") {
      session$sendCustomMessage("resizeMap", list())
    }
  })
  
  # ============================================================
  # SUPPLY TAB (Institution Completions)
  # ============================================================
  
  # Top 10 institutions table
  output$supply_table <- DT::renderDT({
    req(input$selected_year_supply)
    
    my_df <- supply %>% 
      filter(year == input$selected_year_supply) %>% 
      select(
        Year = year, 
        Name = instnm, 
        `Total Completions` = inst_cmplt_tot,
        `ACEA Completions` = mfreq_acea_cip_cmplt1,
        `ACEA Percentage` = inst_perc_acea_tot
      ) %>% 
      arrange(desc(`Total Completions`)) %>%
      head(10) %>%
      mutate(
        `ACEA Percentage` = sprintf("%.1f%%", `ACEA Percentage` * 100),
        `ACEA Completions` = ifelse(is.na(`ACEA Completions`), 0, `ACEA Completions`)
      )

    DT::datatable(my_df,
                  selection = 'single',
                  options = list(
                    pageLength = 10,
                    lengthChange = FALSE
                  ))
  })
  
  # Institution time series plot
  output$supply_degrees_by_institution <- renderPlot({
    req(input$supply_table_rows_selected)
    
    # Get selected institution
    selected_data <- supply %>% 
      filter(year == input$selected_year_supply) %>% 
      select(
        Year = year,
        Name = instnm,
        `Total Completions` = inst_cmplt_tot,
        `ACEA Completions` = mfreq_acea_cip_cmplt1,
        `ACEA Percentage` = inst_perc_acea_tot
      ) %>%
      arrange(desc(`Total Completions`)) %>%
      head(10)
    
    my_inst <- selected_data[input$supply_table_rows_selected, "Name"]
    
    # Create time series plot
    supply %>% 
      filter(instnm == my_inst$Name) %>% 
      ggplot(aes(x = year, y = inst_cmplt_tot)) +
      geom_point(color = "steelblue", size = 3) +
      geom_line(color = "steelblue", linewidth = 1) +
      theme_minimal() +
      labs(
        title = paste("Total Completions Over Time:", my_inst$Name),
        x = "Year",
        y = "Total Completions"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = seq(2010, 2023, 2))
  })
  
  # Supply tree plot (treemap)
  output$supply_treemap <- renderPlotly({
    req(input$selected_year_supply)
    
    # Create treemap data
    treemap_data <- supply %>%
      filter(year == input$selected_year_supply) %>%
      group_by(instnm) %>%
      summarise(
        total_completions = sum(inst_cmplt_tot, na.rm = TRUE),
        acea_completions = sum(mfreq_acea_cip_cmplt1, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_completions)) %>%
      head(50)  # Top 50 institutions for treemap
    
    # Create treemap
    plot_ly(
      data = treemap_data,
      type = "treemap",
      labels = ~instnm,
      parents = ~"All Institutions",
      values = ~total_completions,
      textinfo = "label+value+percent parent",
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Total Completions: %{value:,}<br>",
        "ACEA Completions: %{customdata:,}<br>",
        "<extra></extra>"
      ),
      customdata = ~acea_completions
    ) %>%
      layout(
        title = paste("Top 50 Institutions by Completions -", input$selected_year_supply),
        margin = list(t = 50, l = 25, r = 25, b = 25)
      )
  })
  
  # Overall completions trend
  output$supply_trend <- renderPlot({
    yearly_totals <- supply %>%
      group_by(year) %>%
      summarize(
        total_completions = sum(inst_cmplt_tot, na.rm = TRUE),
        acea_completions = sum(mfreq_acea_cip_cmplt1, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(yearly_totals, aes(x = year)) +
      geom_line(aes(y = total_completions, color = "Total Completions"), linewidth = 1) +
      geom_line(aes(y = acea_completions, color = "ACEA Completions"), linewidth = 1) +
      geom_point(
        data = dplyr::filter(yearly_totals, year == input$selected_year_supply),
        aes(y = total_completions),
        color = "red",
        size = 3
      ) +
      theme_minimal() +
      labs(
        title = "Completions Trend Over Time",
        x = "Year",
        y = "Number of Completions",
        color = "Type"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = seq(2010, 2023, 2)) +
      scale_color_manual(values = c("Total Completions" = "steelblue", "ACEA Completions" = "darkgreen"))
  })
  
  # ============================================================
  # DEMAND TAB (Job Postings)
  # ============================================================
  
  # Top 10 CZs table
  output$demand_table <- DT::renderDT({
    req(input$selected_year_demand)
    
    my_df_demand <- demand %>%
      filter(YEAR == input$selected_year_demand) %>%
      group_by(CZ_label) %>%
      summarise(
        total_posts = sum(TOTAL_JOB_POSTS, na.rm = TRUE),
        airea_posts = sum(TOTAL_JOB_POSTS[AIREA == 1], na.rm = TRUE),
        population = first(population_estimate_sum),
        .groups = "drop"
      ) %>%
      mutate(
        airea_percentage = (airea_posts / total_posts) * 100,
        posts_per_1000 = (total_posts / population) * 1000
      ) %>%
      select(
        `Commuting Zone` = CZ_label,
        `Total Posts` = total_posts,
        `AIREA Posts` = airea_posts,
        `AIREA %` = airea_percentage,
        `Posts per 1000` = posts_per_1000
      ) %>%
      arrange(desc(`Total Posts`)) %>%
      head(10) %>%
      mutate(
        `AIREA %` = sprintf("%.1f%%", `AIREA %`),
        `Posts per 1000` = sprintf("%.1f", `Posts per 1000`)
      )

    DT::datatable(my_df_demand,
                  selection = 'single',
                  options = list(
                    pageLength = 10,
                    lengthChange = FALSE
                  ))
  })
  
  # CZ time series plot
  output$demand_cz_trend <- renderPlot({
    req(input$demand_table_rows_selected)
    
    # Get selected CZ
    selected_data <- demand %>%
      filter(YEAR == input$selected_year_demand) %>%
      group_by(CZ_label) %>%
      summarise(
        total_posts = sum(TOTAL_JOB_POSTS, na.rm = TRUE),
        airea_posts = sum(TOTAL_JOB_POSTS[AIREA == 1], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_posts)) %>%
      head(10)
    
    my_cz <- selected_data[input$demand_table_rows_selected, "CZ_label"]
    
    # Create time series plot
    demand %>% 
      filter(CZ_label == my_cz$CZ_label) %>%
      group_by(YEAR) %>%
      summarise(
        total_posts = sum(TOTAL_JOB_POSTS, na.rm = TRUE),
        airea_posts = sum(TOTAL_JOB_POSTS[AIREA == 1], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      ggplot(aes(x = YEAR)) +
      geom_line(aes(y = total_posts, color = "Total Posts"), linewidth = 1) +
      geom_line(aes(y = airea_posts, color = "AIREA Posts"), linewidth = 1) +
      geom_point(aes(y = total_posts), color = "steelblue", size = 2) +
      theme_minimal() +
      labs(
        title = paste("Job Postings Over Time:", my_cz$CZ_label),
        x = "Year",
        y = "Number of Job Postings",
        color = "Type"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = seq(2010, 2025, 2)) +
      scale_color_manual(values = c("Total Posts" = "steelblue", "AIREA Posts" = "darkgreen"))
  })
  
  # Demand tree plot (treemap)
  output$demand_treemap <- renderPlotly({
    req(input$selected_year_demand)
    
    # Create treemap data
    treemap_data <- demand %>%
      filter(YEAR == input$selected_year_demand) %>%
      group_by(CZ_label) %>%
      summarise(
        total_posts = sum(TOTAL_JOB_POSTS, na.rm = TRUE),
        airea_posts = sum(TOTAL_JOB_POSTS[AIREA == 1], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(total_posts)) %>%
      head(50)  # Top 50 CZs for treemap
    
    # Create treemap
    plot_ly(
      data = treemap_data,
      type = "treemap",
      labels = ~CZ_label,
      parents = ~"All Commuting Zones",
      values = ~total_posts,
      textinfo = "label+value+percent parent",
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Total Posts: %{value:,}<br>",
        "AIREA Posts: %{customdata:,}<br>",
        "<extra></extra>"
      ),
      customdata = ~airea_posts
    ) %>%
      layout(
        title = paste("Top 50 Commuting Zones by Job Postings -", input$selected_year_demand),
        margin = list(t = 50, l = 25, r = 25, b = 25)
      )
  })
  
  # Overall job postings trend
  output$demand_trend <- renderPlot({
    yearly_totals <- demand %>%
      group_by(YEAR) %>%
      summarize(
        total_posts = sum(TOTAL_JOB_POSTS, na.rm = TRUE),
        airea_posts = sum(TOTAL_JOB_POSTS[AIREA == 1], na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(yearly_totals, aes(x = YEAR)) +
      geom_line(aes(y = total_posts, color = "Total Posts"), linewidth = 1) +
      geom_line(aes(y = airea_posts, color = "AIREA Posts"), linewidth = 1) +
      geom_point(
        data = dplyr::filter(yearly_totals, YEAR == input$selected_year_demand),
        aes(y = total_posts),
        color = "red",
        size = 3
      ) +
      theme_minimal() +
      labs(
        title = "Job Postings Trend Over Time",
        x = "Year",
        y = "Number of Job Postings",
        color = "Type"
      ) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = seq(2010, 2025, 2)) +
      scale_color_manual(values = c("Total Posts" = "steelblue", "AIREA Posts" = "darkgreen"))
  })
  
  # ============================================================
  # LEGACY CODE (keeping for compatibility)
  # ============================================================
  
  # Load treemap list for legacy functionality
  treemap_list <- readRDS("Green_degree_treemap_plotly_list.rds")
  
  output$treemapPlot <- renderPlotly({
    req(input$selected_year_supply)
    treemap_list[[ as.character(input$selected_year_supply) ]]
  })
  
  # Legacy trend plot
  output$trendPlot <- renderPlotly({
    p_SOC <- readRDS("d_SOC.rds") %>%
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