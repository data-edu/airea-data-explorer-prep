# ============================================================
# Load Data
# ============================================================
# Load supply data (institution completions)
supply <- readRDS("supply-institutions-raw-data.rds")

# Load demand data (job postings)
demand <- readRDS("demand-jobs-raw-data.rds")

# Load CZ job post data for map functionality
CZ_job_post <- readRDS("CZ_job_post.rds")

# Load SOC and CIP labels for treemaps
soc_labels <- readRDS("d_SOC.rds")
cip_labels <- readRDS("ccrc_cip_comp.rds")

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
        "<strong>AIREA Completions:</strong> ",
        format(search_result$inst_perc_acea_tot * search_result$inst_cmplt_tot, big.mark = ",", scientific = FALSE), "<br>",
        "<strong>AIREA Percentage:</strong> ",
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
  # SUPPLY TAB (Institution Completions) - OPTIMIZED
  # ============================================================
  
  # Reactive expression for supply data aggregated across all years
  supply_aggregated_data <- reactive({
    supply %>% 
      group_by(instnm) %>%
      summarise(
        `Total Completions` = sum(inst_cmplt_tot, na.rm = TRUE),
        `AIREA Completions` = sum(mfreq_acea_cip_cmplt1, na.rm = TRUE),
        `AIREA Percentage` = sum(mfreq_acea_cip_cmplt1, na.rm = TRUE) / sum(inst_cmplt_tot, na.rm = TRUE),
        .groups = "drop"
      ) %>% 
      arrange(desc(`Total Completions`))
  })
  
  # Reactive expression for selected institution
  selected_institution <- reactive({
    req(input$supply_table_rows_selected)
    req(supply_aggregated_data())
    supply_aggregated_data()[input$supply_table_rows_selected, "instnm"]
  })
  
  # All institutions table (sorted by total completions, showing first 10)
  output$supply_table <- DT::renderDT({
    my_df_supply <- supply_aggregated_data() %>%
      select(
        `Institution` = instnm,
        `Total Completions` = `Total Completions`,
        `AIREA Completions` = `AIREA Completions`,
        `AIREA %` = `AIREA Percentage`
      ) %>%
      mutate(
        `Total Completions` = format(`Total Completions`, big.mark = ","),
        `AIREA Completions` = format(`AIREA Completions`, big.mark = ","),
        `AIREA %` = sprintf("%.1f%%", `AIREA %` * 100)
      )

    DT::datatable(my_df_supply,
                  selection = 'single',
                  options = list(
                    pageLength = 10,
                    lengthChange = FALSE
                  ))
  })
  
  # Institution time series plot (showing AIREA percentage)
  output$supply_degrees_by_institution <- renderPlot({
    req(selected_institution())
    
    my_inst <- selected_institution()
    
    # Create time series plot showing AIREA percentage
    supply %>% 
      filter(instnm == my_inst$instnm) %>% 
      ggplot(aes(x = year, y = inst_perc_acea_tot * 100)) +
      geom_point(color = "steelblue", size = 3) +
      geom_line(color = "steelblue", linewidth = 1) +
      theme_minimal() +
      labs(
        title = paste("AIREA Completion Percentage Over Time:", my_inst$instnm),
        x = "Year",
        y = "AIREA Completion Percentage (%)"
      ) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      scale_x_continuous(breaks = seq(2010, 2023, 2))
  })
  
  # Supply tree plot (treemap) - Top 5 CIPs for selected institution
  output$supply_treemap <- renderPlotly({
    req(selected_institution())
    
    my_inst <- selected_institution()
    
    # Create treemap data for top 5 CIPs for the selected institution (aggregated across all years)
    # Get the institution data with all 5 CIP fields aggregated across all years
    inst_data <- supply %>%
      filter(instnm == my_inst$instnm) %>%
      group_by(instnm) %>%
      summarise(
        mfreq_acea_cip_cmplt1 = sum(mfreq_acea_cip_cmplt1, na.rm = TRUE),
        mfreq_acea_cip_cmplt2 = sum(mfreq_acea_cip_cmplt2, na.rm = TRUE),
        mfreq_acea_cip_cmplt3 = sum(mfreq_acea_cip_cmplt3, na.rm = TRUE),
        mfreq_acea_cip_cmplt4 = sum(mfreq_acea_cip_cmplt4, na.rm = TRUE),
        mfreq_acea_cip_cmplt5 = sum(mfreq_acea_cip_cmplt5, na.rm = TRUE),
        mfreq_acea_cip1_pct = mean(mfreq_acea_cip1_pct, na.rm = TRUE),
        mfreq_acea_cip2_pct = mean(mfreq_acea_cip2_pct, na.rm = TRUE),
        mfreq_acea_cip3_pct = mean(mfreq_acea_cip3_pct, na.rm = TRUE),
        mfreq_acea_cip4_pct = mean(mfreq_acea_cip4_pct, na.rm = TRUE),
        mfreq_acea_cip5_pct = mean(mfreq_acea_cip5_pct, na.rm = TRUE),
        .groups = "drop"
      )
    
    if (nrow(inst_data) == 0) {
      # Create empty treemap if no data
      plot_ly(
        type = "treemap",
        labels = "No AIREA CIPs",
        parents = "",
        values = 1
      ) %>%
        layout(
          title = paste("Top 5 AIREA CIPs for", my_inst$instnm, "(All Years)"),
          margin = list(t = 50, l = 25, r = 25, b = 25)
        )
    } else {
      # Extract all 5 CIPs and their percentages
      treemap_data <- data.frame(
        CIP_Number = 1:5,
        CIP_Completions = c(
          inst_data$mfreq_acea_cip_cmplt1,
          inst_data$mfreq_acea_cip_cmplt2,
          inst_data$mfreq_acea_cip_cmplt3,
          inst_data$mfreq_acea_cip_cmplt4,
          inst_data$mfreq_acea_cip_cmplt5
        ),
        CIP_Percentage = c(
          inst_data$mfreq_acea_cip1_pct,
          inst_data$mfreq_acea_cip2_pct,
          inst_data$mfreq_acea_cip3_pct,
          inst_data$mfreq_acea_cip4_pct,
          inst_data$mfreq_acea_cip5_pct
        )
      ) %>%
        filter(!is.na(CIP_Completions), CIP_Completions > 0) %>%
        mutate(
          CIP_Label = paste0("CIP ", CIP_Number, ": ", 
                            case_when(
                              CIP_Number == 1 ~ "Engineering Technologies",
                              CIP_Number == 2 ~ "Construction Trades", 
                              CIP_Number == 3 ~ "Mechanic & Repair Technologies",
                              CIP_Number == 4 ~ "Precision Production",
                              CIP_Number == 5 ~ "Agriculture & Natural Resources"
                            )),
          CIP_Percentage = CIP_Percentage * 100
        )
      
      if (nrow(treemap_data) == 0) {
        # Create empty treemap if no CIPs
        plot_ly(
          type = "treemap",
          labels = "No AIREA CIPs",
          parents = "",
          values = 1
        ) %>%
          layout(
            title = paste("Top 5 AIREA CIPs for", my_inst$instnm, "(All Years)"),
            margin = list(t = 50, l = 25, r = 25, b = 25)
          )
      } else {
        # Create treemap
        plot_ly(
          data = treemap_data,
          type = "treemap",
          labels = ~CIP_Label,
          parents = ~"AIREA CIPs",
          values = ~CIP_Completions,
          textinfo = "label+value+percent parent",
          hovertemplate = paste(
            "<b>%{label}</b><br>",
            "Completions: %{value:,}<br>",
            "Percentage: %{customdata:.1f}%<br>",
            "<extra></extra>"
          ),
          customdata = ~CIP_Percentage
        ) %>%
          layout(
            title = paste("Top 5 AIREA CIPs for", my_inst$instnm, "(All Years)"),
            margin = list(t = 50, l = 25, r = 25, b = 25)
          )
      }
    }
  })
  
  # ============================================================
  # DEMAND TAB (Job Postings) - OPTIMIZED
  # ============================================================
  
  # Reactive expression for demand data by year (using most recent year as default, excluding 2025)
  demand_year_data <- reactive({
    # Filter out 2025 data and use most recent year through 2024
    max_year_2024 <- max(demand$YEAR[demand$YEAR <= 2024], na.rm = TRUE)
    demand %>%
      filter(YEAR == max_year_2024) %>%
      group_by(CZ_label) %>%
      summarise(
        total_posts = sum(TOTAL_JOB_POSTS, na.rm = TRUE),
        airea_posts = sum(TOTAL_JOB_POSTS[AIREA == 1], na.rm = TRUE),
        population = first(population_estimate_sum),
        .groups = "drop"
      ) %>%
      mutate(
        airea_percentage = (airea_posts / total_posts) * 100,
        posts_per_1000 = ifelse(is.na(population) | population == 0, 
                               NA, 
                               (total_posts / population) * 1000)
      ) %>%
      arrange(desc(airea_percentage))
  })
  
  # Reactive expression for selected CZ
  selected_cz <- reactive({
    req(input$demand_table_rows_selected)
    req(demand_year_data())
    demand_year_data()[input$demand_table_rows_selected, "CZ_label"]
  })
  
  # All CZs table (sorted by AIREA percentage, showing first 10)
  output$demand_table <- DT::renderDT({
    my_df_demand <- demand_year_data() %>%
      select(
        `Commuting Zone` = CZ_label,
        `Total Posts` = total_posts,
        `AIREA Posts` = airea_posts,
        `AIREA %` = airea_percentage,
        `Posts per 1000` = posts_per_1000
      ) %>%
      mutate(
        `Commuting Zone` = gsub(" CZ$", "", `Commuting Zone`),
        `Total Posts` = format(`Total Posts`, big.mark = ","),
        `AIREA Posts` = format(`AIREA Posts`, big.mark = ","),
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
  
  # CZ time series plot (showing AIREA percentage)
  output$demand_cz_trend <- renderPlot({
    req(selected_cz())
    
    my_cz <- selected_cz()
    
    # Create time series plot showing AIREA percentage (excluding 2025)
    demand %>% 
      filter(CZ_label == my_cz$CZ_label, YEAR <= 2024) %>%
      group_by(YEAR) %>%
      summarise(
        total_posts = sum(TOTAL_JOB_POSTS, na.rm = TRUE),
        airea_posts = sum(TOTAL_JOB_POSTS[AIREA == 1], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(airea_percentage = (airea_posts / total_posts) * 100) %>%
      ggplot(aes(x = YEAR, y = airea_percentage)) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_point(color = "steelblue", size = 2) +
      theme_minimal() +
      labs(
        title = paste("AIREA Job Posting Percentage Over Time:", my_cz$CZ_label),
        x = "Year",
        y = "AIREA Job Posting Percentage (%)"
      ) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      scale_x_continuous(breaks = seq(2010, 2024, 2))
  })
  
  # Demand tree plot (treemap) - Top SOCs for selected CZ (aggregated across all years)
  output$demand_treemap <- renderPlotly({
    req(selected_cz())
    
    my_cz <- selected_cz()
    
    # Create treemap data for top SOCs for the selected CZ (aggregated across all years, excluding 2025)
    treemap_data <- demand %>%
      filter(CZ_label == my_cz$CZ_label, YEAR <= 2024) %>%
      group_by(SOC_CODE) %>%
      summarise(
        total_posts = sum(TOTAL_JOB_POSTS, na.rm = TRUE),
        airea_posts = sum(TOTAL_JOB_POSTS[AIREA == 1], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(airea_percentage = (airea_posts / total_posts) * 100) %>%
      arrange(desc(total_posts)) %>%
      head(10) %>%
      mutate(
        airea_percentage = ifelse(is.na(airea_percentage), 0, airea_percentage)
      )
    
    # Join with SOC labels if available
    if (exists("soc_labels") && nrow(soc_labels) > 0) {
      # Debug: print what we're joining
      print(paste("SOC labels available, rows:", nrow(soc_labels)))
      print(paste("Treemap data SOC codes:", paste(head(treemap_data$SOC_CODE, 5), collapse = ", ")))
      
      treemap_data <- treemap_data %>%
        left_join(soc_labels %>% select(SOC_CODE, Occupation) %>% distinct(), by = "SOC_CODE") %>%
        mutate(
          SOC_Label = ifelse(!is.na(Occupation), 
                            paste0("SOC ", SOC_CODE, ": ", Occupation),
                            paste0("SOC ", SOC_CODE))
        )
      
      # Debug: print result
      print(paste("After join, sample labels:", paste(head(treemap_data$SOC_Label, 3), collapse = ", ")))
    } else {
      print("SOC labels not available")
      treemap_data <- treemap_data %>%
        mutate(SOC_Label = paste0("SOC ", SOC_CODE))
    }
    
    # Create treemap
    plot_ly(
      data = treemap_data,
      type = "treemap",
      labels = ~SOC_Label,
      parents = ~"All SOCs",
      values = ~total_posts,
      textinfo = "label+value+percent parent",
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Total Posts: %{value:,}<br>",
        "AIREA Posts: %{customdata:,}<br>",
        "AIREA %: %{customdata2:.1f}%<br>",
        "<extra></extra>"
      ),
      customdata = ~airea_posts,
      customdata2 = ~airea_percentage
    ) %>%
      layout(
        title = paste("Top 10 SOCs for", my_cz$CZ_label, "(All Years through 2024)"),
        margin = list(t = 50, l = 25, r = 25, b = 25)
      )
  })
  
  # ============================================================
  # LEGACY CODE (keeping for compatibility)
  # ============================================================
  
  # Load treemap list for legacy functionality
  treemap_list <- readRDS("Green_degree_treemap_plotly_list.rds")
  
  output$treemapPlot <- renderPlotly({
    # Use the most recent year as default
    default_year <- max(supply$year, na.rm = TRUE)
    treemap_list[[ as.character(default_year) ]]
  })
}