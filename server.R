# ==============================================================================
# Load Data
# ==============================================================================

library(dplyr)
library(tidyr)
library(scales)
library(arrow)


# ==============================================================================
# Load supply data (institution completions)
# ==============================================================================

## map supply data for tab 1 only(Don't delete, used in map)
mapsupply <- readRDS("prep/mapsupply.rds")


## supply for tab 2 
supply <- read_parquet("prep/supply.parquet.gzip")

supply %>% 
  distinct()



# ==============================================================================
# Load demand data (job postings)
# ==============================================================================


demand <- read_parquet("prep/demand.parquet.gzip")


# ==============================================================================
# Server Logic
# ==============================================================================

server <- function(input, output, session) {
  
  # Map functionality
  observeEvent(input$cz_metric, {
    session$sendCustomMessage("updateCZMetric", input$cz_metric)
  })
  
  

  # ============================================================================
  # Panel 1: Map
  # ============================================================================
  
  # --- 1. Map tab's year selection & search ---
  observe({
    req(input$selected_year_map)
    
    # Update institution dropdown for the selected year
    inst_choices <- sort(
      unique(mapsupply$instnm[mapsupply$year == input$selected_year_map])
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
    
    search_result <- mapsupply %>%
      filter(
        grepl(input$search_term, instnm, ignore.case = TRUE),
        year == input$selected_year_map
      ) %>%
      slice(1)
    
    if (nrow(search_result) > 0) {
      popup_text <- paste0(
        "<strong>", search_result$instnm, "</strong><br>",
        "<strong>CZ:</strong> ", search_result$cz_label, "<br>",
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
  
  
  
  # ============================================================================
  # Panel 2: Degree Completions
  # ============================================================================
  
  # Reactive expression for supply data aggregated across all years
  supply_aggregated_data <- reactive({
    supply %>% 
      group_by(instnm) %>%
      summarise(
        `Total Completions` = sum(total_completions, na.rm = TRUE),
        `AIREA Completions` = sum(airea_completions, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(`AIREA Percentage` = ifelse(`Total Completions` > 0, `AIREA Completions` / `Total Completions`, NA_real_)) %>% 
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
        `AIREA Completions` = format(`AIREA Completions`, big.mark = ",")
        # Keep AIREA % as numeric for proper sorting
      )

    DT::datatable(my_df_supply,
                  selection = list(mode = 'single', selected = 1),  # Select first row by default
                  options = list(
                    pageLength = 10,
                    lengthChange = FALSE,
                    order = list(list(3, 'desc'))  # Sort by AIREA % (4th column, index 3)
                  ),
                  style="bootstrap"
    ) %>%
    DT::formatPercentage(columns = "AIREA %", digits = 1)  # Format as percentage while keeping numeric sorting
  })
  
  # Institution time series plot (showing AIREA percentage)
  output$supply_degrees_by_institution <- renderPlot({
    req(selected_institution())
    
    my_inst <- selected_institution()
    
    # Create time series plot showing AIREA percentage
    supply %>% 
      filter(instnm == my_inst$instnm) %>%
      group_by(year) %>%
      summarise(
        total_completions = sum(total_completions, na.rm = TRUE),
        airea_completions = sum(airea_completions, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(airea_pct = ifelse(total_completions > 0, airea_completions / total_completions * 100, NA_real_)) %>%
      ggplot(aes(x = year, y = airea_pct)) +
      geom_point(color = "#31a2b6", size = 3) +
      geom_line(color = "#31a2b6", linewidth = 1) +
      theme_minimal() +
      labs(
        title = paste("AIREA Degrees Over Time:", my_inst$instnm),
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
    
    # Use the most recent year for the institution
    most_recent_year <- supply %>%
      filter(instnm == my_inst$instnm) %>%
      summarise(max_year = max(year, na.rm = TRUE)) %>%
      pull(max_year)

    # Build treemap data: AIREA completions by CIP for that year
    treemap_data <- supply %>%
      filter(instnm == my_inst$instnm, year == most_recent_year) %>%
      group_by(cip, cip_title) %>%
      summarise(
        CIP_Completions = sum(airea_completions, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(CIP_Completions), CIP_Completions > 0) %>%
      arrange(desc(CIP_Completions))

    total_airea_inst_year <- sum(treemap_data$CIP_Completions, na.rm = TRUE)

    treemap_data <- treemap_data %>%
      mutate(
        CIP_Code = cip,
        CIP_Label = ifelse(!is.na(cip_title) & cip_title != "",
                            paste0("CIP ", cip, ": ", cip_title),
                            paste0("CIP ", cip)),
        CIP_Percentage = ifelse(total_airea_inst_year > 0,
                                (CIP_Completions / total_airea_inst_year) * 100,
                                NA_real_)
      )

    # Get top 5 and calculate "Other"
    top_5_data <- treemap_data %>% head(5)
    if (nrow(treemap_data) > 5) {
      other_completions <- treemap_data %>% slice(6:n()) %>% summarise(CIP_Completions = sum(CIP_Completions, na.rm = TRUE)) %>% pull(CIP_Completions)
      other_percentage <- ifelse(total_airea_inst_year > 0, (other_completions / total_airea_inst_year) * 100, NA_real_)
    } else {
      other_completions <- 0
      other_percentage <- 0
    }
    
    # Add "Other" row if there are additional CIPs
    if (other_completions > 0) {
      other_row <- data.frame(
        CIP_Code = "Other",
        CIP_Completions = other_completions,
        CIP_Percentage = other_percentage,
        CIP_Label = "Other AIREA CIPs"
      )
      treemap_data <- bind_rows(top_5_data, other_row)
    } else {
      treemap_data <- top_5_data
    }

    # Labels already constructed from `cip_title`; ensure 'Other' label is set
    treemap_data <- treemap_data %>%
      mutate(CIP_Label = ifelse(CIP_Code == "Other", "Other AIREA CIPs", CIP_Label))

    if (nrow(treemap_data) == 0) {
      plot_ly(
        type = "treemap",
        labels = "No AIREA CIPs",
        parents = "",
        values = 1,
        textfont = list(size = 14, color = "white")  # Balanced font size for readability
      ) %>%
        layout(
          title = paste("Top 5 AIREA CIPs for", my_inst$instnm, "-", most_recent_year),
          margin = list(t = 50, l = 25, r = 25, b = 25)
        )
    } else {
      plot_ly(
        data = treemap_data,
        type = "treemap",
        labels = ~CIP_Label,
        parents = ~"AIREA CIPs",
        values = ~CIP_Completions,
        textinfo = "label+text",
        text = ~paste0(sprintf("%.1f%%", CIP_Percentage)),
        textfont = list(size = 14, color = "white"),
        hovertemplate = paste(
          "<b>%{label}</b><br>",
          "Completions: %{value:,}<br>",
          "Percentage: %{customdata:.1f}%<br>",
          "<extra></extra>"
        ),
        customdata = ~CIP_Percentage
      ) %>%
        layout(
          title = paste("Top 5 AIREA CIPs for", my_inst$instnm, "-", most_recent_year),
          margin = list(t = 50, l = 25, r = 25, b = 25)
        )
    }
  })
  
  
  
  # ============================================================================
  # Panel 3: Job Postings
  # ============================================================================
  
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
        airea_percentage = airea_posts / total_posts,  # Keep as decimal (0-1) for formatPercentage()
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
        `Commuting Zone` = gsub("^[0-9]+ ", "", gsub(" CZ$", "", `Commuting Zone`)),  # Remove ID numbers and " CZ"
        `Total Posts` = format(`Total Posts`, big.mark = ","),
        `AIREA Posts` = format(`AIREA Posts`, big.mark = ","),
        # Keep AIREA % as numeric for proper sorting
        `Posts per 1000` = sprintf("%.1f", `Posts per 1000`)
      )

    DT::datatable(my_df_demand,
                  selection = list(mode = 'single', selected = 1),  # Select first row by default
                  options = list(
                    pageLength = 10,
                    lengthChange = FALSE
                  ),
                  style="bootstrap"
    ) %>%
    DT::formatPercentage(columns = "AIREA %", digits = 1)  # Format as percentage while keeping numeric sorting
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
      mutate(airea_percentage = (airea_posts / total_posts) * 100) %>%  # Keep as percentage (0-100) for plot display
      ggplot(aes(x = YEAR, y = airea_percentage)) +
      geom_line(color = "#31a2b6", linewidth = 1) +
      geom_point(color = "#31a2b6", size = 2) +
      theme_minimal() +
      labs(
        title = paste("AIREA Job Posting Percentage Over Time:", gsub("^[0-9]+ ", "", gsub(" CZ$", "", my_cz$CZ_label))),
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
    
    # Get total posts for the CZ (for percentage calculation)
    cz_total_posts <- demand %>%
      filter(CZ_label == my_cz$CZ_label, YEAR <= 2024) %>%
      summarise(total_cz_posts = sum(TOTAL_JOB_POSTS, na.rm = TRUE)) %>%
      pull(total_cz_posts)
    
    # Create treemap data for top AIREA SOCs for the selected CZ (aggregated across all years, excluding 2025)
    treemap_data <- demand %>%
      filter(CZ_label == my_cz$CZ_label, YEAR <= 2024, AIREA == 1) %>%
      group_by(SOC_CODE) %>%
      summarise(
        total_posts = sum(TOTAL_JOB_POSTS, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        percentage_of_all_posts = (total_posts / cz_total_posts) * 100
      ) %>%
      arrange(desc(total_posts))
    
    # Get top 5 and calculate "Other"
    top_5_data <- treemap_data %>% head(5)
    other_posts <- treemap_data %>% slice(6:n()) %>% summarise(total_posts = sum(total_posts, na.rm = TRUE)) %>% pull(total_posts)
    other_percentage <- (other_posts / cz_total_posts) * 100
    
    # Add "Other" row if there are additional SOCs
    if (other_posts > 0) {
      other_row <- data.frame(
        SOC_CODE = "Other",
        total_posts = other_posts,
        percentage_of_all_posts = other_percentage
      )
      treemap_data <- rbind(top_5_data, other_row)
    } else {
      treemap_data <- top_5_data
    }
    
    # Join with SOC labels if available (for top 5 only)
    if (!is.null(soc_labels) && nrow(soc_labels) > 0) {
      treemap_data <- treemap_data %>%
        left_join(soc_labels %>% select(soc, soc2018title) %>% distinct(), by = c("SOC_CODE" = "soc")) %>%
        mutate(
          SOC_Label = ifelse(SOC_CODE == "Other", 
                            "Other AIREA SOCs",
                            ifelse(!is.na(soc2018title), 
                                   paste0("SOC ", SOC_CODE, ": ", soc2018title),
                                   paste0("SOC ", SOC_CODE)))
        )
    } else {
      treemap_data <- treemap_data %>%
        mutate(SOC_Label = ifelse(SOC_CODE == "Other", 
                                 "Other AIREA SOCs",
                                 paste0("SOC ", SOC_CODE)))
    }
    
    # Create treemap
    plot_ly(
      data = treemap_data,
      type = "treemap",
      labels = ~SOC_Label,
      parents = ~"AIREA SOCs",
      values = ~total_posts,
      textinfo = "label+text",
      text = ~paste0(sprintf("%.1f%%", percentage_of_all_posts)),
      textfont = list(size = 14, color = "white"),
      hovertemplate = paste(
        "<b>%{label}</b><br>",
        "Total Posts: %{value:,}<br>",
        "Percentage of All Posts: %{customdata:.1f}%<br>",
        "<extra></extra>"
      ),
      customdata = ~percentage_of_all_posts
    ) %>%
      layout(
        title = paste("Top 5 AIREA SOCs for", gsub("^[0-9]+ ", "", gsub(" CZ$", "", my_cz$CZ_label)), "(All Years through 2024)"),
        margin = list(t = 50, l = 25, r = 25, b = 25)
      )
  })
}