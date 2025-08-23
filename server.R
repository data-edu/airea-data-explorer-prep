# ==============================================================================
# Load Data
# ==============================================================================

library(shiny)
library(dplyr)
library(tidyr)
library(scales)
library(arrow)
library(ggplot2)
library(plotly)
library(DT)


# ==============================================================================
# Load supply data (institution completions)
# ==============================================================================

## map supply data for tab 1 only(Don't delete, used in map)
mapsupply <- readRDS("prep/mapsupply.rds")

# Tab 2 table source (replace interactive table with this CSV)
supply_table_df <- read.csv("prep/supply-table.csv", stringsAsFactors = FALSE)

# Arrow dataset for Tab 2 line chart (partitioned parquet)
open_ds <- open_dataset("prep/supply_partitioned", format = "parquet")

# ==============================================================================
# Load demand data (job postings)
# ==============================================================================

# Arrow dataset for Tab 3 demand line chart
demand_ds <- open_dataset("prep/demand_partitioned", format = "parquet")
# Tab 3 table source (CSV summary for CZs)

cz_table_df <- read_csv("prep/cz-summary-table.csv")

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
  
  # Tab 2 table now uses prebuilt CSV loaded as supply_table_df
  
  # Reactive expression for selected institution (from CSV-backed table)
  selected_institution <- reactive({
    req(input$supply_table_rows_selected)
    data.frame(
      instnm = supply_table_df$institution[input$supply_table_rows_selected],
      stringsAsFactors = FALSE
    )
  })
  
  # All institutions table now displays the CSV contents directly
  output$supply_table <- DT::renderDT({
    table_display <- supply_table_df %>%
      select(
        `Institution` = institution,
        `CZ Label` = cz_label,
        `Total Completions` = mean_completions,
        `AIREA Completions` = mean_airea_completions,
        `AIREA%` = pct_airea_completions,
        `Total Enrollment` = mean_students_enrolled,
        `Rural` = rural,
        `Tribal` = tribal
      ) %>%
      mutate(
        `AIREA%_num` = `AIREA%` * 100,
        `AIREA%` = paste0(round(`AIREA%_num`, 2), "%"),
        `Total Completions` = scales::comma(round(`Total Completions`)),
        `AIREA Completions` = scales::comma(round(`AIREA Completions`)),
        `Total Enrollment` = scales::comma(round(`Total Enrollment`)),
        `Rural` = scales::comma(round(`Rural`)),
        `Tribal` = scales::comma(round(`Tribal`))
      )

    DT::datatable(
      table_display,
      selection = list(mode = 'single', selected = 1),
      options = list(
        pageLength = 10,
        lengthChange = FALSE,
        order = list(list(8, 'desc')),
        columnDefs = list(list(visible = FALSE, targets = c(8)))
      ),
      style = "bootstrap",
      rownames = FALSE,
      escape = FALSE
    )
  })
  
  # Institution time series plot (using Arrow dataset and partitioned parquet)
  output$supply_degrees_by_institution <- renderPlot({
    req(selected_institution())

    my_inst <- selected_institution()

    # Load selected institution records from Arrow dataset
    selected_instnm <- open_ds %>%
      filter(instnm == my_inst$instnm) %>%
      collect()

    if (nrow(selected_instnm) == 0) return(NULL)

    plot_df <- selected_instnm %>%
      group_by(year) %>%
      summarize(
        total_completions = sum(total_completions, na.rm = TRUE),
        total_students_enrolled = sum(total_students_enrolled, na.rm = TRUE),
        total_airea_completions = sum(airea_completions, na.rm = TRUE),
        .groups = "drop"
      )

    if (nrow(plot_df) == 0) return(NULL)

    year_min <- suppressWarnings(min(plot_df$year, na.rm = TRUE))
    year_max <- suppressWarnings(max(plot_df$year, na.rm = TRUE))
    year_breaks <- if (is.finite(year_min) && is.finite(year_max) && year_min <= year_max)
      seq(year_min, year_max, 1) else NULL

    # Choose metric based on toggle
    metric <- input$supply_metric
    if (is.null(metric) || !(metric %in% c("airea", "pct"))) metric <- "airea"

    plot_df <- plot_df %>%
      mutate(
        airea_pct = ifelse(total_completions > 0,
                            (total_airea_completions / total_completions) * 100,
                            NA_real_)
      )

    y_col <- if (metric == "pct") "airea_pct" else "total_airea_completions"
    y_label <- if (metric == "pct") "AIREA Completion Percentage (%)" else "AIREA Completions"
    title_txt <- if (metric == "pct") "AIREA Percentage Over Time" else "AIREA Completions Over Time"

    p <- ggplot(plot_df, aes(x = year, y = .data[[y_col]])) +
      geom_line(linewidth = 1.1, color = "#0072B2") +
      geom_point(size = 2, color = "#0072B2") +
      theme_bw() +
      { if (!is.null(year_breaks)) scale_x_continuous(breaks = year_breaks) else NULL } +
      labs(
        title = title_txt,
        x = NULL,
        y = y_label
      ) +
      theme_minimal(base_size = 13)

    if (metric == "pct") {
      p <- p + scale_y_continuous(labels = function(x) paste0(x, "%"))
    }

    p
  })
  
  # CIP by award level stacked bar (most recent year for selected institution)
  output$supply_cip_award_bar <- renderPlot({
    req(selected_institution())

    my_inst <- selected_institution()

    # Determine most recent year for this institution from Arrow dataset
    inst_years <- open_ds %>%
      filter(instnm == my_inst$instnm) %>%
      summarize(max_year = max(year, na.rm = TRUE)) %>%
      collect()

    if (nrow(inst_years) == 0 || is.na(inst_years$max_year)) return(NULL)
    most_recent_year <- inst_years$max_year[[1]]

    selected_instm_year <- open_ds %>%
      filter(instnm == my_inst$instnm, year == most_recent_year) %>%
      collect()

    if (nrow(selected_instm_year) == 0) return(NULL)

    plot_df <- selected_instm_year %>%
      group_by(ciptitle, award_level) %>%
      summarize(total_airea_completions = sum(airea_completions, na.rm = TRUE), .groups = "drop") %>%
      filter(total_airea_completions > 0) %>%
      mutate(
        award_level = haven::as_factor(award_level, levels = "labels"),
        award_level = forcats::fct_rev(award_level)
      )

    if (nrow(plot_df) == 0) return(NULL)

    ggplot(plot_df, aes(x = reorder(ciptitle, total_airea_completions),
                        y = total_airea_completions,
                        fill = award_level)) +
      geom_col(position = "fill") +
      coord_flip() +
      theme_bw() +
      labs(y = "Total AIREA Completions", x = NULL, fill = "Award level") +
      scale_fill_brewer("", palette = "Set1") +
      theme(legend.position = "top")
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
  
  # Reactive expression for selected CZ (prefers CSV-backed table if available)
  selected_cz <- reactive({
    req(input$demand_table_rows_selected)
    if (nrow(cz_table_df) > 0) {
      # Try to locate a CZ label column in the CSV
      name_map <- names(cz_table_df)
      lower_names <- tolower(name_map)
      candidates <- c("cz_label", "cz label", "commuting zone", "commuting_zone", "czlabel")
      match_idx <- match(candidates, lower_names)
      match_idx <- match_idx[!is.na(match_idx)]
      if (length(match_idx) > 0) {
        label_col <- name_map[match_idx[1]]
        cz_val <- cz_table_df[[label_col]][input$demand_table_rows_selected]
        return(data.frame(CZ_label = as.character(cz_val), stringsAsFactors = FALSE))
      }
    }
    req(demand_year_data())
    demand_year_data()[input$demand_table_rows_selected, "CZ_label"]
  })
  
  # All CZs table: prefer CSV if present; otherwise fall back to computed table
  output$demand_table <- DT::renderDT({
    if (nrow(cz_table_df) > 0) {
      df <- cz_table_df
      # Identify label and percent columns if present
      lower_names <- tolower(names(df))
      label_idx <- match(c("cz_label", "cz label", "commuting zone", "commuting_zone", "czlabel"), lower_names)
      label_idx <- label_idx[!is.na(label_idx)]
      pct_idx <- match(c("airea %", "airea%", "pct_airea", "pct_airea_posts", "airea pct", "airea_percentage"), lower_names)
      pct_idx <- pct_idx[!is.na(pct_idx)]

      # Reorder columns to put label first if found
      if (length(label_idx) > 0) {
        first_col <- label_idx[1]
        df <- df[, c(first_col, setdiff(seq_len(ncol(df)), first_col)), drop = FALSE]
        names(df)[1] <- "Commuting Zone"
      }

      # Format numeric columns: integers with commas; decimals rounded(2); percent to percentage strings
      for (col in names(df)) {
        if (is.numeric(df[[col]])) {
          is_pct_col <- length(pct_idx) > 0 && match(tolower(col), lower_names[pct_idx], nomatch = 0) > 0
          if (is_pct_col) {
            vals <- df[[col]]
            vals_scaled <- if (all(vals <= 1, na.rm = TRUE)) vals * 100 else vals
            df[[col]] <- paste0(round(vals_scaled, 2), "%")
          } else if (all(df[[col]] == floor(df[[col]]), na.rm = TRUE)) {
            df[[col]] <- scales::comma(df[[col]])
          } else {
            df[[col]] <- round(df[[col]], 2)
          }
        }
      }

      order_list <- list()
      if (length(pct_idx) > 0) {
        # Adjust for label moved to first position
        pct_col_name <- names(cz_table_df)[pct_idx[1]]
        pct_display_idx <- match(pct_col_name, names(df)) - 1  # 0-based for DataTables
        if (!is.na(pct_display_idx) && pct_display_idx >= 0) {
          order_list <- list(list(pct_display_idx, 'desc'))
        }
      }

      # Rename common columns for readability if present
      col_map <- c(
        total_posts = "Total Posts",
        airea_posts = "AIREA Posts",
        airea_percentage = "AIREA%",
        posts_per_1000 = "Posts per 1,000",
        posts_per_100k = "Posts per 100,000"
      )
      for (nm in names(col_map)) {
        idx <- which(tolower(names(df)) == nm)
        if (length(idx) == 1) names(df)[idx] <- col_map[[nm]]
      }

      DT::datatable(
        df,
        selection = list(mode = 'single', selected = 1),
        options = list(
          pageLength = 10,
          lengthChange = FALSE,
          order = order_list
        ),
        style = "bootstrap",
        rownames = FALSE
      )
    } else {
      my_df_demand <- demand_year_data() %>%
        select(
          `Commuting Zone` = CZ_label,
          `Total Posts` = total_posts,
          `AIREA Posts` = airea_posts,
          `AIREA %` = airea_percentage,
          `Posts per 1000` = posts_per_1000
        ) %>%
        mutate(
          `Commuting Zone` = gsub("^[0-9]+ ", "", gsub(" CZ$", "", `Commuting Zone`)),
          `Total Posts` = format(`Total Posts`, big.mark = ","),
          `AIREA Posts` = format(`AIREA Posts`, big.mark = ","),
          `Posts per 1000` = sprintf("%.1f", `Posts per 1000`)
        )

      DT::datatable(my_df_demand,
                    selection = list(mode = 'single', selected = 1),
                    options = list(
                      pageLength = 10,
                      lengthChange = FALSE
                    ),
                    style="bootstrap"
      ) %>%
      DT::formatPercentage(columns = "AIREA %", digits = 1)
    }
  })
  
  # CZ time series plot (with metric toggle)
  output$demand_cz_trend <- renderPlot({
    req(selected_cz())
    
    my_cz <- selected_cz()
    
    # Pull time series from partitioned dataset, exclude 2025
    demand_selected <- demand_ds %>%
      filter(cz_label == my_cz$CZ_label) %>%
      collect() %>%
      filter(year != 2025) %>%
      group_by(year) %>%
      summarise(
        posts_total = sum(total_job_postings, na.rm = TRUE),
        posts_airea = sum(total_job_postings[airea == 1], na.rm = TRUE),
        pop_year    = mean(mean_population, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        posts_per_100k = ifelse(is.na(pop_year) | pop_year == 0, NA_real_, (posts_airea / pop_year) * 100000),
        airea_pct = ifelse(posts_total > 0, (posts_airea / posts_total) * 100, NA_real_)
      )

    if (nrow(demand_selected) == 0) return(NULL)

    metric <- input$demand_metric
    if (is.null(metric) || !(metric %in% c("airea", "pct", "per100k"))) metric <- "airea"

    y_col <- switch(metric,
                    airea = "posts_airea",
                    pct = "airea_pct",
                    per100k = "posts_per_100k")
    y_label <- switch(metric,
                      airea = "AIREA job posts",
                      pct = "AIREA job posts (%)",
                      per100k = "AIREA job posts per 100,000")
    title_txt <- paste("AIREA Job Posts Over Time —", gsub("^[0-9]+ ", "", gsub(" CZ$", "", my_cz$CZ_label)))

    p <- ggplot(demand_selected, aes(x = year, y = .data[[y_col]])) +
      geom_line(linewidth = 1.1, color = "#0072B2") +
      geom_point(size = 2, color = "#0072B2") +
      theme_minimal(base_size = 13) +
      labs(
        title = title_txt,
        x = NULL,
        y = y_label
      )

    if (metric == "airea") {
      p <- p + scale_y_continuous(labels = scales::comma)
    } else if (metric == "pct") {
      p <- p + scale_y_continuous(labels = function(x) paste0(x, "%"))
    }

    p
  })
  
  # Demand stacked bar: Top occupations by postings, stacked by education requirement (most recent year)
  output$demand_soc_edreq_bar <- renderPlot({
    req(selected_cz())

    my_cz <- selected_cz()

    # Determine most recent year available for this CZ
    cz_years <- demand_ds %>%
      filter(cz_label == my_cz$CZ_label, year != 2025) %>%
      summarise(max_year = max(year, na.rm = TRUE)) %>%
      collect()

    if (nrow(cz_years) == 0 || is.na(cz_years$max_year)) return(NULL)
    most_recent_year <- cz_years$max_year[[1]]

    plot_df <- demand_ds %>%
      filter(cz_label == my_cz$CZ_label, year == most_recent_year) %>%
      collect() %>%
      filter(!is.na(soc_title)) %>%
      filter(airea == 1) %>%
      select(soc_title, ed_req, total_job_postings) %>%
      mutate(
        ed_req = haven::as_factor(ed_req, levels = "labels"),
        ed_req = forcats::fct_rev(ed_req),
        ed_req = forcats::fct_na_value_to_level(ed_req, "Missing")
      ) %>%
      group_by(soc_title, ed_req) %>%
      summarise(total_postings = sum(total_job_postings, na.rm = TRUE), .groups = "drop") %>%
      group_by(soc_title) %>%
      mutate(total_soc = sum(total_postings)) %>%
      ungroup() %>%
      slice_max(order_by = total_soc, n = 20, with_ties = FALSE) %>%
      mutate(soc_title = forcats::fct_reorder(soc_title, total_soc))

    if (nrow(plot_df) == 0) return(NULL)

    ggplot(plot_df, aes(x = soc_title, y = total_postings, fill = ed_req)) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      labs(
        x = NULL,
        y = "Number of job postings",
        fill = "Education requirement",
        title = "Education requirement share by occupation"
      ) +
      theme_bw() +
      theme(legend.position = "top") +
      scale_fill_brewer("", palette = "Set1") +
      guides(fill = guide_legend(nrow = 3, byrow = TRUE))
  })
}