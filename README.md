library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(DT)
library(tidyr)

ui <- fluidPage(
  titlePanel("DHD Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file_upload", "Upload CSV", accept = ".csv"),
      selectInput("player", "Select Player", choices = character(0)),
      selectInput(
        "swing_filter",
        "Swing Filter:",
        choices = c(
          "All Pitches" = "all",
          "Swings Only" = "swing",
          "Takes Only" = "take",
          "Swing & Miss Only" = "miss",
          "Contact Only" = "contact"
        ),
        selected = "all"
      ),
      checkboxGroupInput("pitch_type", "Select Pitch Type(s)", choices = character(0)),
      sliderInput(
        "date_range",
        "Select Date Range",
        min = Sys.Date(),
        max = Sys.Date(),
        value = c(Sys.Date(), Sys.Date()),
        timeFormat = "%Y-%m-%d"
      ),
      sliderInput(
        "exit_velocity",
        "Select Exit Velocity Range",
        min = 0,
        max = 120,
        value = c(0, 120)
      ),
      radioButtons(
        "plot_type",
        "Select Plot Type:",
        choices = c("Points" = "point", "Density" = "density"),
        selected = "point"
      ),
      
      tags$hr(),
      h4("Custom Metric Filter"),
      selectInput("custom_metric", "Select Metric to Filter:", choices = NULL),
      numericInput(
        "custom_min",
        "Minimum Value:",
        value = NA,
        step = 1
      ),
      numericInput(
        "custom_max",
        "Maximum Value:",
        value = NA,
        step = 1
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Strike Zone Heatmap", plotlyOutput("strike_zone_heatmap")),
        tabPanel("Contact Points", plotlyOutput("contact_point_chart")),
        tabPanel("Spray Chart", plotlyOutput("spray_chart")),
        tabPanel(
          "Stats",
          selectInput("stats_player", "Select Player for Stats", choices = character(0)),
          DTOutput("stats_table"),
          
          tags$hr(),
          h4("Multiple Regression Builder"),
          
          selectInput("y_var", "Select Target Variable (Y):", choices = NULL),
          selectizeInput(
            "x_vars",
            "Select Predictor Variables (X):",
            choices = NULL,
            multiple = TRUE
          ),
          actionButton("run_model", "Run Regression"),
          
          tags$br(),
          verbatimTextOutput("model_summary"),
          plotlyOutput("prediction_plot"),
          
          tags$hr(),
          h4("Game vs Practice Correlation"),
          
          # Automatically merged correlation from main CSV
          verbatimTextOutput("correlation_result"),
          plotlyOutput("correlation_plot"),
          
          tags$hr(),
          h4("Manual Game vs Practice Correlation"),
          
          # Upload Game & Practice separately
          fileInput("game_file", "Upload Game CSV", accept = ".csv"),
          fileInput("practice_file", "Upload Practice CSV", accept = ".csv"),
          
          conditionalPanel(
            condition = "input.game_file.name && input.practice_file.name",
            selectInput("game_col", "Select Game Metric:", choices = NULL),
            selectInput("practice_col", "Select Practice Metric:", choices = NULL),
            verbatimTextOutput("manual_correlation_result"),
            plotlyOutput("manual_correlation_plot")
          )
        ),
        tabPanel(
          "Performance Histograms",
          selectInput("hist_player", "Select Player", choices = character(0)),
          selectInput(
            "hist_metric",
            "Select Metric",
            choices = c(
              "Exit Velocity" = "ExitVelocity",
              "Launch Angle" = "LaunchAngle",
              "Exit Direction" = "ExitDirection",
              "Bat Speed" = "Bat Speed",
              "Attack Angle (deg)" = "Attack Angle (deg)",
              "Early Connection (deg)" = "Early Connection (deg)",
              "Connection at Impact (deg)" = "Connection at Impact (deg)",
              "Vertical Bat Angle (deg)" = "Vertical Bat Angle (deg)",
              "Time to Contact (sec)" = "Time to Contact (sec)",
              "Squared Up %" = "Squared Up %"
            ),
            # ✅ Added "Squared Up %"
            selected = "ExitVelocity"
          ),
          plotlyOutput("performance_histogram")
        ),
        
        # ✅ Correct placement of Biomechanics Histograms tab
        tabPanel(
          "Biomechanics Histograms",
          selectInput(
            "biomech_metric",
            "Select Biomechanics Metric",
            choices = c(
              "chest__gain_from_pelvis_speed",
              "arm__gain_from_chest_speed",
              "spine__stability",
              "pelvis__peak_rot_speed",
              "chest__peak_rot_speed",
              "arm__peak_rot_speed",
              "spine__max_separation",
              "chest__chest_delay",
              "pelvis__peak_rot_acceleration",
              "chest__peak_rot_acceleration",
              "arm__peak_rot_acceleration",
              "bat__gain_from_arm_speed",
              "bat__peak_rot_speed",
              "bat__speed_to_impact",
              "bat__peak_rot_acceleration",
              "bat__bat_to_spine_angle"
            ),
            selected = "chest__gain_from_pelvis_speed"
          ),
          plotlyOutput("biomech_histogram")
        ),
        
        # ✅ Correct placement of Biomechanics Regressions tab
        tabPanel(
          "Biomechanics Regressions",
          selectInput(
            "regression_x",
            "Biomechanics Variable",
            choices = c(
              "spine__stability",
              "pelvis_forward_bend__impact",
              "pelvis_rotation__impact",
              "pelvis_side_bend__impact",
              "chest_forward_bend__impact",
              "chest_rotation__impact",
              "chest_side_bend__impact",
              "bat__bat_horizontal_angle_at_impact",
              "bat__vertical_angle_at_impact",
              "bat__bat_time",
              "bat__attack_angle"
            ),
            selected = "spine__stability"
          ),
          
          selectInput(
            "regression_y",
            "Performance Metric",
            choices = c("launch angle", "exit direction", "exit velocity", "bat speed"),
            selected = "launch angle"
          ),
          
          plotlyOutput("biomech_regression")
        )
      ) # Closing tabsetPanel()
    ) # Closing mainPanel
  ) # Closing sidebarLayout
) # Closing fluidPage()




server <- function(input, output, session) {
  data <- reactiveVal()
  
  # — 1) Update dropdown when game file is uploaded —
  observeEvent(input$game_file, {
    req(input$game_file)
    game_df <- read_csv(input$game_file$datapath)
    updateSelectInput(session, "game_col", choices = names(game_df))
  })
  
  # — 2) Update dropdown when practice file is uploaded —
  observeEvent(input$practice_file, {
    req(input$practice_file)
    practice_df <- read_csv(input$practice_file$datapath)
    updateSelectInput(session, "practice_col", choices = names(practice_df))
  })
  
  # — 3) Reactive: read & filter game data once —
  filtered_game_data <- reactive({
    req(input$game_file)
    game_df <- read_csv(input$game_file$datapath)
    
    if ("Player Name.y" %in% names(game_df) && input$player != "") {
      game_df <- game_df %>% filter(`Player Name.y` == input$player)
    }
    if ("Pitch Type" %in% names(game_df) && !is.null(input$pitch_type)) {
      game_df <- game_df %>% filter(`Pitch Type` %in% input$pitch_type)
    }
    if ("Date" %in% names(game_df)) {
      game_df <- game_df %>%
        filter(Date >= input$date_range[1], Date <= input$date_range[2])
    }
    if ("ExitVelocity" %in% names(game_df)) {
      game_df <- game_df %>%
        filter(ExitVelocity >= input$exit_velocity[1],
               ExitVelocity <= input$exit_velocity[2])
    }
    
    game_df
  })
  
  # — 4) Reactive: read & filter practice data once —
  filtered_practice_data <- reactive({
    req(input$practice_file)
    practice_df <- read_csv(input$practice_file$datapath)
    
    if ("Player Name.y" %in% names(practice_df) && input$player != "") {
      practice_df <- practice_df %>% filter(`Player Name.y` == input$player)
    }
    if ("Pitch Type" %in% names(practice_df) && !is.null(input$pitch_type)) {
      practice_df <- practice_df %>% filter(`Pitch Type` %in% input$pitch_type)
    }
    if ("Date" %in% names(practice_df)) {
      practice_df <- practice_df %>%
        filter(Date >= input$date_range[1], Date <= input$date_range[2])
    }
    if ("ExitVelocity" %in% names(practice_df)) {
      practice_df <- practice_df %>%
        filter(ExitVelocity >= input$exit_velocity[1],
               ExitVelocity <= input$exit_velocity[2])
    }
    
    practice_df
  })
  
  # — 5) Pearson correlation result —
  output$manual_correlation_result <- renderPrint({
    req(input$game_col, input$practice_col)
    game_df     <- filtered_game_data()
    practice_df <- filtered_practice_data()
    
    df <- tibble(
      Game     = game_df[[input$game_col]],
      Practice = practice_df[[input$practice_col]]
    ) %>% filter(!is.na(Game) & !is.na(Practice))
    
    if (nrow(df) < 2) {
      cat("⚠️ Not enough valid pairs after filtering.")
      return()
    }
    
    print(cor.test(df$Game, df$Practice, method = "pearson"))
  })
  
  # — 6) Pearson scatterplot with regression line —
  output$manual_correlation_plot <- renderPlotly({
    req(input$game_col, input$practice_col)
    game_df     <- filtered_game_data()
    practice_df <- filtered_practice_data()
    
    df <- tibble(
      Game     = game_df[[input$game_col]],
      Practice = practice_df[[input$practice_col]]
    ) %>% filter(!is.na(Game) & !is.na(Practice))
    
    if (nrow(df) < 2) return(NULL)
    
    p <- ggplot(df, aes(x = Practice, y = Game)) +
      geom_point(color = "blue", alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(
        title = paste("Filtered Correlation:", input$game_col, "vs", input$practice_col),
        x     = input$practice_col,
        y     = input$game_col
      )
    
    ggplotly(p)
  })
  
  # — 7) File‐upload processing (your original logic) —
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    df <- tryCatch(
      read_csv(
        input$file_upload$datapath,
        show_col_types = FALSE,
        locale = locale(encoding = "UTF-8")
      ),
      error = function(e) {
        print(paste("Error reading file:", e$message))
        return(NULL)
      }
    )
    req(!is.null(df))
    
    # Update metric choices
    numeric_cols <- names(df)[sapply(df, is.numeric)]
    colnames(df) <- gsub("^Swing \\?$", "Swing?", colnames(df))
    updateSelectInput(session, "custom_metric", choices = numeric_cols)
    updateSelectInput(session, "y_var", choices = numeric_cols)
    updateSelectizeInput(session, "x_vars", choices = numeric_cols)
    
    # Verify required columns
    required_cols <- c(
      "Date.x", "Date.y", "Player Name.y", "Pitch Type",
      "Strike Zone Side", "Strike Zone Height", "ExitVelocity",
      "Distance", "ExitDirection", "Contact Depth", "Swing?",
      "BIP", "LaunchAngle"
    )
    missing_cols <- setdiff(required_cols, colnames(df))
    if (length(missing_cols) > 0) {
      print(paste("Missing Columns:", paste(missing_cols, collapse = ", ")))
      return()
    }
    
    df <- df %>%
      mutate(
        `Contact Depth`      = suppressWarnings(as.numeric(gsub("^-?$", NA, `Contact Depth`))),
        `Strike Zone Side`   = suppressWarnings(as.numeric(gsub("^-?$", NA, `Strike Zone Side`))),
        `Strike Zone Height` = suppressWarnings(as.numeric(gsub("^-?$", NA, `Strike Zone Height`))),
        Date                  = as.Date(coalesce(Date.y, Date.x), format = "%Y-%m-%d"),
        ExitVelocity         = suppressWarnings(as.numeric(ExitVelocity)),
        Distance              = suppressWarnings(as.numeric(Distance)),
        ExitDirection        = suppressWarnings(as.numeric(ExitDirection)),
        LaunchAngle           = suppressWarnings(as.numeric(LaunchAngle)),
        `Swing Result`       = case_when(
          tolower(trimws(`Swing?`)) == "yes" & ExitVelocity == 0   ~ "Swing & Miss",
          tolower(trimws(`Swing?`)) == "yes" & ExitVelocity > 0   ~ "Contact",
          tolower(trimws(`Swing?`)) == "no"                       ~ "Take",
          TRUE                                                    ~ NA_character_
        )
      ) %>%
      drop_na(`Strike Zone Side`, `Strike Zone Height`) %>%
      filter(
        between(`Strike Zone Side`, -15, 15),
        between(`Strike Zone Height`, 5, 40),
        is.na(Distance) | Distance <= 450,
        is.na(ExitVelocity) | ExitVelocity >= 0
      ) %>%
      select(-Date.x, -Date.y)
    
    data(df)
    
    if (nrow(df) > 0) {
      updateSelectInput(session, "player", choices = c("", unique(df$`Player Name.y`)))
      updateSelectInput(session, "stats_player", choices = c("", unique(df$`Player Name.y`)))
      updateSelectInput(session, "hist_player", choices = c("", unique(df$`Player Name.y`)))
      updateCheckboxGroupInput(session, "pitch_type", choices = unique(df$`Pitch Type`))
      updateSliderInput(
        session, "date_range",
        min   = min(df$Date, na.rm = TRUE),
        max   = max(df$Date, na.rm = TRUE),
        value = c(min(df$Date, na.rm = TRUE), max(df$Date, na.rm = TRUE))
      )
    }
  })
}

                             

# ✅ Biomechanics Histogram (Fixed)
output$biomech_histogram <- renderPlotly({
  df <- data()
  req(df)  # Ensure data exists
  req(input$biomech_metric %in% colnames(df))  # Ensure selected column exists
  
  p <- ggplot(df, aes(.data[[input$biomech_metric]])) +  # ✅ Correct way to access dynamic columns
    geom_histogram(
      binwidth = 5,
      fill = "blue",
      color = "black",
      alpha = 0.7
    ) +
    theme_minimal() +
    labs(
      title = paste("Histogram of", input$biomech_metric),
      x = input$biomech_metric,
      y = "Frequency"
    )
  
  ggplotly(p)
})

# ✅ Biomechanics Regression Plot (Fixed)
output$biomech_regression <- renderPlotly({
  df <- data()
  req(df)  # Ensure data exists
  req(input$regression_x %in% colnames(df),
      input$regression_y %in% colnames(df))  # Ensure both columns exist
  
  p <- ggplot(df, aes(.data[[input$regression_x]], .data[[input$regression_y]])) +  # ✅ Correct dynamic column reference
    geom_point(alpha = 0.7, color = "blue") +
    geom_smooth(method = "lm", color = "red") +
    theme_minimal() +
    labs(
      title = paste("Regression: ", input$regression_x, " vs ", input$regression_y),
      x = input$regression_x,
      y = input$regression_y
    )
  
  ggplotly(p)
})


output$stats_table <- renderDT({
  df_player <- filtered_stats_data()
  if (nrow(df_player) == 0)
    return(data.frame())
  
  # -- 1) Filter for batted-ball events
  batted_ball_df <- df_player %>%
    filter(`Swing?` == "Yes", ExitVelocity > 0, !is.na(LaunchAngle))
  
  # -- 2) Pitch decision stats
  strikes_seen <- round(sum(df_player$`Is Strike` == "YES", na.rm = TRUE) /
                          max(sum(
                            df_player$`Is Strike` %in% c("YES", "NO"), na.rm = TRUE
                          ), 1) * 100,
                        1)
  swing_pct <- round(sum(df_player$`Swing?` == "Yes", na.rm = TRUE) /
                       max(nrow(df_player), 1) * 100,
                     1)
  in_zone_in_play_pct <- round(
    sum(df_player$BIP == "Yes" &
          df_player$`Is Strike` == "YES", na.rm = TRUE) /
      max(sum(df_player$`Is Strike` == "YES", na.rm = TRUE), 1) * 100,
    1
  )
  swing_miss_pct_iz <- round(
    sum(
      df_player$`Swing?` == "Yes" &
        df_player$ExitVelocity == 0 &
        df_player$`Is Strike` == "YES",
      na.rm = TRUE
    ) /
      max(
        sum(
          df_player$`Swing?` == "Yes" &
            df_player$`Is Strike` == "YES",
          na.rm = TRUE
        ),
        1
      ) * 100,
    1
  )
  
  # -- 3) Ideal attack-angle
  ideal_attack_angle_pct <- round(
    sum(
      df_player$`Swing?` == "Yes" &
        !is.na(df_player$AttackAngle) &
        df_player$AttackAngle >= 5 &
        df_player$AttackAngle <= 15,
      na.rm = TRUE
    ) /
      max(sum(
        df_player$`Swing?` == "Yes" &
          !is.na(df_player$AttackAngle),
        na.rm = TRUE
      ), 1) * 100,
    1
  )
  
  # -- 4) Quality Swing %
  #    swings within 7% of max bat speed, AA 0–15°, TTC < .15s
  max_bat_speed      <- max(df_player$BatSpeed, na.rm = TRUE)
  quality_swing_pct  <- round(
    sum(
      df_player$`Swing?` == "Yes" &
        !is.na(df_player$BatSpeed) &
        df_player$BatSpeed >= 0.93 * max_bat_speed &
        !is.na(df_player$AttackAngle) &
        df_player$AttackAngle >= 0 &
        df_player$AttackAngle <= 15 &
        !is.na(df_player$TimeToContact) &
        df_player$TimeToContact < 0.15,
      na.rm = TRUE
    ) /
      max(sum(df_player$`Swing?` == "Yes", na.rm = TRUE), 1) * 100,
    1
  )
  
  # -- 5) Batted-ball profile
  line_drive_rate  <- round(
    sum(
      batted_ball_df$LaunchAngle >= 10 &
        batted_ball_df$LaunchAngle <= 25,
      na.rm = TRUE
    ) /
      max(nrow(batted_ball_df), 1) * 100,
    1
  )
  ground_ball_rate <- round(sum(batted_ball_df$LaunchAngle < 10, na.rm = TRUE) /
                              max(nrow(batted_ball_df), 1) * 100,
                            1)
  fly_ball_rate    <- round(sum(batted_ball_df$LaunchAngle > 25, na.rm = TRUE) /
                              max(nrow(batted_ball_df), 1) * 100,
                            1)
  gb_fb_ratio      <- ifelse(sum(batted_ball_df$LaunchAngle > 25, na.rm = TRUE) > 0,
                             round(
                               sum(batted_ball_df$LaunchAngle < 10, na.rm = TRUE) /
                                 max(sum(batted_ball_df$LaunchAngle > 25, na.rm = TRUE), 1),
                               1
                             ),
                             NA)
  
  # -- 6) EV & Blast metrics
  ev_90          <- round(quantile(df_player$ExitVelocity, probs = 0.9, na.rm = TRUE), 1)
  squared_up_pct <- round(mean(df_player$`Squared Up %`, na.rm = TRUE), 1)
  blast_pct      <- round(ev_90 * (squared_up_pct / 100), 2)
  
  blast_ball_df  <- df_player %>%
    filter(
      !is.na(ExitVelocity),
      ExitVelocity > 0,!is.na(Distance),
      Distance    > 0,!is.na(`Squared Up %`)
    )
  blast_values   <- with(blast_ball_df, ExitVelocity * Distance * (`Squared Up %` /
                                                                     100))
  blast_composite <- if (length(blast_values) > 0) {
    round(exp(mean(log(blast_values), na.rm = TRUE)), 1)
  } else
    NA
  
  # -- 7) Assemble and render with grouping + caption
  stats_df <- data.frame(
    Group     = c(
      rep("Composite Metrics", 2),
      rep("Pitch Decision", 7),
      rep("Batted Ball Profile", 4),
      "Composite Metrics"
    ),
    Statistic = c(
      "90th Percentile EV",
      "Blast % (Bat speed × SQ%)",
      
      "Strikes Seen (%)",
      "BIP Count",
      "Swing (%)",
      "In-Zone In-Play (%)",
      "Swing & Miss % (IZ)",
      "Ideal Attack Angle % (5–15°)",
      "Quality Swing % (≥93% max speed, 0–15° AA, <0.15s TTC)",
      
      "Line Drive Rate (%)",
      "Ground Ball Rate (%)",
      "Fly Ball Rate (%)",
      "GB/FB Ratio",
      
      "Blast Composite Score"
    ),
    Value     = c(
      ev_90,
      blast_pct,
      
      strikes_seen,
      sum(df_player$BIP == "Yes", na.rm = TRUE),
      swing_pct,
      in_zone_in_play_pct,
      swing_miss_pct_iz,
      ideal_attack_angle_pct,
      quality_swing_pct,
      
      line_drive_rate,
      ground_ball_rate,
      fly_ball_rate,
      gb_fb_ratio,
      
      blast_composite
    ),
    stringsAsFactors = FALSE
  )
  
  datatable(
    stats_df,
    caption    = htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: center;',
      "MLB averages: 90th EV = 105; Blast % = 18%; Blast Composite = 100"
    ),
    extensions = "RowGroup",
    options    = list(pageLength = 15, rowGroup   = list(dataSrc = 0)),
    rownames = FALSE
  )
})


filtered_histogram_data <- reactive({
  req(data(), input$hist_player, input$hist_metric)  # Ensure required inputs exist
  
  df_hist <- data()
  
  # ✅ Apply filters only if selections are made
  if (!is.null(input$hist_player) && input$hist_player != "") {
    df_hist <- df_hist %>% filter(`Player Name.y` == input$hist_player)
  }
  if (!is.null(input$pitch_type) &&
      length(input$pitch_type) > 0) {
    df_hist <- df_hist %>% filter(`Pitch Type` %in% input$pitch_type)
  }
  df_hist <- df_hist %>%
    filter(
      Date >= input$date_range[1],
      Date <= input$date_range[2],
      ExitVelocity >= input$exit_velocity[1],
      ExitVelocity <= input$exit_velocity[2]
    )
  
  print(paste("Filtered histogram data rows:", nrow(df_hist)))  # ✅ Debugging output
  return(df_hist)  # ✅ Always return a dataframe, even if empty
})


# ✅ Now define `filtered_data()`
filtered_data <- reactive({
  req(data(), input$player, input$pitch_type)
  df <- data() %>%
    filter(
      `Player Name.y` == input$player,
      `Pitch Type` %in% input$pitch_type,
      Date >= input$date_range[1],
      Date <= input$date_range[2],
      ExitVelocity >= input$exit_velocity[1],
      ExitVelocity <= input$exit_velocity[2]
    )
  return(df)
})
output$spray_chart <- renderPlotly({
  df <- filtered_data()
  
  # ✅ Apply custom metric filter if selected
  if (!is.null(input$custom_metric) &&
      input$custom_metric %in% names(df) &&
      !is.na(input$custom_min) && !is.na(input$custom_max)) {
    df <- df %>% filter(.data[[input$custom_metric]] >= input$custom_min, .data[[input$custom_metric]] <= input$custom_max)
  }
  
  # ✅ Debugging: Print number of rows to ensure data is available
  print(paste("Filtered spray chart data rows:", nrow(df)))
  
  if (nrow(df) == 0) {
    print("⚠ No data available for Spray Chart!")
    return(NULL)  # Prevents error
  }
  
  # ✅ Drop NA values from critical columns
  df <- df %>%
    drop_na(ExitDirection, Distance) %>%
    mutate(
      x = Distance * sin(ExitDirection * pi / 180),
      # Convert to X coordinate
      y = Distance * cos(ExitDirection * pi / 180)   # Convert to Y coordinate
    )
  
  # ✅ Debugging: Print first few rows after conversion
  print(head(df))
  
  plot <- ggplot(df, aes(x = x, y = y)) +
    geom_point(aes(color = ExitVelocity), size = 1.5, alpha = 0.7) +
    scale_color_gradient(low = "blue", high = "red") +
    labs(title = "Spray Chart", x = "Left/Right Field", y = "Distance (Feet)") +
    theme_minimal() +
    xlim(-250, 250) +  # Keep within baseball field shape
    ylim(0, 450)  # Ensure distance is valid
  
  ggplotly(plot)
})


output$contact_point_chart <- renderPlotly({
  df <- filtered_data()
  
  if (nrow(df) == 0) {
    print("⚠ No data available for Contact Point Chart!")
    return(NULL)  # Prevents errors
  }
  
  # ✅ Apply custom metric filter if selected
  if (!is.null(input$custom_metric) &&
      input$custom_metric %in% names(df) &&
      !is.na(input$custom_min) && !is.na(input$custom_max)) {
    df <- df %>% filter(.data[[input$custom_metric]] >= input$custom_min, .data[[input$custom_metric]] <= input$custom_max)
  }
  df <- df %>% filter(`Contact Depth` <= 70)
  
  plot <- ggplot(df, aes(x = `Strike Zone Side`, y = `Contact Depth`)) +
    coord_fixed(ratio = 1) +
    theme_minimal() +
    labs(title = "Contact Point Chart", x = "Horizontal Location", y = "Point of Contact Depth")
  
  if (input$plot_type == "point") {
    plot <- plot + geom_point(
      aes(color = ExitVelocity),
      alpha = 0.5,
      size = 2,
      na.rm = TRUE
    ) +
      scale_color_gradient(low = "blue", high = "red")
  } else {
    plot <- plot + stat_density2d(
      geom = "raster",
      aes(fill = after_stat(density)),
      contour = FALSE,
      na.rm = TRUE
    ) +
      scale_fill_gradientn(colours = c("#f7f7f7", "#fee391", "#fec44f", "#fe9929", "#993404"))
  }
  
  ggplotly(plot)
})

output$strike_zone_heatmap <- renderPlotly({
  df <- filtered_data()
  # ✅ Apply custom metric filter if selected
  if (!is.null(input$custom_metric) &&
      input$custom_metric %in% names(df) &&
      !is.na(input$custom_min) && !is.na(input$custom_max)) {
    df <- df %>% filter(.data[[input$custom_metric]] >= input$custom_min, .data[[input$custom_metric]] <= input$custom_max)
  }
  
  if (nrow(df) == 0)
    return(NULL)
  
  # ✅ Normalize Swing? column
  if ("Swing?" %in% names(df)) {
    df <- df %>%
      mutate(`Swing?` = trimws(tolower(`Swing?`)))  # Normalize for safety
    
    if (input$swing_filter == "swing") {
      df <- df %>% filter(`Swing?` == "yes")
    } else if (input$swing_filter == "take") {
      df <- df %>% filter(`Swing?` == "no")
    } else if (input$swing_filter == "miss") {
      df <- df %>% filter(`Swing Result` == "Swing & Miss")
    } else if (input$swing_filter == "contact") {
      df <- df %>% filter(`Swing Result` == "Contact")  # ✅ New condition
    }
  }
  
  
  # ✅ Only now filter for valid plotting rows
  df <- df %>% filter(!is.na(`Strike Zone Side`), !is.na(`Strike Zone Height`))
  
  if (nrow(df) == 0)
    return(NULL)
  
  
  
  # ✅ Normalize Swing? values for robust filtering
  if ("Swing?" %in% names(df)) {
    df <- df %>%
      mutate(`Swing?` = trimws(tolower(`Swing?`)))  # clean up values
    
    # ✅ Apply user-selected swing filter
    if (input$swing_filter == "swing") {
      df <- df %>% filter(`Swing?` == "yes")
    } else if (input$swing_filter == "take") {
      df <- df %>% filter(`Swing?` == "no")
    }
  }
  
  # ✅ Debugging output to confirm rows remaining
  print(paste("Rows after swing filter:", nrow(df)))
  print(table(df$`Swing?`, useNA = "ifany"))
  
  
  # ✅ Now drop NA coordinates to ensure valid plot
  df <- df %>% filter(!is.na(`Strike Zone Side`), !is.na(`Strike Zone Height`))
  
  if (nrow(df) == 0) {
    print("⚠ No valid pitches after filtering.")
    return(NULL)
  }
  
  # Continue your ggplot logic here...
  
  
  plot <- ggplot(df, aes(x = `Strike Zone Side`, y = `Strike Zone Height`)) +
    coord_fixed(ratio = 1) +
    theme_minimal() +
    labs(title = "Strike Zone Heatmap (catcher view)", x = "Horizontal Location", y = "Vertical Location") +
    annotate(
      "rect",
      xmin = -10,
      xmax = 10,
      ymin = 18,
      ymax = 35,
      color = "black",
      fill = NA,
      linewidth = 1
    )
  
  if (input$plot_type == "point") {
    plot <- plot + geom_point(
      aes(color = ExitVelocity),
      alpha = 0.5,
      size = 2,
      na.rm = TRUE
    ) +
      scale_color_gradient(low = "blue", high = "red")
  } else {
    plot <- plot + stat_density2d(
      geom = "raster",
      aes(fill = after_stat(density)),
      contour = FALSE,
      na.rm = TRUE
    ) +
      scale_fill_gradientn(colours = c("#f7f7f7", "#fee391", "#fec44f", "#fe9929", "#993404"))
  }
  
  ggplotly(plot)
})

output$performance_histogram <- renderPlotly({
  df_hist <- filtered_histogram_data()  # ✅ Use the filtered data
  print(head(df_hist))  # ✅ Debugging: Print first few rows to confirm filtering
  
  # ✅ Ensure the selected metric exists before plotting
  if (!(input$hist_metric %in% colnames(df_hist))) {
    print(paste("⚠ Metric", input$hist_metric, "not found in dataset!"))
    return(NULL)
  }
  
  # ✅ Adjust bin width dynamically for "Squared Up %" and "Time to Contact"
  bin_width <- if (input$hist_metric == "Time to Contact (sec)")
    0.005
  else if (input$hist_metric == "Squared Up %")
    0.05
  else
    5
  
  plot <- ggplot(df_hist, aes(x = .data[[input$hist_metric]])) +
    geom_histogram(
      binwidth = bin_width,
      fill = "steelblue",
      color = "black",
      alpha = 0.7,
      na.rm = TRUE
    ) +
    theme_minimal() +
    labs(
      title = paste(input$hist_metric, "Distribution"),
      x = input$hist_metric,
      y = "Count"
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  
  ggplotly(plot)
})


output$performance_histogram <- renderPlotly({
  df_hist <- filtered_histogram_data()  # ✅ Use the filtered data
  
  print("🔹 First few rows of filtered data:")
  print(head(df_hist))  # ✅ Debugging: Print first rows to verify filtering
  
  # ✅ Ensure the selected metric exists before plotting
  if (!(input$hist_metric %in% colnames(df_hist))) {
    print(paste("⚠ Metric", input$hist_metric, "not found in dataset!"))
    return(NULL)
  }
  
  # ✅ Adjust bin width dynamically
  bin_width <- if (input$hist_metric == "Time to Contact (sec)")
    0.005
  else if (input$hist_metric == "Squared Up %")
    0.05
  else
    5
  
  plot <- ggplot(df_hist, aes(x = .data[[input$hist_metric]])) +
    geom_histogram(
      binwidth = bin_width,
      fill = "steelblue",
      color = "black",
      alpha = 0.7,
      na.rm = TRUE
    ) +
    theme_minimal() +
    labs(
      title = paste(input$hist_metric, "Distribution"),
      x = input$hist_metric,
      y = "Count"
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
  
  ggplotly(plot)
})

observeEvent(input$file_upload, {
  req(input$file_upload)  # Ensure a file is uploaded
  
  df_raw <- read_csv(
    input$file_upload$datapath,
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
  )
  
  print("Column names in raw uploaded CSV:")
  print(colnames(df_raw))  # ✅ Print all column names to console
})

shinyApp(ui, server)
