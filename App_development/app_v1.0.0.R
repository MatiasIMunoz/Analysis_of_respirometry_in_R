
# Shiny app for the analysis of stop-flow respirometry recordings.
# By Matías I. Muñoz

# Load packages and install them if missing
packages <- c("shiny", "bslib", "ggplot2", "DT", "cowplot", "dplyr", "tidyr", "viridis", "pracma", "plotly", "shinyjs")

if (!requireNamespace(packages, quietly = TRUE)) {
  install.packages(packages, dependencies = TRUE)
}

library(shiny)
library(bslib)
library(ggplot2)
library(DT)
library(cowplot)
library(dplyr)
library(tidyr)
library(viridis)
library(pracma)
library(plotly)
library(shinyjs)

# Load script with custom functions. NECESSARY!
source("/Users/matiasvumac/Documents/GitHub/Analysis_of_respirometry_in_R/Respirometry_functions.R")

# Define a ggplot theme
my_theme <-theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face="bold"), 
        strip.background = element_rect(colour = "black", fill = "white"), 
        strip.text.x = element_text(colour = "black", face = "bold"),
        plot.tag = element_text(face = "bold"))

#************************************************************
#
#                     A) User Interface (UI) ----
#
#************************************************************

theme <- bs_theme(bootswatch = "yeti",base_font = font_google("Open Sans"),heading_font = font_google("Montserrat"),primary = "#0056b3")

ui <- page_navbar(

    theme = theme,
  
  #title = "Stop-flow respirometry analysis/nversion: 1.0.0 (18Dec2025)",
  title = tags$div(
    "Stop-flow respirometry analysis",
    tags$div(
      "version: 1.0.0 (18 December 2025)",
      style = "font-size: 0.6em; opacity: 0.8;"
    )
  ),
  
  # This just makes the handle knowbs of sliderInput() smaller, so selecting smaller values is easier...
  tags$head(
    tags$style(HTML("
      .irs-handle {
        width: 10px !important; 
        height: 10px !important;
        top:13px !important;
      }
    "))
  ),
  
  #********************************
  # 
  ###  Tab 1. Load data ----
  #
  #********************************
  
  nav_panel("1) Load data",
            useShinyjs(), # necessary for blocking parameters after clicking the action button....
            
            layout_sidebar(
              sidebar = sidebar(
                
                #Load CSV file
                tags$p(tags$b("Load your .csv file:")),
                fileInput("file1", "Choose a file",
                          multiple = FALSE,
                          accept = c("text/csv", ".csv")),
                tags$hr(),
                
                tags$p(tags$b("Visualize your file:")),
                selectInput("yvar", "Choose a channel:", choices = NULL),
                sliderInput("x_zoom", "Zoom in:", min = 0, max = 10, value = c(0, 5)),
                checkboxInput("show_markers", "Show markers", value = FALSE),
                tags$hr(),
                
                tags$p(tags$b("Define some basic parameters:")),
                numericInput("N.frogs", "Number of frogs:", value = 1, min = 1, max = 7, step = 1),
                numericInput("N.reps", "Number of repetitions:", value = 1, min = 1, max = 20, step = 1),

                tags$hr(),
                
                #tags$p(tags$b("Frogs IDs:")),
                #textInput("ID_Ch2", "ID channel 2:", value = "NA"),
                #textInput("ID_Ch3", "ID channel 3:", value = "NA"),
                #textInput("ID_Ch4", "ID channel 4:", value = "NA"),
                #textInput("ID_Ch5", "ID channel 5:", value = "NA"),
                #textInput("ID_Ch6", "ID channel 6:", value = "NA"),
                #textInput("ID_Ch7", "ID channel 7:", value = "NA"),
                #textInput("ID_Ch8", "ID channel 8:", value = "NA"),
                tags$p(tags$b("Frogs IDs:")),
                uiOutput("frog_id_inputs"),
                
                
                tags$hr(),
                
                actionButton("apply_params", "Apply parameters", style = "color: white; background-color: darkblue; border-color: darkblue;"),
                
                
                tags$br(), tags$br(),
                textOutput("param_status") 
              ),
              
              layout_column_wrap(
                
                width = 1, 
                tags$div(
                  style = "background-color:#f8f9fa; border-left:4px solid #0056b3;padding:12px; margin-bottom:10px; border-radius:6px;",
                  
                  
                  tags$h5("Instructions:"),
                  tags$p("1. Upload your CSV file exported from ExpeData."),
                  tags$p("2. Choose which channel to visualize."),
                  tags$p("3. Introduce the number of frogs and repetitions. You have to count them manually from the figures."),
                  tags$p("4. Introduce the IDs of each individual. Click 'Apply parameters'"),

                  tags$hr(),
                ),
                
                tags$br(),
                
                div(
                  style = "display: flex; justify-content: center; align-items: center;",
                  
                  tableOutput("contents")), # this simply centers the table
                tags$br(),
                plotOutput("firstPlot", width = "85%"),
                plotOutput("zoomPlot", width = "85%")
              )
              
            )
  ),
  
  
  #********************************
  #
  ###  Tab 2. Lag correction ----
  #
  #********************************
  
  nav_panel("2) Lag correction",
            layout_sidebar(
              sidebar = sidebar(
                
                tags$p(tags$b("Recording details:")),
                tableOutput("params_table"),
                
                sliderInput("x_zoom2", "Choose a cross-correlation window:", min = 0, max = 10, value = c(0, 5)),
                
                numericInput("lag_max", "Maximum lag for cross-correlation (s):", value = 200, min = 50, max = 400, step = 1),
                actionButton("LagCorr", label = "Correct lag",  style = "color: white; background-color: darkblue; border-color: darkblue;"),
                
                tags$p(tags$b("Cross correlation window (s):")),
                tableOutput("xcorr_window_table"),

                tags$p(tags$b("Calculated lags (s):")),
                tableOutput("lags_table")
              ),
              
              
              layout_column_wrap(   
                width = 1, 
                
                tags$div(
                  style = "background-color:#f8f9fa; border-left:4px solid #0056b3; padding:12px; margin-bottom:5px; border-radius:6px; width:100%; max-height:250px; overflow-y:auto;",
                  
                  tags$h5("Instructions:"),
                  tags$p("1) Check that the number of frogs and repetitions introduced in the previous tab are correct (the 'Recording details' table)."),
                  tags$p("2) Choose a window for the cross-correlation analysis. It's recommended to avoid including the first few minutes where chambers are flushed for a short time...."),
                  tags$p("3) Modify the maximum lag values for the cross-correlation analysis. A max. lag of 200 (s) works generally well, but in some cases it may have to be either increased or decreased to find the appropiate lag. Check the 'ACF vs lag' at the bottom of the page to evaluate your choice."),
                  tags$hr(),
                ),
                
                
                div(
                  style = "overflow-y:auto; max-height:75vh; padding-right:10px; width:100%;",
                  
                  tags$h4("Select a window for cross-correlation analysis:"),
                  plotOutput("plotCrossCorrWin", width = "95%", height = "800px"),
                  
                  tags$hr(),
                  
                  tags$h4("Results of the cross-correlation analysis:"),
                  plotOutput("ccfPlot", width = "95%", height = "250px"),
                  
                  tags$hr(),
                  
                  tags$h4("Lag in each channel:"),
                  plotOutput("plotLag", width = "95%", height = "800px"),
                  plotlyOutput("ploty_fig", width = "95%", height = "800px"), # added 9 December 2025
                  

                  

                  tags$hr(),
                  
                  tags$h4("Lag-corrected data frame:"),
                  #tableOutput("LagCorrTable")
                  DTOutput("LagCorrTable")
                )
                
              )
            )
  ),
  
  
  
  #********************************
  #
  ### Tab 3. Drift correction ----
  #
  #********************************
  
  nav_panel("3) Drift correction",
            layout_sidebar(
              sidebar = sidebar(
                
                sliderInput("PercentBaseline", "Prop. of baseline to average:", min = 0.05, max = 0.9, value = c(0.1), step = 0.05),
                actionButton("DriftCorr", label = "Correct drift",  style = "color: white; background-color: darkblue; border-color: darkblue;")
                
                
              ),
              
              layout_column_wrap(   
                width = 1, 
                
                tags$div(
                  style = "background-color:#f8f9fa; border-left:4px solid #0056b3; padding:12px; margin-bottom:5px; border-radius:5px; width:100%; max-height:250px; overflow-y:auto;",
                  
                  
                  tags$h5("Instructions:"),
                  tags$p("1) Choose a percentage of the baseline window to compute the average O2 and CO2. The average gas concentration and midpoint of the baseline will be used to fit the spline required to correct drift. 10% works generally well!"),
                  tags$p("2) Click the 'Correct drift' button. This will fit a Forsythe, Malcolm and Moler spline, and substract the predicted values for O2 and CO2 from the lag-corrected values."),
                  tags$hr(),
                  
                ),
                
                div(
                  #style = "padding-right:10px; width:100%;",
                  style = "overflow-y:auto; max-height:75vh; padding-right:10px; width:100%;",
                  
                  tags$p(tags$b("Baseline sections:")),
                  tableOutput("baseline_table"),
                  tags$br(),
                  
                  tags$hr(),
                  
                  tags$p(tags$b("Forsythe, Malcolm and Moler spline:")),
                  plotOutput("baseline_plot", width = "95%", height = "400px"),
                  tags$br(),
                  
                  tags$hr(),
                  
                  tags$p(tags$b("Drift corrected O2 and CO2:")),
                  plotOutput("drift_corrected_plot", width = "95%", height = "400px"),
                  tags$br(),
                  
                  tags$hr(),
                  
                  tags$h4("Lag and drift corrected data frame:"),
                  DTOutput("LagDriftCorrTable")
                )
                
                
              )
            )
            
            
  ),
  
  
  #********************************
  ###  Tab 4. Inspect your data ----
  #********************************
  nav_panel("4) Check your data",
            layout_sidebar(
              sidebar = sidebar(
                
                sliderInput("x_zoom_tab4", "Zoom in:", min = 0, max = 10, value = c(0, 5)),
                tags$hr(),
                downloadButton("download_drift", "Download corrected data (.csv)"),
                downloadButton("download_markers", "Download markers data (.csv)")
              ),
              
              
              layout_column_wrap(
                width = 1,
                
                tags$div(
                  style = "background-color:#f8f9fa; border-left:4px solid #0056b3; padding:12px; margin-bottom:0px; border-radius:5px; width:100%;",
                  
                  
                  tags$h5("Instructions:"),
                  tags$p("1) Check that data looks alright (e.g., did lag and drift correction worked? all individuals are correctly selected?"),
                  #tags$hr(),
                  
                ),
                
                div(
                  #style = "margin-top:0px; padding-top:0px;",
                  style = "overflow-y:auto; max-height:75vh; padding-right:10px; width:100%;",
                  #style = "margin-top:0 !important; padding-top:0 !important; overflow-y:auto; max-height:75vh; padding-right:10px; width:100%;",
                  
                  tags$p(tags$b("Check your data before analysing:")),
                  plotOutput("plotPreAnalysis",  width = "95%", height = "850px"),
                )
              )
              
              
            )),
  
  #********************************
  ###  Tab 5. Analysis ----
  #********************************
  nav_panel("5) Analysis",
            layout_sidebar(
              sidebar = sidebar(
                
                sliderInput("Enclosure_time", "Flush time (s):", min = 10, max = 1200, value = 600, step = 1),
                actionButton("Analyse", label = "Analyse!",  style = "color: white; background-color: darkblue; border-color: darkblue;"),
                tags$hr(),
                downloadButton("download_results", "Download results (.csv)")
              ),
              layout_column_wrap(
                width = 1,
                
                tags$div(
                  style = "background-color:#f8f9fa; border-left:4px solid #0056b3; padding:12px; margin-bottom:5px; border-radius:5px; width:100%; max-height:250px; overflow-y:auto;",
                  tags$h5("Instructions:"),
                  tags$p("1) The app will analyze the lag- and drif-corrected gases."),
                  tags$hr(),
                  #tags$p("2) Click the 'Correct drift' button. This will fit a Forsythe, Malcolm and Moler spline, and substract the predicted values for O2 and CO2 from the lag-corrected values."),
                ),
                
                div(
                  tags$p(tags$b("Results:")),
                  #tableOutput("markers_table"),
                  DTOutput("results"),
                  #tableOutput("results")
                  
                  
                  
                )
                
              )
            )
  )
  
  
  
)




#************************************************************
#
#                        B) Server ----
#
#************************************************************


server <- function(input, output, session) {
  
  
  #********************************
  # 
  #      Tab 1. Load data ----
  #
  #********************************
  
  #______________________________________________________________________
  ## 1.1) Load CSV data ----
  #______________________________________________________________________
  datafile <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath, sep = ",", quote = '"')
    
    # Change marker at t = 0 to from -1 to 50, to the first value is a marker
    if ("Marker" %in% names(df)) { 
      df$Marker[1] <- 50
    }
    
    # Attach filename as an attribute
    # attr(df, "filename") <- input$file1$name
    
    df
  })
  
  #______________________________________________________________________
  ## 1.X) Extract filename ----
  #______________________________________________________________________
  #Reactive for filename
  filename <- reactive({
    req(input$file1)
    
    input$file1$name
  })
  
  #______________________________________________________________________
  ## 1.2) Update Y variable selector and slider range after file is loaded ----
  #______________________________________________________________________
  observeEvent(datafile(), {
    df <- datafile()
    cols <- setdiff(names(df), "Seconds")  # exclude x-axis
    updateSelectInput(session, "yvar", choices = cols, selected = cols[1])
    
    # Update zoom slider range
    updateSliderInput(session, "x_zoom",
                      min = min(df$Seconds),
                      max = max(df$Seconds),
                      value = c(min(df$Seconds), 
                                max(df$Seconds)/2))
  })
  
  
  
  #______________________________________________________________________
  ## 1.3) Frog number and repetitions parameters ----
  #______________________________________________________________________
  params <- eventReactive(input$apply_params, {
    list(
      N.frogs = input$N.frogs,
      N.reps = input$N.reps
    )
  })
  
  
  #______________________________________________________________________
  ## 1.X) Frog IDs (updates depending on N.frogs) ----
  #______________________________________________________________________
  
  output$frog_id_inputs <- renderUI({
    req(input$N.frogs)
    
    n <- input$N.frogs
    
    # Channels start at 2
    chs <- 2:(n + 1)
    
    tagList(
      lapply(chs, function(ch) {
        textInput(
          inputId = paste0("ID_Ch", ch),
          label   = paste("ID channel", ch, ":"),
          value   = "782"
        )
      })
    )
  })
  
  #______________________________________________________________________
  ## 1.XX) Block basic parameters after click  ----
  #______________________________________________________________________
  
  observeEvent(input$apply_params, {
    
    n <- input$N.frogs
    chs <- 2:(n + 1)
    
    # Disable each ID input
    lapply(chs, function(ch) {
      shinyjs::disable(paste0("ID_Ch", ch))
    })
    
    # block nfrogs and nreps
    shinyjs::disable("N.frogs")
    shinyjs::disable("N.reps")
    
    
  })
  
  #______________________________________________________________________
  ## 1.XXX) Set channel names IDs ----
  #______________________________________________________________________
  
  frog_id_lookup <- reactive({
    
    req(input$apply_params > 0)
    req(input$N.frogs)
    
    chs <- 2:(input$N.frogs + 1)
    
    ids <- sapply(chs, function(ch) {
      input[[paste0("ID_Ch", ch)]]
    })
    
    # Named vector: names = channel numbers
    setNames(ids, as.character(chs))
  })
  
  
  
  #______________________________________________________________________
  ## 1.4) Display applied parameters as confirmation text ----
  #______________________________________________________________________
  output$param_status <- renderText({
    req(params()) 
    
    paste0(
      "Parameters applied: ",
      params()$N.frogs, " frogs, ",
      params()$N.reps, " repetitions."
    )
  })
  
  
  
  #______________________________________________________________________
  ## 1.5) Preview head of data frame ----
  #______________________________________________________________________
  output$contents <- renderTable({
    req(datafile())
    df <- datafile()
    
    head(df)
  })
  
  
  
  
  #______________________________________________________________________
  ## 1.6) Plots (full and zoomed) ----
  #______________________________________________________________________
  output$firstPlot <- renderPlot({
    df <- datafile()
    req(input$yvar)
    
    p0 <- ggplot(df, aes(x = Seconds, y = .data[[input$yvar]])) +
      geom_line(color = "black") +
      labs(title = paste(input$yvar, "vs Time"), x = "Seconds", y = input$yvar)+
      my_theme
    
    p1 <- ggplot(df, aes(x = Seconds, y = .data[[input$yvar]])) +
      geom_line(color = "black") +
      labs(title = paste(input$yvar, "vs Time (Zoomed)"), x = "Seconds", y = input$yvar) +
      coord_cartesian(xlim = input$x_zoom)+
      my_theme
    
    if (input$show_markers && "Marker" %in% names(df)) {
      marker_times <- df$Seconds[df$Marker != -1]
      if (length(marker_times) > 0) {
        p0 <- p0 + geom_vline(xintercept = marker_times, color = "darkred", linetype = "solid", alpha = 0.4)
        p1 <- p1 + geom_vline(xintercept = marker_times, color = "darkred", linetype = "solid", alpha = 0.4)
      }
    }
    
    gridExtra::grid.arrange(p0, p1, ncol = 1)
    
  }, height = 400)
  
  
  
  
  
  #********************************
  # 
  #      Tab 2. Lag correction ----
  #
  #********************************
  
  #______________________________________________________________________
  ## 2.1) Small table for n frogs, n repetitions(params()) ----
  #______________________________________________________________________
  output$params_table <- renderTable({
    req(params())
    
    data.frame(
      "Frogs" = params()$N.frogs,
      "Repetitions" = params()$N.reps
    )
  }, rownames = FALSE) 
  
  
  #______________________________________________________________________
  ## X.X) Update Y variable selector and slider range after file is loaded ----
  #______________________________________________________________________
  observeEvent(datafile(), {
    df <- datafile()
    #cols <- setdiff(names(df), "Seconds")  # exclude x-axis
    
    # Update zoom slider range
    updateSliderInput(session, "x_zoom2",
                      min = min(df$Seconds),
                      max = max(df$Seconds),
                      value = c(min(df$Seconds), 
                                max(df$Seconds)/2))
  })
  
  
  #______________________________________________________________________
  ## X.X) Plot cross correlation window ----
  #______________________________________________________________________
  
  output$plotCrossCorrWin <- renderPlot({
    
    req(datafile())
    req(input$x_zoom2)
    
    marker_times <- datafile()$Seconds[datafile()$Marker != -1]
    
    p0 <-
      ggplot(datafile(), aes(x = Seconds, y = O2)) +
      annotate("rect", xmin = input$x_zoom2[1], xmax = input$x_zoom2[2], ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.05)+
      geom_line(color = "black") +
      coord_cartesian(xlim = input$x_zoom2)+
      geom_vline(xintercept = marker_times, color = "grey30", linetype = "solid", alpha = 0.4)+
      my_theme
    
    p1 <- 
      ggplot(datafile(), aes(x = Seconds, y = CO2)) +
      annotate("rect", xmin = input$x_zoom2[1], xmax = input$x_zoom2[2], ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.05)+
      geom_line(color = "black") +
      coord_cartesian(xlim = input$x_zoom2)+
      geom_vline(xintercept = marker_times, color = "grey30", linetype = "solid", alpha = 0.4)+
      my_theme
    
    p2 <- 
      ggplot(datafile(), aes(x = Seconds, y = FlowRate)) +
      annotate("rect", xmin = input$x_zoom2[1], xmax = input$x_zoom2[2], ymin = -Inf, ymax = Inf, fill = "blue", alpha = 0.05)+
      geom_line(color = "black") +
      coord_cartesian(xlim = input$x_zoom2)+
      geom_vline(xintercept = marker_times, color = "grey30", linetype = "solid", alpha = 0.4)+
      my_theme
    
    gridExtra::grid.arrange(p0, p1, p2, ncol = 1)
    
    
  }, height = 600)
  
  
  #______________________________________________________________________
  ## REMOVED 2.2) Calculate the window for cross-correlation analysis ----
  #______________________________________________________________________
#  xcorr_window <- eventReactive(input$LagCorr, {
 #   req(datafile())
  #  req(params())
    
   # df <- datafile()
#    nfrogs <- params()$N.frogs
 #   nreps <- params()$N.reps
    
  #  marker_times <- datafile()$Seconds[df$Marker != -1] # extract markers
    
    # Create names for markers (e.g., B, 2, 3, ..., B, B)
   # marker_names <- c(rep(c("B", as.character(2:(nfrogs+1))), nreps + 1), "B", "B")
    #names(marker_times) <- marker_names
    
    # Find indices of all "B" markers
#    b_id <- which(names(marker_times) == "B")
    
    # Select 2nd and 3rd "B" markers as window
 #   window <- marker_times[c(b_id[3], b_id[4])]
  #  window
  #})
  
  
  
  
  #______________________________________________________________________
  ## 2.3) Small table for cross-correlation window ----
  #______________________________________________________________________
  output$xcorr_window_table <- renderTable({
    req(input$x_zoom2)
    
    data.frame(
      Start =  input$x_zoom2[[1]],
      End =  input$x_zoom2[[2]]
    )
  }, rownames = FALSE) 
  
  
  #______________________________________________________________________
  ## 2.4) Run cross-correlation analysis ----
  #______________________________________________________________________
  xcorr_analysis <- eventReactive(input$LagCorr,{
    req(datafile())
    #req(xcorr_window())
    req(input$x_zoom2)
    req(input$lag_max)
    
    df <- datafile()
    
    lags_ch <- lag_correct_channels(df,
                                    window = input$x_zoom2,
                                    # window = xcorr_window(),
                                    lag.max = input$lag_max) 
    
    lags_ch
  })
  
  
  
  
  
  #______________________________________________________________________
  ## 2.8) Create lag-corrected data frame for O2, CO2, WVP ----
  #_____________________________________________________________________-
#  df_lag <- reactive({
 #   req(datafile())
  #  req(xcorr_analysis())
    
   # df_lag <- datafile()
    #lags <- xcorr_analysis()$lags
    
    #O2_lag <- abs(lags[["O2"]])
    #CO2_lag <- abs(lags[["CO2"]]) 
    #WVP_lag <- abs(lags[["WVP"]])
    
    # Fixed: using correct channel data for each correction
#    df_lag$O2_lagcorrected <- c(df_lag$O2[-(1:O2_lag)], rep(NA, O2_lag))
 #   df_lag$CO2_lagcorrected <- c(df_lag$CO2[-(1:CO2_lag)], rep(NA, CO2_lag))
  #  df_lag$WVP_lagcorrected <- c(df_lag$WVP[-(1:WVP_lag)], rep(NA, WVP_lag))
    
   # return(df_lag)
  #})
  
  #Solution:
  df_lag <- reactive({
    req(datafile())
    req(xcorr_analysis())
    
    df_lag <- datafile()
    lags <- xcorr_analysis()$lags
    
    O2_lag  <- abs(lags[["O2"]])
    CO2_lag <- abs(lags[["CO2"]])
    WVP_lag <- abs(lags[["WVP"]])
    
    shift <- function(x, n) {
      if (n == 0) return(x)              # no shift REMOVED <=
      c(x[(n+1):length(x)], rep(NA, n))  # shift forward
    }
    
    df_lag$O2_lagcorrected  <- shift(df_lag$O2,  O2_lag)
    df_lag$CO2_lagcorrected <- shift(df_lag$CO2, CO2_lag)
    df_lag$WVP_lagcorrected <- shift(df_lag$WVP, WVP_lag)
    
    df_lag
  })
  
  #______________________________________________________________________
  ## 2.5) Plot lag correction ----
  #______________________________________________________________________
   output$plotLag <- renderPlot({
    
     req(datafile())
     req(df_lag())
     req(xcorr_analysis())
     req(input$x_zoom2) 
    
     df <- datafile()
     df_lag <- df_lag()
    
     pO2 <- ggplot(df_lag, aes(x = Seconds)) +
       geom_vline(xintercept =  df$Seconds[df$Marker != -1], color = "darkred", linetype = "solid", alpha = 0.4)+
       geom_line(aes(y = O2, color = "original"), linewidth = 1) +
       geom_line(data = df_lag, aes(y = O2_lagcorrected, color = "corrected"), inherit.aes = TRUE, linewidth = 1) +
       scale_color_manual(values = c("original" = "grey70", "corrected" = "black")) +
       theme_minimal(base_size = 14) +
       labs(title =  "O2 vs Time", x = "Seconds", y = "O2", color = "")+
       my_theme+
       lims(x =  input$x_zoom2)
    
     pCO2 <- ggplot(df_lag, aes(x = Seconds)) +
       geom_vline(xintercept =  df$Seconds[df$Marker != -1], color = "darkred", linetype = "solid", alpha = 0.4)+
       geom_line(aes(y = CO2, color = "original"), linewidth = 1) +
       geom_line(data = df_lag, aes(y = CO2_lagcorrected, color = "corrected"), inherit.aes = TRUE, linewidth = 1) +
       scale_color_manual(values = c("original" = "grey70", "corrected" = "black")) +
       theme_minimal(base_size = 14) +
       labs(title =  "CO2 vs Time", x = "Seconds", y = "CO2", color = "")+
       my_theme+
       theme(legend.position = "right")+
       guides(linetype = guide_legend(override.aes = list(size = 2)))+
       lims(x = input$x_zoom2)
    
     pWVP <- ggplot(df_lag, aes(x = Seconds)) +
       geom_vline(xintercept =  df$Seconds[df$Marker != -1], color = "darkred", linetype = "solid", alpha = 0.4)+
       geom_line(aes(y = WVP, color = "original"), linewidth = 1) +
       geom_line(data = df_lag, aes(y = WVP_lagcorrected, color = "corrected"), inherit.aes = TRUE, linewidth = 1) +
       scale_color_manual(values = c("original" = "grey70", "corrected" = "black")) +
       theme_minimal(base_size = 14) +
       labs(title =  "WVP vs Time", x = "Seconds", y = "WVP", color = "")+
       my_theme+
       lims(x = input$x_zoom2)
    
     pFlowRate <- ggplot(df_lag, aes(x = Seconds)) +
       geom_vline(xintercept =  df$Seconds[df$Marker != -1], color = "darkblue", linetype = "solid", alpha = 0.4)+
       geom_line(aes(y = FlowRate, color = "original"), linewidth = 1) +
       scale_color_manual(values = c("original" = "royalblue3")) +
       theme_minimal(base_size = 14) +
       labs(title =  "FlowRate vs Time - REFERENCE", x = "Seconds", y = "FlowRate", color = "")+
       my_theme+
       lims(x = input$x_zoom2)
    
     cowplot::plot_grid(pO2, pCO2, pWVP, pFlowRate, ncol = 1, align = "hv")
    
   },  height = 799)
  
  
  #______________________________________________________________________
  ## 2.X) PlotLY lag correction ----
  #______________________________________________________________________
  
  output$ploty_fig <- renderPlotly({
    req(datafile())
    req(df_lag())
    req(xcorr_analysis())
    req(input$x_zoom2) 
    
    df <- datafile()
    df_lag <- df_lag()
    
    plotly_O2<- 
  ggplotly(ggplot(df_lag, aes(x = Seconds)) +
      geom_vline(xintercept =  df$Seconds[df$Marker != -1], color = "darkred", linetype = "solid", alpha = 0.4)+
      geom_line(aes(y = O2, color = "original"), linewidth = 1) +
      geom_line(data = df_lag, aes(y = O2_lagcorrected, color = "corrected"), inherit.aes = TRUE, linewidth = 1) +
      scale_color_manual(values = c("original" = "grey70", "corrected" = "black")) +
      theme_minimal(base_size = 14) +
      labs(title =  "", x = "Seconds", y = "O2", color = "")+
      my_theme,
      height = 500
      )
    
    plotly_CO2<- 
      ggplotly(ggplot(df_lag, aes(x = Seconds)) +
                 geom_vline(xintercept =  df$Seconds[df$Marker != -1], color = "darkred", linetype = "solid", alpha = 0.4)+
                 geom_line(aes(y = CO2, color = "original"), linewidth = 1) +
                 geom_line(data = df_lag, aes(y = CO2_lagcorrected, color = "corrected"), inherit.aes = TRUE, linewidth = 1) +
                 scale_color_manual(values = c("original" = "grey70", "corrected" = "black")) +
                 theme_minimal(base_size = 14) +
                 labs(title =  "", x = "Seconds", y = "CO2", color = "")+
                 my_theme,
               height = 500
      )
    
    plotly_WVP <-
      ggplotly(ggplot(df_lag, aes(x = Seconds)) +
      geom_vline(xintercept =  df$Seconds[df$Marker != -1], color = "darkred", linetype = "solid", alpha = 0.4)+
      geom_line(aes(y = WVP, color = "original"), linewidth = 1) +
      geom_line(data = df_lag, aes(y = WVP_lagcorrected, color = "corrected"), inherit.aes = TRUE, linewidth = 1) +
      scale_color_manual(values = c("original" = "grey70", "corrected" = "black")) +
      theme_minimal(base_size = 14) +
      labs(title =  "", x = "Seconds", y = "WVP", color = "")+
      my_theme,
      height = 500
      )
    
    plotly_O2 %>% layout(xaxis = list(rangeslider = list()))
    plotly_CO2 %>% layout(xaxis = list(rangeslider = list()))
    plotly_WVP %>% layout(xaxis = list(rangeslider = list()))
    
    
    subplot(style(plotly_O2, showlegend = TRUE),
            style(plotly_CO2, showlegend = FALSE),
            style(plotly_WVP, showlegend = FALSE),
            nrows = 3,
            shareX = TRUE, shareY = TRUE)
  })

  
  #______________________________________________________________________
  ## 2.6) Table for lags ----
  #______________________________________________________________________
 output$lags_table <- renderTable({
    req(xcorr_analysis())
    
    # Create a small data frame
    data.frame(
      Channel = c("O2", "CO2", "WVP"),
     Lag = unlist(xcorr_analysis()$lags)
    )
  }, rownames = FALSE, digits = 0) 
  
  
  
  
  
  #______________________________________________________________________
  ## 2.7) Plot cross-correlation results ----
  #______________________________________________________________________
  output$ccfPlot <- renderPlot({
    req(xcorr_analysis())
    
    res <- xcorr_analysis()
    
    par(mfrow = c(1, length(res$ccf))) # figure with 3 panels
    
    for (ch in names(res$ccf)) {
      
      ccf_res <- res$ccf[[ch]]
      #plot(ccf_res, main = "")
      
      df_ccf_plot <- cbind.data.frame(lag = ccf_res$lag, acf = (ccf_res$acf)^2)
      df_ccf_plot <-  subset(df_ccf_plot, lag <= 0) # Keep only negative lags
      
      #plot(ccf_res$lag, (ccf_res$acf)^2, type = "l", xlab = "lag (s)", ylab = "Cross-correlation squared")
      plot(acf~lag, data = df_ccf_plot, type = "l")
      
      polygon(
        x = c(df_ccf_plot$lag, rev(df_ccf_plot$lag)),
        y = c(df_ccf_plot$acf, rep(0, length(df_ccf_plot$acf))),
        col = "black",
        border = NA
      )
      
      abline(v = res$lags[ch], col = "red")
      title(paste0(ch, " (lag = ", res$lags[ch], " s)"))
    }
  }, height = 249)
  
  
  
  
  #______________________________________________________________________
  ## 1.X) Min. and Max. seconds lag-corrected ----
  #______________________________________________________________________
  min_max_seconds <- reactive({
    req(df_lag())
    
    marker_times <- df_lag()$Seconds[df_lag()$Marker != -1] # extract markers.
    
    min_seconds <- min(marker_times) # minimum seconds with markers.
    max_seconds <- max(marker_times) # maximum seconds with markers.
    
    #min_max_seconds_df <- cbind.data.frame(Min = min_seconds, Max = max_seconds)
    return(list(min_seconds = min_seconds, max_seconds = max_seconds))
  })
  
  
  #______________________________________________________________________
  ## 2.9) Head of lag-corrected data frame ----
  #______________________________________________________________________
  #output$LagCorrTable <- renderDT({
   # req(df_lag())
    
    #datatable(
     # head(df_lag(), 25),
      #options = list(
       # scrollX = TRUE,        
        #scrollY = "400px",     
#        scrollCollapse = TRUE,
 #       paging = FALSE,        
  #      searching = FALSE,     
   #     info = FALSE         
    #  ),
     # rownames = FALSE
#    )
  #})
  
  
  
  
  
  #********************************
  #
  # Tab 3. Drift correction ----
  #
  #********************************
  
  #______________________________________________________________________
  ## 3.1) Percent of baseline window for O2 average ----
  #______________________________________________________________________
  #params <- eventReactive(input$PercentBaseline, {
  # list(
  #  N.frogs = input$N.frogs,
  # N.reps = input$N.reps
  #)
  #})
  
  #______________________________________________________________________
  ## 3.2) Calculate table for drift correction (baseline midpoints and mean O2 and CO2) ----
  #______________________________________________________________________
  drift_table <- eventReactive(input$DriftCorr, {
    req(datafile())
    req(df_lag())
    req(params())
    req(input$PercentBaseline)
    
    
    df <- datafile()
    df_lag <- df_lag()
    nfrogs <- params()$N.frogs
    nreps <- params()$N.reps
    baseline_prop <- input$PercentBaseline
    
    
    marker_times <- df_lag$Seconds[df_lag$Marker != -1]
    marker_names <- c(rep(c("B", as.character(2:(nfrogs+1))), nreps+1), "B", "B")
    names(marker_times) <- marker_names
    marker_times
    
    
    # All B's and 2's
    Bs_2s <- marker_times[names(marker_times) == "B" | names(marker_times) == "2"]
    Bs_2s_df <- cbind.data.frame(Repetition = rep(1:(length(Bs_2s)/2), each = 2) , Marker = rep(c("Begin", "End"), (length(Bs_2s)/2)), Seconds =  Bs_2s)
    Bs_2s_df_wide <- pivot_wider(Bs_2s_df, names_from = Marker, values_from = Seconds)
    Bs_2s_df_wide$Midpoint <- round((Bs_2s_df_wide$End + Bs_2s_df_wide$Begin)/2)
    Bs_2s_df_wide <- as.data.frame(Bs_2s_df_wide)
    
    Bs_2s_df_wide$Lower.prop <- round(Bs_2s_df_wide[,2] + (Bs_2s_df_wide[,3] - Bs_2s_df_wide[,2])*((1-baseline_prop)/2))
    Bs_2s_df_wide$Upper.prop <- round(Bs_2s_df_wide[,3] - (Bs_2s_df_wide[,3] - Bs_2s_df_wide[,2])*((1-baseline_prop)/2))
    
    
    # Compute mean O2 and CO2 for each repetition
    mean_O2_df <- Bs_2s_df_wide %>%
      rowwise() %>%
      mutate(mean_O2 = mean(df_lag$O2[df_lag$Seconds >= Lower.prop & df_lag$Seconds <= Upper.prop], na.rm = TRUE),
             mean_CO2 = mean(df_lag$CO2[df_lag$Seconds >= Lower.prop & df_lag$Seconds <= Upper.prop], na.rm = TRUE)) %>%
      ungroup()
    
    
    # turn table into data frame
    mean_O2_df <- as.data.frame(mean_O2_df)
    
    # (mean_O2_df)
    
  })
  
  
  
  
  #______________________________________________________________________
  ## 3.3) Print drift table ----
  #______________________________________________________________________
  output$baseline_table <- renderTable({
    req(drift_table())
    
    print(drift_table())
    
  }, rownames = FALSE, width = "70%") 
  
  
  
  
  #______________________________________________________________________
  ## 3.4) Fit spline----
  #______________________________________________________________________
  ffm_spline <- eventReactive(input$DriftCorr, {
    req(drift_table())
    req(datafile())
    req(df_lag())
    
    # Fit a Forsythe, Malcolm and Moler (FFM) spline
    O2_spline_function <- splinefun(drift_table()$Midpoint, drift_table()$mean_O2, method = "fmm")
    CO2_spline_function <- splinefun(drift_table()$Midpoint, drift_table()$mean_CO2, method = "fmm")
    
    # Predict baseline for all seconds
    O2_predict <- O2_spline_function(df_lag()$Seconds)
    CO2_predict <- CO2_spline_function(df_lag()$Seconds)
    
    # Create drift data frame
    df_drift <- df_lag()
    
    # Add lag and drift
    df_drift$O2_driftlagcorrected <- df_drift$O2_lagcorrected - O2_predict
    df_drift$CO2_driftlagcorrected <-df_drift$CO2_lagcorrected - CO2_predict
    
    # Return a data frame with spline predictions as list.
    list(
      baseline_df = 
        data.frame(
          Seconds = df_lag()$Seconds,
          Baseline_O2 = O2_predict,
          Baseline_CO2 = CO2_predict
        )
    )
    
  })
  
  
  
  
  #______________________________________________________________________
  ## 3.5) Plot drift spline ----
  #______________________________________________________________________
  output$baseline_plot <- renderPlot({
    req(drift_table())
    req(datafile())
    req(ffm_spline())
    req(df_lag())
    
    df <- datafile()
    df_lag <- df_lag()
    spline_df <- ffm_spline()
    
    marker_times <- df_lag$Seconds[df$Marker != -1] # extract markers
    
    #Plot O2
    p0 <-
      ggplot(df_lag, aes(x = Seconds))+
      geom_rect(data = drift_table(), aes(xmin = Begin, xmax = End, ymax = Inf, ymin = -Inf), inherit.aes = FALSE, fill = "grey70", alpha = 0.2)+
      geom_rect(data = drift_table(), aes(xmin = Lower.prop, xmax = Upper.prop, ymax = Inf, ymin = -Inf), inherit.aes = FALSE, fill = "red", alpha = 0.2)+
      geom_line(aes(y = O2_lagcorrected), col = "black")+
      geom_point(data = drift_table(), aes(x = Midpoint, y = mean_O2), col = "red", size = 2)+
      geom_line(data = spline_df$baseline_df, aes(x = Seconds, y = Baseline_O2), col = "red")+
      labs(title = "Drift uncorrected O2")+
      lims(x = c(min(drift_table()$Begin), max(drift_table()$End)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    p0
    
    #Plot CO2
    p1 <-
      ggplot(df_lag, aes(x = Seconds))+
      geom_rect(data = drift_table(), aes(xmin = Begin, xmax = End, ymax = Inf, ymin = -Inf), inherit.aes = FALSE, fill = "grey70", alpha = 0.2)+
      geom_rect(data = drift_table(), aes(xmin = Lower.prop, xmax = Upper.prop, ymax = Inf, ymin = -Inf), inherit.aes = FALSE, fill = "red", alpha = 0.2)+
      geom_line(aes(y = CO2_lagcorrected), col = "black")+
      geom_point(data = drift_table(), aes(x = Midpoint, y = mean_CO2), col = "red", size = 2)+
      geom_line(data = spline_df$baseline_df, aes(x = Seconds, y = Baseline_CO2), col = "red")+
      labs(title = "Drift uncorrected CO2")+
      lims(x = c(min(drift_table()$Begin), max(drift_table()$End)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    p1
    
    gridExtra::grid.arrange(p0, p1, ncol = 1)
    
  }, height = 399)
  
  
  #______________________________________________________________________
  ## 3.6) Create drift-corrected data frame for O2 and CO2 ----
  #______________________________________________________________________
  dataframe_drift <- reactive({
    req(df_lag())
    req(ffm_spline())
    req(drift_table())
    req(min_max_seconds())
    
    drift_df <- df_lag()
    spline_df <- ffm_spline()
    
    
    # Correct O2.
    drift_df$O2_lagdriftcorrected <- drift_df$O2_lagcorrected - (spline_df$baseline_df$Baseline_O2)
    
    # Correct CO2.
    drift_df$CO2_lagdriftcorrected <- drift_df$CO2_lagcorrected - (spline_df$baseline_df$Baseline_CO2)
    
    # Subset df between markers.
    drift_df <- drift_df[c((min_max_seconds()$min_seconds):(min_max_seconds()$max_seconds)), ]
    
    
    # Span O2 and CO2 to 0 (zero).
    #drift_df$O2_lagdriftcorrected  <- drift_df$O2_lagdriftcorrected  - abs(max(drift_df$O2_lagdriftcorrected, na.rm = TRUE)) # remove maximum to avoid positive values
    #drift_df$CO2_lagdriftcorrected  <- drift_df$CO2_lagdriftcorrected + abs(min(drift_df$CO2_lagdriftcorrected, na.rm = TRUE)) # add minimum to avoid negative values
    
    return(drift_df)
  })
  
  
  
  
  #______________________________________________________________________
  ## 3.7) Head of drift-corrected data frame for O2 and CO2 ----
  #_________________________________________________________ _____________
  
  output$LagDriftCorrTable <- renderDT({
    req(dataframe_drift())
    
    datatable(
      head(dataframe_drift(), 6),
      options = list(
        scrollX = TRUE,        
        scrollY = "400px",     
        scrollCollapse = TRUE,
        paging = FALSE,        
        searching = FALSE,     
        info = FALSE         
      ),
      rownames = FALSE
    )
  })
  
  
  #______________________________________________________________________
  ## 3.8) Plot drift and lag corrected ----
  #______________________________________________________________________
  
  output$drift_corrected_plot <- renderPlot({
    req(drift_table())
    req(dataframe_drift())
    
    p0 <- ggplot(dataframe_drift(), aes(x = Seconds))+
      geom_rect(data = drift_table(), aes(xmin = Begin, xmax = End, ymax = Inf, ymin = -Inf), inherit.aes = FALSE, fill = "grey70", alpha = 0.2)+
      geom_line(aes(y = O2_lagdriftcorrected), col = "black")+
      geom_hline(yintercept = 0, col = "red", linetype = "dashed")+
      #lims(x = c(min(drift_table()$Begin), max(drift_table()$End)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    p0
    
    p1 <- ggplot(dataframe_drift(), aes(x = Seconds))+
      geom_rect(data = drift_table(), aes(xmin = Begin, xmax = End, ymax = Inf, ymin = -Inf), inherit.aes = FALSE, fill = "grey70", alpha = 0.2)+
      geom_line(aes(y = CO2_lagdriftcorrected), col = "black")+
      geom_hline(yintercept = 0, col = "red", linetype = "dashed")+
      #lims(x = c(min(drift_table()$Begin), max(drift_table()$End)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    p1
    
    gridExtra::grid.arrange(p0, p1, ncol = 1)
    
  }, height = 399)
  
  #********************************
  #
  # Tab 4. Check your data ----
  #
  #********************************
  
  #______________________________________________________________________
  ## 4.0) Update plot x-axis zoom ----
  #______________________________________________________________________
  
  #observeEvent(df_drift() {
  
  # Update zoom slider range
  # updateSliderInput(session, "x_zoom_tab4",
  #                  min = min(df_drift()$Seconds),
  #                 max = max(df_drift()$Seconds),
  #                value = c(min(df_drift()$Seconds), 
  #                         max(df_drift()$Seconds)/2))
  #})
  
  
  
  #______________________________________________________________________
  ## 4.1) Markers table ----
  #______________________________________________________________________
  
  markers_df <- reactive({
    req(dataframe_drift())
    req(params())
    req(datafile())
    
    df_analysis <- dataframe_drift()
    nfrogs <- params()$N.frogs
    nreps <- params()$N.reps
    
    marker_times <- datafile()$Seconds[datafile()$Marker != -1];length(marker_times)
    marker_names <- c(rep(c("B", as.character(2:(nfrogs+1))), nreps+1), "B", "B");length(marker_names)
    #marker_names[length(marker_names)] <- "B"  # set last one to "B"
    names(marker_times) <- marker_names
    marker_times
    
    # Extract marker names and times
    Marker      <- names(marker_times)
    Begin_time  <- marker_times
    End_time    <- c(marker_times[-1], NA)        # shift left for end times
    Begin_time  <- Begin_time + c(0, rep(1, length(marker_times)-1))  # add +1 except first
    
    # Build data frame
    markers_df <- data.frame(
      Marker = Marker,
      Begin_time = Begin_time,
      End_time = End_time
    )
    
    # Remove last row (no end time)
    markers_df <- markers_df[-nrow(markers_df), ]
    
    markers_df$Repetition <- c(rep(0:nreps, rep_len(c(nfrogs+1), nreps+1)), NA)
    
    return(markers_df)
    
  })
  
  #______________________________________________________________________
  ## 4.X) Update slider range after file is loaded ----
  #______________________________________________________________________
  observeEvent(datafile(), {
    df <- datafile()
    #cols <- setdiff(names(df), "Seconds")  # exclude x-axis
    
    # Update zoom slider range
    updateSliderInput(session, "x_zoom_tab4",
                      min = min(df$Seconds),
                      max = max(df$Seconds),
                      value = c(min(df$Seconds), 
                                max(df$Seconds)/5))
  })
  
  
  #______________________________________________________________________
  ## 4.XX) Dowload corrected data and markers  ----
  #______________________________________________________________________
  output$download_drift <- downloadHandler(
    filename = function() {
      req(filename())
      
      # Remove .csv extension from original filename
      base <- sub("\\.csv$", "", filename())
      
      paste0(base, "_corrected.csv")
    },
    content = function(file) {
      req(dataframe_drift())
      write.csv(dataframe_drift(), file, row.names = FALSE)
    }
  )
  
  
  output$download_markers <- downloadHandler(
    filename = function() {
      req(filename())
      
      # Remove .csv extension from original filename
      base <- sub("\\.csv$", "", filename())
      
      paste0(base, "_markers.csv")
    },
    content = function(file) {
      req(markers_df())
      write.csv(markers_df(), file, row.names = FALSE)
    }
  )
  
  #______________________________________________________________________
  ## 4.2) Plot data to analyze ----
  #______________________________________________________________________
  
  output$plotPreAnalysis <- renderPlot({
    req(drift_table())
    req(dataframe_drift())
    req(markers_df())
    req(params())
    
    
    # Define colors for different channels
    cols <- viridis(params()$N.frogs, option = "turbo")
    cols <- c("grey70", cols) # add grey baseline.
    cols <- setNames(cols, unique(markers_df()$Marker)) #set names for each color.
    
    
    # Plots
    pO2 <- ggplot(dataframe_drift(), aes(x = Seconds))+
      geom_rect(data = markers_df(), 
                aes(xmin = Begin_time, xmax = End_time, ymin = -Inf, ymax = Inf, fill = Marker), 
                inherit.aes = FALSE,
                alpha = 0.2) +
      scale_fill_manual(values = cols) +
      geom_hline(yintercept = 0, col = "grey70", linetype = "dashed")+
      geom_line(aes(y = O2_lagdriftcorrected), col = "black")+
      #lims(x = c(min(drift_table()$Begin), max(drift_table()$End)))+
      coord_cartesian(xlim = input$x_zoom_tab4)+
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
    
    pCO2 <- ggplot(dataframe_drift(), aes(x = Seconds))+
      geom_rect(data = markers_df(), 
                aes(xmin = Begin_time, xmax = End_time, ymin = -Inf, ymax = Inf, fill = Marker), 
                inherit.aes = FALSE,
                alpha = 0.2) +
      scale_fill_manual(values = cols) +
      geom_hline(yintercept = 0, col = "grey70", linetype = "dashed")+
      geom_line(aes(y = CO2_lagdriftcorrected), col = "black")+
      # lims(x = c(min(drift_table()$Begin), max(drift_table()$End)))+
      coord_cartesian(xlim = input$x_zoom_tab4)+
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
    
    pWVP <- ggplot(dataframe_drift(), aes(x = Seconds))+
      geom_rect(data = markers_df(), 
                aes(xmin = Begin_time, xmax = End_time, ymin = -Inf, ymax = Inf, fill = Marker), 
                inherit.aes = FALSE,
                alpha = 0.2) +
      scale_fill_manual(values = cols) +
      geom_line(aes(y = WVP_lagcorrected), col = "black")+
      #lims(x = c(min(drift_table()$Begin), max(drift_table()$End)))+
      coord_cartesian(xlim = input$x_zoom_tab4)+
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
    pFlowRate <- ggplot(dataframe_drift(), aes(x = Seconds))+
      geom_rect(data = markers_df(), 
                aes(xmin = Begin_time, xmax = End_time, ymin = -Inf, ymax = Inf, fill = Marker), 
                inherit.aes = FALSE,
                alpha = 0.2) +
      scale_fill_manual(values = cols) +
      geom_line(aes(y = FlowRate), col = "black")+
      # lims(x = c(min(drift_table()$Begin), max(drift_table()$End)))+
      coord_cartesian(xlim = input$x_zoom_tab4)+
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
    
    pBP <- ggplot(dataframe_drift(), aes(x = Seconds))+
      geom_rect(data = markers_df(), 
                aes(xmin = Begin_time, xmax = End_time, ymin = -Inf, ymax = Inf, fill = Marker), 
                inherit.aes = FALSE,
                alpha = 0.2) +
      scale_fill_manual(values = cols) +
      geom_line(aes(y = BP), col = "black")+
      #lims(x = c(min(drift_table()$Begin), max(drift_table()$End)))+
      coord_cartesian(xlim = input$x_zoom_tab4)+
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    
    # Combine plots
    cowplot::plot_grid(pO2, pCO2, pWVP, pFlowRate, pBP, ncol = 1, align = "hv")
    
  })
  
  
  
  
  
  
  
  #********************************
  #
  # Tab 5. Analysis ----
  #
  #********************************
  
  #______________________________________________________________________
  ## 5.2) Print markers ----
  #______________________________________________________________________
  #output$markers_table <- renderTable({
  # req(markers_df())
  
  #print(markers_df())
  
  #})
  
  
  #______________________________________________________________________
  ## 5.3) Official analysis ----
  #______________________________________________________________________
  resp_analysis <- eventReactive(input$Analyse, {
    req(dataframe_drift()) # lag- and drift-corrected data frame.
    req(markers_df()) # markers data frame to know where to integrate.
    req(filename()) # file name for the table.
    req(input$Enclosure_time) # require "enclosure time" input
    req(params())
    req(input$x_zoom2)
    req(input$lag_max)
    
    analysis <- stop_flow_analysis(
      
      df = dataframe_drift(),
      markers_df = markers_df(),
      filename = filename(),
      enclosure_time = input$Enclosure_time,
      nfrogs = params()$N.frogs
      
    )
    
    # Divide by VolO2 by enclosure time to obtain VO2.
    analysis$VO2 <- analysis$VolO2_integral/analysis$Enclosure_time_m
    
    # Calculate VCO2
    # VCO2 = FR(FeCO2 - FiCO2) - FeCO2*VO2)/(1 - FeCO2)
    analysis$VCO2 <- ((analysis$VolCO2_integral/analysis$Enclosure_time_m) - analysis$Mean_CO2*analysis$VO2)/(1 - analysis$Mean_CO2)
    
    # Transform to hourly rates (multiply VO2 and VCO2 by 60)
    analysis$VO2_mlhr <- analysis$VO2*60
    analysis$VCO2_mlhr <- analysis$VCO2*60
    
    # Add cross-correlation window boundaries to final data frame.
    analysis$crosscorr_lower_sec <- rep(input$x_zoom2[1], nrow(analysis))
    analysis$crosscorr_upper_sec <- rep(input$x_zoom2[2], nrow(analysis))
    
    # Add maximum lag value to final data frame.
    analysis$max_lag_sec <- rep(input$lag_max, nrow(analysis))
    
    # Add frog ID column
    chs <- 2:(params()$N.frogs + 1)
    
    frog_id_lookup <- setNames(
      sapply(chs, function(ch) {
        input[[paste0("ID_Ch", ch)]]
      }),
      as.character(chs)
    )
    
    analysis$frog_ID <- frog_id_lookup[as.character(analysis$Marker)]
    

    return(analysis)
  })
  
  #______________________________________________________________________
  ## 5.3) Results table ----
  #______________________________________________________________________
  #output$results <- renderTable({
  # req(resp_analysis())
  
  #print(resp_analysis())
  
  #})
  
  
  output$results <- renderDT({
    req(resp_analysis())
    
    datatable(
      resp_analysis(),
      options = list(
        scrollX = TRUE,
        scrollY = "400px",
        scrollCollapse = TRUE,
        paging = FALSE,
        searching = FALSE,
        info = FALSE,
        autoWidth = TRUE,
        fixedColumns = TRUE
      ),
      rownames = FALSE,
      class = "compact hover stripe nowrap"
    ) %>%
      formatRound(
        columns = c("VolO2_integral", "VolCO2_integral", "Mean_CO2", "VO2", "VCO2", "VO2_mlhr", "VCO2_mlhr"),
        digits = c(2,2,5,5,5,5,5)
      )
  })
  
  
  #______________________________________________________________________
  ## 5.XX) Download results  ----
  #______________________________________________________________________
  output$download_results <- downloadHandler(
    filename = function() {
      req(filename())
      
      # Remove .csv extension from original filename
      base <- sub("\\.csv$", "", filename())
      
      paste0(base, "_results.csv")
    },
    
    content = function(file) {
      req(resp_analysis())
      write.csv(resp_analysis(), file, row.names = FALSE)
    }
  )
  
  
}

shinyApp(ui, server)
