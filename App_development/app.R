library(shiny)
library(bslib)
library(ggplot2)
library(DT) # for nice tables
library(cowplot)
library(dplyr)
library(tidyr)

source("/Users/matiasvumac/Documents/GitHub/Analysis_of_respirometry_in_R/Respirometry_functions.R")

# Define a ggplot theme
my_theme <-theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face="bold"), 
        #legend.position="none",
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
  
  title = "Stop-flow respirometry analysis",
  
  
  #********************************
  # 
  ###  Tab 1. Load data ----
  #
  #********************************
  
  nav_panel("1) Load data",
       
            layout_sidebar(
              sidebar = sidebar(
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
                actionButton("apply_params", "Apply parameters", style = "color: white; background-color: darkblue; border-color: darkblue;"),
                tags$br(), tags$br(),
                textOutput("param_status") 
              ),
              
              layout_column_wrap(
                width = 1, 
                tags$div(
                  style = "background-color:#f8f9fa; border-left:4px solid #0056b3;
                 padding:12px; margin-bottom:10px; border-radius:6px;",
                  tags$h5("Instructions:"),
                tags$p("1. Upload your CSV file exported from ExpeData."),
                tags$p("2. Choose which channel to visualize."),
                tags$p("3. Introduce the number of frogs and repetitions, then click 'Apply parameters'.")
                ),
                
              tags$br(),
              div(style = "display: flex; justify-content: center; align-items: center;", tableOutput("contents")), # this simply centers the table
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
                 tags$p("1) Check that the number of frogs and repetitions introduced in the previous tab are correct (the 'Recording details' table). THIS IS IMPORTAT FOR THE REST OF THE ANALYSES."),
                 tags$p("2) Modify the maximum lag values for the cross-correlation analysis. A max. lag of 200 works generally well, but in some cases it may have to be increased. Check the figures at the bottom of the page to evaluate your choice."),
                 tags$hr(),
                 tags$h6("After pressing the 'Correct lag' button, the app will:"),
                 tags$p("a. Compute the window used for cross-correlation. We don't want to cross-correlate across the whole recording, so by default the app chooses the second repetition of the repirometry experiment)."),
                 tags$p("b. Run the cross-correlation analysis. By default the app uses  'FlowRate' as reference channel (also called the 'pacemaker' channel in ExpeData), and record the lag value at which the |cross-correlation coefficient| is maximum."),
                 tags$p("c. Create a figure showing the original and corrected traces for O2, CO2, WVP."),
                 tags$p("d. Create a figure showing the result of the cross-correlation analysis."),
                 
               ),
               

             div(
               style = "overflow-y:auto; max-height:75vh; padding-right:10px; width:100%;",
               
               tags$h4("Lag in each channel:"),
               plotOutput("plotLag", width = "90%", height = "800px"),
               
               tags$hr(),
               
               tags$h4("Results of the cross-correlation analysis:"),
               plotOutput("ccfPlot", width = "90%", height = "250px"),
               
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
                 tags$p("1) Choose a percentage of the baseline window to compute the average O2 and CO2. The average gas concentration and midpoint of the baseline will be used to fit the spline required to correct drift."),
                 tags$p("2) Click the 'Correct drift' button. This will fit a Forsythe, Malcolm and Moler spline, and substract the predicted values for O2 and CO2 from the lag-corrected values."),
               ),
               
               div(
                 tags$p(tags$b("Baseline sections:")),
                 tableOutput("baseline_table"),
                 
                 tags$hr(),
                 
                 tags$p(tags$b("Points for spline fitting and spline:")),
                 plotOutput("baseline_plot", width = "95%", height = "400px"),
                 
                 tags$hr(),
                 
                 tags$p(tags$b("Drift corrected O2 and CO2:")),
                 plotOutput("drift_corrected_plot", width = "95%", height = "250px"),
                 
                 #tags$hr(),
                 
                 #tags$h4("Lag and drift corrected data frame:"),
                 #DTOutput("LagDriftCorrTable")
                 
               )
               
               
               )
           )
           
           
         ),
 
 

 
 #********************************
 #
 ###  Tab 4. Check your data ----
 #
 #********************************
 nav_panel("4) Check your data"),
 

 #********************************
 ###  Tab 5. Analysis ----
 #********************************
 nav_panel("5) Analysis")
 
 

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
    
    # Change marker at t = 0 to 50
    if ("Marker" %in% names(df)) { 
      df$Marker[1] <- 50
    }
    
    df
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
  ## 2.2) Calculate the window for cross-correlation analysis ----
  #______________________________________________________________________
  xcorr_window <- eventReactive(input$LagCorr, {
    req(datafile())
    req(params())
    
    df <- datafile()
    nfrogs <- params()$N.frogs
    nreps <- params()$N.reps
    
    marker_times <- datafile()$Seconds[df$Marker != -1] # extract markers
    
    # Create names for markers (e.g., B, 2, 3, ..., B, B)
    marker_names <- c(rep(c("B", as.character(2:(nfrogs+1))), nreps + 1), "B", "B")
    names(marker_times) <- marker_names
    
    # Find indices of all "B" markers
    b_id <- which(names(marker_times) == "B")
    
    # Select 2nd and 3rd "B" markers as window
    window <- marker_times[c(b_id[3], b_id[4])]
    window
  })
  
  
  
  
  #______________________________________________________________________
  ## 2.3) Small table for cross-correlation window ----
  #______________________________________________________________________
  output$xcorr_window_table <- renderTable({
    req(xcorr_window())
    
    data.frame(
      Start = xcorr_window()[[1]],
      End = xcorr_window()[[2]]
    )
  }, rownames = FALSE) 
  
  
  
  #______________________________________________________________________
  ## 2.4) Plot lag correction ----
  #______________________________________________________________________
  output$plotLag <- renderPlot({
    req(datafile())
    req(df_lag())
    req(xcorr_analysis())
    
    df <- datafile()
    df_lag <- df_lag()
    
    pO2 <- ggplot(df_lag, aes(x = Seconds)) +
      geom_vline(xintercept =  df$Seconds[df$Marker != -1], color = "darkred", linetype = "solid", alpha = 0.4)+
      geom_line(aes(y = O2, color = "original"), linewidth = 1) +
      geom_line(aes(y = O2_lagcorrected, color = "corrected"), linewidth = 1) +
      scale_color_manual(values = c("original" = "grey70", "corrected" = "black")) +
      theme_minimal(base_size = 14) +
      labs(title =  "O2 vs Time", x = "Seconds", y = "O2", color = "")+
      my_theme+
      lims(x = xcorr_window())
    
    pCO2 <- ggplot(df_lag, aes(x = Seconds)) +
      geom_vline(xintercept =  df$Seconds[df$Marker != -1], color = "darkred", linetype = "solid", alpha = 0.4)+
      geom_line(aes(y = CO2, color = "original"), linewidth = 1) +
      geom_line(aes(y = CO2_lagcorrected, color = "corrected"), linewidth = 1) +
      scale_color_manual(values = c("original" = "grey70", "corrected" = "black")) +
      theme_minimal(base_size = 14) +
      labs(title =  "CO2 vs Time", x = "Seconds", y = "CO2", color = "")+
      my_theme+
      theme(legend.position = "right")+
      guides(linetype = guide_legend(override.aes = list(size = 2)))+
      lims(x = xcorr_window())
    
    pWVP <- ggplot(df_lag, aes(x = Seconds)) +
      geom_vline(xintercept =  df$Seconds[df$Marker != -1], color = "darkred", linetype = "solid", alpha = 0.4)+
      geom_line(aes(y = WVP, color = "original"), linewidth = 1) +
      geom_line(aes(y = WVP_lagcorrected, color = "corrected"), linewidth = 1) +
      scale_color_manual(values = c("original" = "grey70", "corrected" = "black")) +
      theme_minimal(base_size = 14) +
      labs(title =  "WVP vs Time", x = "Seconds", y = "WVP", color = "")+
      my_theme+
      lims(x = xcorr_window())
    
    pFlowRate <- ggplot(df_lag, aes(x = Seconds)) +
      geom_vline(xintercept =  df$Seconds[df$Marker != -1], color = "darkblue", linetype = "solid", alpha = 0.4)+
      geom_line(aes(y = FlowRate, color = "original"), linewidth = 1) +
      scale_color_manual(values = c("original" = "royalblue3")) +
      theme_minimal(base_size = 14) +
      labs(title =  "FlowRate vs Time - REFERENCE", x = "Seconds", y = "FlowRate", color = "")+
      my_theme+
      lims(x = xcorr_window())
    
    cowplot::plot_grid(pO2, pCO2, pWVP, pFlowRate, ncol = 1, align = "hv")
    
  },  height = 750)
  
  
 
  
  
  #______________________________________________________________________
  ## 2.5) Run cross-correlation analysis ----
  #______________________________________________________________________
  xcorr_analysis <- eventReactive(input$LagCorr,{
    req(datafile())
    req(xcorr_window())
    req(input$lag_max)

    df <- datafile()
    
   lags_ch <- lag_correct_channels(df,
                                   window = xcorr_window(),
                                   lag.max = input$lag_max) 
   
   lags_ch
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
      plot(ccf_res, main = "")
      abline(v = res$lags[ch], col = "red")
      title(paste0(ch, " (lag = ", res$lags[ch], " s)"))
    }
  })
  
  
  

  
  #______________________________________________________________________
  ## 2.8) Create lag-corrected data frame for O2, CO2, WVP ----
  #_____________________________________________________________________-
  df_lag <- reactive({
    req(datafile())
    req(xcorr_analysis())
    
    df_lag <- datafile()
    lags <- xcorr_analysis()$lags
    
    O2_lag <- abs(lags[["O2"]])
    CO2_lag <- abs(lags[["CO2"]]) 
    WVP_lag <- abs(lags[["WVP"]])
    
    # Fixed: using correct channel data for each correction
    df_lag$O2_lagcorrected <- c(df_lag$O2[-(1:O2_lag)], rep(NA, O2_lag))
    df_lag$CO2_lagcorrected <- c(df_lag$CO2[-(1:CO2_lag)], rep(NA, CO2_lag))
    df_lag$WVP_lagcorrected <- c(df_lag$WVP[-(1:WVP_lag)], rep(NA, WVP_lag))
    
    return(df_lag)
  })
  

  
  
  
  #______________________________________________________________________
  ## 2.9) Head of lag-corrected data frame ----
  #______________________________________________________________________
  output$LagCorrTable <- renderDT({
    req(df_lag())
    
    datatable(
      head(df_lag(), 25),
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
  ## 3.2) Calculate table for drift correction (baseline midpoints and mean O2) ----
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
    
    
    marker_times <- df$Seconds[df$Marker != -1]
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
    
    
    # Compute mean O2 for each repetition
    mean_O2_df <- Bs_2s_df_wide %>%
      rowwise() %>%
      mutate(mean_O2 = mean(df_lag$O2[df_lag$Seconds >= Lower.prop & df_lag$Seconds <= Upper.prop], na.rm = TRUE),
             mean_CO2 = mean(df_lag$CO2[df_lag$Seconds >= Lower.prop & df_lag$Seconds <= Upper.prop], na.rm = TRUE)) %>%
      ungroup()
    

    # turn table into data frame
    mean_O2_df <- as.data.frame(mean_O2_df)
    
    (mean_O2_df)
    
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
    
    # Predict baseline for all times
     O2_predict <- O2_spline_function(df_lag()$Seconds)
    CO2_predict <- CO2_spline_function(df_lag()$Seconds)
    
    # Create drift data frame
    df_drift <- df_lag()
    
    # Add lag and dri
    df_drift$O2_driftlagcorrected <- df_drift$O2_lagcorrected - O2_predict
    df_drift$CO2_driftlagcorrected <- df_drift$CO2_lagcorrected - CO2_predict
    
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
      geom_line(aes(y = O2_lagcorrected), col = "grey50")+
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
      geom_line(aes(y = CO2_lagcorrected), col = "grey50")+
      geom_point(data = drift_table(), aes(x = Midpoint, y = mean_CO2), col = "red", size = 2)+
      geom_line(data = spline_df$baseline_df, aes(x = Seconds, y = Baseline_CO2), col = "red")+
      labs(title = "Drift uncorrected CO2")+
      lims(x = c(min(drift_table()$Begin), max(drift_table()$End)))+
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
    p1
    
    gridExtra::grid.arrange(p0, p1, ncol = 1)
    
  }, height = 400)
  
  
  #______________________________________________________________________
  ## 3.6) Create drift-corrected data frame for O2 and CO2 ----
  #______________________________________________________________________
  df_drift <- reactive({
    req(df_lag())
    req(ffm_spline())

    drift_df <- df_lag()
    spline_df <- ffm_spline()
    
    # Correct O2 and span to 20.95%
    drift_df$O2_lagdriftcorrected <- drift_df$O2_lagcorrected - (spline_df$baseline_df$Baseline_O2 - 20.95)
    # Correct CO2 and span to 0
    drift_df$CO2_lagdriftcorrected <- drift_df$CO2_lagcorrected - (spline_df$baseline_df$Baseline_CO2)
    
    return(drift_df)
  })
  
  
  
  
  #______________________________________________________________________
  ## 3.7) Head of drift-corrected data frame for O2 and CO2 ----
  #______________________________________________________________________
  
#  output$LagDriftCorrTable <- renderDT({
 #   req(df_drift())
    
  #  datatable(
   #   head(df_drift(), 25),
    #  options = list(
     #   scrollX = TRUE,        
      #  scrollY = "400px",     
       # scrollCollapse = TRUE,
      #  paging = FALSE,        
       # searching = FALSE,     
        #info = FALSE         
      #),
      #rownames = FALSE
    #)
  #})
  
  
  #______________________________________________________________________
  ## 3.8) Plot drift and lag corrected ----
  #______________________________________________________________________
  
output$drift_corrected_plot <- renderPlot({
  req(drift_table)
  req(df_drift())
  
  p0 <- ggplot(df_drift(), aes(x = Seconds))+
    geom_rect(data = drift_table(), aes(xmin = Begin, xmax = End, ymax = Inf, ymin = -Inf), inherit.aes = FALSE, fill = "grey70", alpha = 0.2)+
    geom_line(aes(y = O2_lagdriftcorrected), col = "grey50")+
    geom_hline(yintercept = 20.95, col = "red", linetype = "dashed")+
    lims(x = c(min(drift_table()$Begin), max(drift_table()$End)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  p0
  
  p1 <- ggplot(df_drift(), aes(x = Seconds))+
    geom_rect(data = drift_table(), aes(xmin = Begin, xmax = End, ymax = Inf, ymin = -Inf), inherit.aes = FALSE, fill = "grey70", alpha = 0.2)+
    geom_line(aes(y = CO2_lagdriftcorrected), col = "grey50")+
    geom_hline(yintercept = 0, col = "red", linetype = "dashed")+
    lims(x = c(min(drift_table()$Begin), max(drift_table()$End)))+
    theme_bw()+
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank())
  p1
  
  gridExtra::grid.arrange(p0, p1, ncol = 1)
  
}, height = 400)
  
}

shinyApp(ui, server)
