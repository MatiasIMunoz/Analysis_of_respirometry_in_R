library(shiny)
library(bslib)
library(ggplot2)
library(DT)


source("/Users/matiasvumac/Documents/GitHub/Analysis_of_respirometry_in_R/Respirometry_functions.R")

# Define a ggplot theme
my_theme <-theme_bw()+ 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(face="bold"), 
        legend.position="none",
        strip.background = element_rect(colour = "black", fill = "white"), 
        strip.text.x = element_text(colour = "black", face = "bold"),
        plot.tag = element_text(face = "bold"))

#********************************
#*
# UI ----
#*
#********************************

theme <- bs_theme(bootswatch = "yeti",base_font = font_google("Open Sans"),heading_font = font_google("Montserrat"),primary = "#0056b3")

ui <- page_navbar(
  theme = theme,
  
  title = "Stop-flow respirometry analysis",
  
  
  #********************************
  ### Tab 1. Load data ----
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
                actionButton("apply_params", "Apply parameters", icon = icon("check")), # add action button.
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
              plotOutput("firstPlot", width = "100%"),
              plotOutput("zoomPlot", width = "100%")
              )
              
            )
  ),
  

  #********************************
  ### Tab 2. Lag correction ----
  #********************************
  
  nav_panel("2) Lag correction",
            layout_sidebar(
              sidebar = sidebar(
                tags$p(tags$em(textOutput("param_summary"))),
                actionButton("LagCorr", label = "Correct lag"),
                textOutput("xcorr_window_text"),
              ),
              plotOutput("plotLag", width = "100%"),
            )
  )
  

)




#********************************
#*
# Server ----
#*
#********************************


server <- function(input, output, session) {

  #********************************
  # Tab 1. Load data ----
  #********************************
  
  ### Load CSV data ----
  datafile <- reactive({
    req(input$file1)
    df <- read.csv(input$file1$datapath, sep = ",", quote = '"')
    
    # Change marker at t = 0 to 50 (assuming column 8 exists)
    if ("Marker" %in% names(df)) {
      df$Marker[1] <- 50
    }
    
    df
  })
  
  ### Update Y variable selector and slider range after file is loaded----
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
  
  ### Frog number and repetitions parameters ----
  params <- eventReactive(input$apply_params, {
    list(
      N.frogs = input$N.frogs,
      N.reps = input$N.reps
    )
  })
  
  # Display applied parameters as confirmation text
  output$param_status <- renderText({
    req(params())  # only show after the button is pressed
    paste0(
      "Parameters applied: ",
      params()$N.frogs, " frogs, ",
      params()$N.reps, " repetitions."
    )
  })
  
  ## Example: using them later
  #observeEvent(params(), {
  #  cat("Parameters applied:\n")
  #  print(params())
  #})
  
  
 ### Preview head of data frame ----
  output$contents <- renderTable({
    df <- datafile()
    head(df)
  })

  
  ### First plot ----
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
  # Tab 2. Lag correction ----
  #********************************
  
  # Show number of frogs and reps in
  output$param_summary <- renderText({
    req(params())
    paste("There are:", params()$N.frogs, "frogs and", params()$N.reps, "repetitions in this file.")
  })
  
  # Calculate the window for cross-correlation analysis
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
    b_idx <- which(names(marker_times) == "B")
    
    # Select 2nd and 3rd "B" markers as window
    window <- marker_times[c(b_idx[3], b_idx[4])]
    window
  })
  
  # Show calculated window values
  output$xcorr_window_text <- renderText({
    req(xcorr_window())
    paste("Cross-correlation window (Seconds):", 
          paste(round(xcorr_window(), 2), collapse = " - "))
  })
  

  # Plot of cross-correlation window
  output$plotLag <- renderPlot({
    req(datafile())
    
    df <- datafile()
    
    pO2 <- ggplot(df, aes(x = Seconds, y = O2)) +
      annotate("rect", xmin =  xcorr_window()[[1]], xmax = xcorr_window()[[2]], ymin = -Inf, ymax = Inf,alpha = 0.05, fill = "red")+
      geom_vline(xintercept =  df$Seconds[df$Marker != -1], color = "darkred", linetype = "solid", alpha = 0.4)+
      geom_line(color = "black") +
      theme_minimal(base_size = 14) +
      labs(title =  "O2 vs Time - Window section used for cross-correlation", x = "Seconds", y = "O2")+
      my_theme+
      lims(x = xcorr_window())
      
    
    pCO2 <- ggplot(df, aes(x = Seconds, y = CO2)) +
      annotate("rect", xmin =  xcorr_window()[[1]], xmax = xcorr_window()[[2]], ymin = -Inf, ymax = Inf,alpha = 0.05, fill = "red")+
      geom_vline(xintercept =  df$Seconds[df$Marker != -1], color = "darkred", linetype = "solid", alpha = 0.4)+
      geom_line(color = "black") +
      theme_minimal(base_size = 14) +
      labs(title =  "CO2 vs Time - Window section used for cross-correlation", x = "Seconds", y = "CO2")+
      my_theme+
      lims(x = xcorr_window())
    
    pWVP <- ggplot(df, aes(x = Seconds, y = WVP)) +
      annotate("rect", xmin =  xcorr_window()[[1]], xmax = xcorr_window()[[2]], ymin = -Inf, ymax = Inf,alpha = 0.05, fill = "red")+
      geom_vline(xintercept =  df$Seconds[df$Marker != -1], color = "darkred", linetype = "solid", alpha = 0.4)+
      geom_line(color = "black") +
      theme_minimal(base_size = 14) +
      labs(title =  "WVP vs Time - Window section used for cross-correlation", x = "Seconds", y = "WVP")+
      my_theme+
      lims(x = xcorr_window())
    
    pFlowRate <- ggplot(df, aes(x = Seconds, y = FlowRate)) +
      annotate("rect", xmin =  xcorr_window()[[1]], xmax = xcorr_window()[[2]], ymin = -Inf, ymax = Inf,alpha = 0.05, fill = "blue")+
      geom_vline(xintercept =  df$Seconds[df$Marker != -1], color = "darkblue", linetype = "solid", alpha = 0.4)+
      geom_line(color = "black") +
      theme_minimal(base_size = 14) +
      labs(title =  "FlowRate vs Time - REFERENCE - Window section used for cross-correlation", x = "Seconds", y = "FlowRate")+
      my_theme+
      lims(x = xcorr_window())
    
    gridExtra::grid.arrange(pO2, pCO2, pWVP, pFlowRate, ncol = 1)
  },  height = 750)
  
  # Run cross-correlation analysis
  xcorr_analysis <- eventReactive(input$LagCorr,{
    req(datafile())
    
    df <- datafile()
    
   lags_ch <- lag_correct_channels(df) 
  })
  
  
}

shinyApp(ui, server)
