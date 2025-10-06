library(shiny)
library(bslib)
library(ggplot2)

ui <- page_navbar(
  title = "Uploading Files",
  
  # Tab 1: Load data
  nav_panel("Load data",
            layout_sidebar(
              sidebar = sidebar(
                fileInput("file1", "Choose CSV File",
                          multiple = FALSE,
                          accept = c("text/csv", ".csv")),
                tags$hr(),
                radioButtons("disp", "Display",
                             choices = c(Head = "head", "All (not recommended...)" = "all"),
                             selected = "head")
              ),
              tableOutput("contents")
            )
  ),
  
  # Tab 2: Plot data
  nav_panel("Plot raw data",
            layout_sidebar(
              sidebar = sidebar(
                selectInput("yvar", "Variable:", choices = NULL),
                checkboxInput("show_markers", "Show markers", value = FALSE),
                sliderInput("x_zoom", "Time zoom:", 
                            min = 0, max = 10, value = c(0, 5))  # initial values; will update
              ),
              plotOutput("plotMain", width = "100%"),
              br(),
              plotOutput("plotZoom", width = "100%")
            )
  ),
  
  # Tab 3: Drift and lag correction
  nav_panel("Drift and lag correction",
            layout_sidebar(
              sidebar = sidebar(
                h4("Coming soon")
              ),
              h4("Coming soon...")
            )
  ),
  
  # Tab 4: Respirometry analysis
  nav_panel("Analysis",
            layout_sidebar(
              sidebar = sidebar(
                numericInput("N.frogs", "Number of frogs:", value = 1, min = 1, max = 7, step = 1),
                numericInput("N.reps", "Number of repetitions:", value = 1, min = 1, max = 20, step = 1),
                numericInput("Flush.time", "Time chambers are open (s):", value = 600, min = 1, max = 2400, step = 1)
              ),
              plotOutput("plotAnalysis1", width = "100%", height = "300px")
            )
  ),
  
)

server <- function(input, output, session) {
  
  # Reactive CSV data
  datafile <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, sep = ",", quote = '"')
  })
  
  # Update Y variable selector and slider range after file is loaded
  observeEvent(datafile(), {
    df <- datafile()
    cols <- setdiff(names(df), "Seconds")  # exclude X-axis
    updateSelectInput(session, "yvar", choices = cols, selected = cols[1])
    
    # Update zoom slider range
    updateSliderInput(session, "x_zoom",
                      min = min(df$Seconds),
                      max = max(df$Seconds),
                      value = c(min(df$Seconds), 
                                max(df$Seconds)/2))
  })
  
  # Data preview
  output$contents <- renderTable({
    df <- datafile()
    if (input$disp == "head") head(df) else df
  })
  
  # Main dynamic plot
  output$plotMain <- renderPlot({
    df <- datafile()
    req(input$yvar)
    
    p <- ggplot(df, aes(x = Seconds, y = .data[[input$yvar]])) +
      geom_line(color = "black") +
      theme_minimal(base_size = 14) +
      labs(title = paste(input$yvar, "vs Time"), x = "Seconds", y = input$yvar)
    
    
    if (input$show_markers && "Marker" %in% names(df)) {
      marker_times <- df$Seconds[df$Marker != -1]
      if (length(marker_times) > 0) {
        p <- p + geom_vline(xintercept = marker_times, color = "darkred", linetype = "solid", alpha = 0.4)
      }
    }
    
    p
  })
  
  # Zoomed-in plot
  output$plotZoom <- renderPlot({
    df <- datafile()
    req(input$yvar)
    
    p <- ggplot(df, aes(x = Seconds, y = .data[[input$yvar]])) +
      geom_line(color = "black") +
      theme_minimal(base_size = 14) +
      labs(title = paste(input$yvar, "vs Time (Zoomed)"), x = "Seconds", y = input$yvar) +
      coord_cartesian(xlim = input$x_zoom)
    
    
    if (input$show_markers && "Marker" %in% names(df)) {
      marker_times <- df$Seconds[df$Marker != -1]
      if (length(marker_times) > 0) {
        # Only show markers in zoomed region
        marker_times <- marker_times[marker_times >= input$x_zoom[1] & marker_times <= input$x_zoom[2]]
        if (length(marker_times) > 0) {
          p <- p + geom_vline(xintercept = marker_times, color = "darkred", linetype = "solid", alpha = 0.4)
        }
      }
    }
    
    p
  })
  
  
  # Plot channels to analyze (Analysis tab)
  output$plotAnalysis1 <- renderPlot({
    df <- datafile()
    req(input$yvar)
    
    p <- ggplot(df, aes(x = Seconds, y = .data[[input$yvar]])) +
      geom_line(color = "black") +
      theme_minimal(base_size = 14) +
      labs(title = paste(input$yvar, "vs Time"), x = "Seconds", y = input$yvar)
    
    
    if (input$show_markers && "Marker" %in% names(df)) {
      marker_times <- df$Seconds[df$Marker != -1]
      if (length(marker_times) > 0) {
        p <- p + geom_vline(xintercept = marker_times, color = "darkred", linetype = "solid", alpha = 0.4)
      }
    }
    
    p
  })
  
  
}

shinyApp(ui, server)
