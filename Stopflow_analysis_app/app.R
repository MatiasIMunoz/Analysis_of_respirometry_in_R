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
                             choices = c(Head = "head", "All (not recommended)" = "all"),
                             selected = "head")
              ),
              tableOutput("contents")
            )
  ),
  
  # Tab 2: Plot data
  nav_panel("Plot raw data",
            layout_sidebar(
              sidebar = sidebar(
                selectInput("yvar", "Y-axis variable:", choices = NULL),
                checkboxInput("show_markers", "Show markers", value = FALSE),
                sliderInput("x_zoom", "X-axis zoom:", 
                            min = 0, max = max(df$Seconds), value = c(0, max(df$Seconds)))
              ),
              plotOutput("plotMain", width = "100%"),
              br(),
              plotOutput("plotZoom", width = "100%")
            )
  ),
  
  # Tab 3: Analysis placeholder
  nav_panel("Lag and drift correction",
            layout_sidebar(
              sidebar = sidebar(
                h4("Analysis options will go here")
              ),
              h4("Coming soon...")
            )
  )
)

server <- function(input, output, session) {
  
  # Reactive CSV data
  datafile <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, sep = ",", quote = '"')
  })
  
  # Update Y variable selector after file is loaded
  observeEvent(datafile(), {
    cols <- setdiff(names(datafile()), "Seconds")  # exclude X-axis
    updateSelectInput(session, "yvar", choices = cols, selected = cols[1])
  })
  
  # Data preview
  output$contents <- renderTable({
    df <- datafile()
    if (input$disp == "head") head(df) else df
  })
  
  # Dynamic plot
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
        p <- p + geom_vline(xintercept = marker_times, color = "red", linetype = "solid", alpha = 0.5)
      }
    }
    
    p
  })
}

shinyApp(ui, server)
