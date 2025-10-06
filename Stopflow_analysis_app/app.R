library(shiny)
library(bslib)
library(ggplot2)

# Define UI ----
ui <- page_sidebar(
  
  title = "Uploading Files",
  
  sidebar = sidebar(
    fileInput(
      "file1",
      "Choose CSV File",
      multiple = FALSE,
      accept = c("text/csv", ".csv")
    ),
    tags$hr(),
    radioButtons(
      "disp",
      "Display",
      choices = c(Head = "head", All = "all"),
      selected = "head"
    )
  ),
  
  # Main panel: data preview + plot
  mainPanel(
    tableOutput("contents"),
    plotOutput("plotO2", width = "150%"),
    plotOutput("plotCO2", width = "150%"),
    plotOutput("plotFR", width = "150%"),
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # Reactive expression to read file
  datafile <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, sep = ",", quote = '"')
  })
  
  # Show data table
  output$contents <- renderTable({
    df <- datafile()
    if (input$disp == "head") head(df) else df
  })
  
  # Show plotO2
  output$plotO2 <- renderPlot({
    df <- datafile()
    
    ggplot(df, aes(x = Seconds, y = O2)) +
      geom_line(color = "blue") +
      theme_minimal(base_size = 14) +
      labs(
        title = "O2 vs Time",
        x = "Seconds",
        y = "O2"
      )
  })
  
  # Show plotCO2
  output$plotCO2 <- renderPlot({
    df <- datafile()
    
    ggplot(df, aes(x = Seconds, y = CO2)) +
      geom_line(color = "red") +
      theme_minimal(base_size = 14) +
      labs(
        title = "CO2 vs Time",
        x = "Seconds",
        y = "CO2"
      )
  })
  
  # Show plotFR
  output$plotFR <- renderPlot({
    df <- datafile()
    
    ggplot(df, aes(x = Seconds, y = FlowRate)) +
      geom_line(color = "black") +
      theme_minimal(base_size = 14) +
      labs(
        title = "FlowRate vs Time",
        x = "Seconds",
        y = "FlowRate"
      )
  })
}


# Run app ----
shinyApp(ui, server)
