

library(shiny)
library(tidyverse)
library(scales)

data <- read.csv("pnfs_key_metrics_long.csv")

data <- data |>
  mutate(
    date = as.Date(date),
    metric = as.factor(metric)
  )

ui <- fluidPage(
  
  titlePanel("Interactive Transportation Metrics Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Dashboard Controls"),
      
      selectInput(
        inputId = "selected_metric",
        label = "Select Metric:",
        choices = unique(data$metric),
        selected = "Total Passengers"
      ),
      
      dateRangeInput(
        inputId = "date_range",
        label = "Select Date Range:",
        start = min(data$date),
        end = max(data$date)
      )
    ),
    
    mainPanel(
      plotOutput("trend_plot")
    )
  )
)

server <- function(input, output) {
  
  filtered_data <- reactive({
    data |>
      filter(
        metric == input$selected_metric,
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
  })
  
  output$trend_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = date, y = value)) +
      geom_line() +
      geom_point() +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
