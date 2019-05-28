library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)

# Data from the Social Security Administration
data <- read.csv("wa_unhealthy_air.csv", stringsAsFactors = FALSE)


# UI
ui <- fluidPage(
  titlePanel("WA Unhealthy Air"),
  
  # A few paragraphs to give context
  p("We are examining the percentage of days with unsafe PM2.5 percentages in different counties of Washington State."),
  p("PM2.5 refers to particles in the air that are tiny and dangerous to inhale."),
    
  sidebarLayout(
    sidebarPanel(
      # Put a widget here for the user to interact with!
      # You could filter by year, by county, etc.
      selectInput("chosenCounties", 
                  label = "Counties to view:",
                  choices = unique(data$CountyName),
                  selected = unique(data$CountyName),
                  multiple = TRUE),
    
    sliderInput("chosenYears",
                "Year",
                min = min(data$ReportYear),
                max = max(data$ReportYear),
                value = c(min(data$ReportYear), max(data$ReportYear)),
                sep = "",
                step = 1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("airQualityPlot")),
        tabPanel("Table", tableOutput("airQualityText"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  chosen_data <- reactive({
    data %>%
      filter(CountyName %in% input$chosenCounties,
             ReportYear >= input$chosenYears[1],
             ReportYear <= input$chosenYears[2])
  })
  
  # A plot of air quality
  # This can be over time, by county, etc.
  output$airQualityPlot <- renderPlot({
    chosen_data() %>% 
      ggplot() +
      geom_line(aes(x = ReportYear, y = Value, color = CountyName)) +
      labs(title = "Washington state air quality over time",
           subtitle = "Percent of days with PM2.5 levels over the National Ambient Air Quality Standard (NAAQS)",
           x = "Year",
           y = "Percent of unsafe days")
  })
  
  # A table of the filtered data
  output$airQualityTable <- renderTable({
    chosen_data()
  })
}

# Run the application
shinyApp(ui = ui, server = server)