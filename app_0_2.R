library(shiny)
library(tidyverse)

# load files containing full algorithm as function
load("algorithm.RData")
# source("Detect_as_function.R")

ui <- fluidPage(
  
  # App title ----
  titlePanel("Personal data detector"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      actionButton("submit", "Analyze", icon("refresh"), width = "100px")

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput("contents"),
      
      tableOutput("results")
      
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  data <- reactive({
    read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep,
             quote = input$quote,
             colClasses = "character",
             fileEncoding = "UTF8",
             na.strings = "", 
             strip.white = TRUE)
  })
  
  output$contents <- renderTable({
    req(input$file1)
    head(data())
  })
  
  output$results <- renderTable({
    req(input$file1, input$submit)
    detect_data_type(data())$results
  })
  
}
# Run the app ----
shinyApp(ui, server)