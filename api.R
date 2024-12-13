library(shiny)
library(tibble)
library(httr2)
library(randomForest)
library(vetiver)
library(caret)
library(tidyverse)
library(plumber)
library(pins)
library(workflows)
library(dplyr)

api_url <- "http://127.0.0.1:8080/predict"

# Load the FM_housing dataset inside the Shiny app
FM_housing <- read_csv(
  file = "https://raw.githubusercontent.com/gmtanner-cord/DATA470-2024/c39c51759761bff7abd5b42e5ab9ff35bf524515/fmhousing/FM_Housing_2018_2022_clean.csv"
)      
FM_housing <- FM_housing %>%
  rename(
    year_built = `Year Built`,
    total_sqft = `Total SqFt.`,
    sold_price = `Sold Price`
  )

# Scaling variables for the input
scaling_vars <- preProcess(
  select(FM_housing, year_built, total_sqft),  # Use the same columns as used in training
  method = c("center", "scale")
)

# UI
ui <- fluidPage(
  titlePanel("Sold Price Predictor"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_built", "Year Built (1800-2000)", min = 1800, max = 2000, value = 1900, step = 1),
      sliderInput("total_sqft", "Total Square Footage", min = 500, max = 10000, value = 5000, step = 100),
      actionButton("predict", "Predict")
    ),
    
    mainPanel(
      h2("Housing Parameters"),
      verbatimTextOutput("vals"),
      h2("Predicted Sold Price"),
      textOutput("pred")
    )
  )
)

server <- function(input, output) {
  # Input params
  vals <- reactive({
    tibble(
      year_built = input$year_built,
      total_sqft = input$total_sqft
    )
  })
  
  # Apply scaling to the inputs before sending them to the API
  scaled_vals <- reactive({
    predict(scaling_vars, vals())  # Apply the same scaling as done in training
  })
  
  # Fetch prediction from API
  pred <- eventReactive(
    input$predict,
    {
      res <- httr2::request(api_url) |>
        httr2::req_body_json(scaled_vals()) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
      
      # Print or inspect the structure of the response
      str(res)  # This will show you the exact structure of the API response
      res  # Return the response for later use
    },
    ignoreInit = TRUE
  )
  
  # Render the predicted value to the UI
  output$pred <- renderText({
    if (!is.null(pred()) && length(pred()) > 0) {
      if (is.numeric(pred())) {
        paste0("Predicted Sold Price: $", round(pred(), 2))
      } else {
        paste0("Predicted Sold Price: $", round(pred()$.pred[[1]], 2))
      }
    } else {
      "Prediction not available"
    }
  })
  
  output$vals <- renderPrint(vals())
}

# Run the application
shinyApp(ui = ui, server = server)

