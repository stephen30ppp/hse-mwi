# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tibble)
library(plotly)

# Ensure row names are unique in your dataset
df <- read.csv("path_to_your_data.csv")  # Replace with your actual file path

# Fix for duplicate row names
rownames(df) <- make.unique(as.character(df$Numerator))

# Convert a column to row names if necessary
df <- tibble::column_to_rownames(df, var = "Numerator")

# Run your app
shinyApp(
  ui = fluidPage(
    # Your UI components here
  ),
  server = function(input, output) {
    # Your server logic here
  }
)
