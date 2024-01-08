library(shiny)
library(dplyr)
library(ggplot2)
library(sf)

ui <- fluidPage(
  titlePanel("Map of the samples of the project"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose xlsx File", accept = ".xlsx"),
      actionButton("process_button", "Process and Plot"),
      downloadButton("download_map", "Download Map")
    ),
    
    mainPanel(
      plotOutput("ggplot_map")
    )
  )
)
