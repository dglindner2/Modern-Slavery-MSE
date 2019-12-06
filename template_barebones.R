library(shiny)
library(tidyverse)

ui <- fluidPage(
  
  navbarPage(
    
    tabPanel("Real Data Sensitivities"),
    
    tabPanel("Generative Model Types"),
    
    tabPanel("Overall Summary")
    
  )
)

server <- function(input, output, session){}

shinyApp(ui=ui, server=server)