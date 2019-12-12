library(shinythemes)

shinyUI(
  navbarPage("MSE Benchmark", theme = shinytheme("flatly"), 
             
             tabPanel("Data Application",
                      sidebarLayout(
                        sidebarPanel(
                          
                        ),
                        mainPanel(
                          
                        )
                        )
                      ),
             
             tabPanel("Accuracy Estimation",
                      sidebarLayout(
                        sidebarPanel(
                          
                        ),
                        mainPanel(
                          
                        )
                      )
             ),
             
             tabPanel("Generative Models",
                      sidebarLayout(
                        sidebarPanel(
                          
                        ),
                        mainPanel(
                          
                        )
                      )
             )
  )
)
