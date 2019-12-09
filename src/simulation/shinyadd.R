
library(shiny)

library(tidyverse)


ui <- fluidPage(
  
  navbarPage(
    
    tabPanel("Real Data Sensitivities"),
    
    tabPanel("Generative Model Types",
             sidebarPanel(  radioButtons("dataset", "Choose the dataset",
                                        choices = c("UKdat", "UKdat_5", "UKdat_4"),
                                        selected = "UKdat"), 
                            selectInput(inputId = "Nb_Sim", label = "Number of simulations", 
                         choices = c("20", "100", "150", "200"), selected = "20"),
                         submitButton("Apply Changes"),
                         width = 4),
             mainPanel(plotOutput(outputId = "hist")),
             ),
    
    tabPanel("Overall Summary")
    
  )
)

server <- function(input, output, session){
  
  simulation <- function(dataset, val, nsim = 1200, n_lists){
    
    dat = simulate_data(val, nsim=nsim, n_lists=n_lists)
    simulation_names = paste0("sim_", 1:nsim)
    list_names = paste0("list", 1:n_lists)
    
    
    estimates = unlist(mclapply(simulation_names, function(sim) {
      MSEfit(dat[, c(paste0("list", 1:n_lists), sim)])$CI[1]
    }, 
    mc.cores = 12))
    
  }
  
  
  

  dataInput <- reactive({
    getSymbols(dataset = ifelse(input$dataset == "UKdat", UKdat, 
                                ifelse(input$dataset == "UKdat_5", UKdat_5, UKdat_4)),
               n_lists = ifelse(input$dataset == "UKdat", 6, 
                                ifelse(input$dataset == "UKdat_5", 5, 4)),
               val = ifelse(input$dataset == "UKdat", model6$fit, 
                      ifelse(input$dataset == "UKdat_5", model5$fit, model4$fit)),
               nsim = ifelse(input$Nb_sim == "20", 20, ifelse(input$Nb_sim == "100", 100, 
                                                              ifelse(input$Nb_sim == "150", 150,
                                                                     200))),
               auto.assign = FALSE)
  })
  

  
  output$hist <- renderPlot({
    
    hist(estimates = simulation(dataset = dataInput$dataset, val = dataInput$val, 
                                nsim = dataInput$nsim, n_lists = dataInput$n_lists), 
         xlab="Estimated abundance", main = sprintf("Generated Estimates -- %s lists", 
                                                    dataInput$n_lists), 
         freq = T, xlim = range(9000,15000), col = "#81DAF5", border = "#FFFFFF", breaks = 6)
    abline(v = MSEfit(dataInput$dataset)$CI[1])
    abline(v = MSEfit(dataInput$dataset)$CI[2], lty=2)
    abline(v = MSEfit(dataInput$dataset)$CI[3], lty=2)
  })

}

shinyApp(ui=ui, server=server)
