library(shiny)

source("src/simulation/preprocess_dat_mod.R")

#simulations

ui <- fluidPage(
  

  navbarPage("MSE for Modern Slavery",
             
             
             tabPanel("Real Data Sensitivities"),
             
             tabPanel("Coverage and CIs",
                      sidebarPanel(
                        
                        radioButtons("dataset", "Choose the dataset",
                                     choices = c("UKdat", "UKdat_5", "UKdat_4"),
                                     selected = "UKdat"), 
                        selectInput(inputId = "Nb_Sim", 
                                    label = "Number of simulations", 
                                    choices = c("20", "100", "200", "1200", "5000"), 
                                    selected = "20"),
                        checkboxInput("Preprocess", "Use Pre-processed Data", TRUE),
                        h6("Note that running an actual simulation might take from a few seconds to several hours"),
                        actionButton("go", "Run"),
                        actionButton("cancel", "Cancel"),
                        width = 3
                        ),
                      mainPanel(
                        plotOutput(outputId = "hist")%>% withSpinner(), 
                        #https://github.com/andrewsali/shinycssloaders
                        ),
                      tableOutput('table'),
             ),
             
             tabPanel("Generative Model Types"),
             
             tabPanel("Overall Summary")
             
  )
)




server <- function(input, output, session){
  
  dataset <- eventReactive(input$go, {
    input$dataset
  })
  
  
  nsim <- eventReactive(input$go, {
    input$Nb_Sim
  })
  
  preprocess <- eventReactive(input$go, {
    input$Preprocess
  })
  
  estimates <- reactive ({
    if(preprocess() == T){NA}
    else{simulation(dataset(), nsim())}
  })  

  plot <- reactive ({
    if(preprocess() == T){
      plot_est(dataset(), nsim())}
    else{plot_sim(estimates(), dataset())}
  })
  
  output$hist <- renderPlot(
    plot()
  )
  
  table <- reactive ({
    if(preprocess() ==T){
      Tab_Preprocess_int(dataset(), nsim())
    }
    else{Tab_sim_int(estimates(),dataset())
    }
  })
  
  output$table <- renderTable({table()}, align = 'c',
    caption = "True Abundance and Confidence Intervals based on Silverman et al. (2015)",
    caption.placement = getOption("xtable.caption.placement", "bottom"), 
    caption.width = getOption("xtable.caption.width", NULL)
  )
}

shinyApp(ui=ui, server=server)
