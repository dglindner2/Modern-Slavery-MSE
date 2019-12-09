library(shiny)
library(tidyverse)
source("Sim_Functions.R")

ui <- fluidPage(

  navbarPage("MSE for Modern Slavery",
  
    tabPanel("Real Data Sensitivities"),
    tabPanel("Generative Model Types", fluid = TRUE,   
             sidebarLayout(
      sidebarPanel(
        sliderInput(inputId = "pop_size", label = "Population Size", 
                  min = 100, max = 20000, value = 10000, step = 50, ticks = F),
        sliderInput(inputId = "high_prob", label = "Highest Inclusion Probabilty", 
                    min = 0, max = 100, value = 5, step = .5, ticks = F, post = '%'),
        sliderInput(inputId = "low_prob", label = "Lowest Inclusion Probability", 
                    min = 0, max = 100, value = 1, step = .5, ticks = F, post = '%'),
        sliderInput(inputId = "shape", label = "Shape of Inclusion Line (1 is a straight line)", 
                    min = .05, max = 5, value = 1, step = .05, ticks = F),
        sliderInput(inputId = "lists", label = "Number of Lists", 
                    min = 1, max = 7, value = 5, step = 1, ticks = F),
        sliderInput(inputId = "referal", label = "List Referal Probability", 
                    min = 0, max = 100, value = 0, step = 1, ticks = F, post = '%'),
        sliderInput(inputId = "removal", label = "List Removal Probability", 
                    min = 0, max = 100, value = 0, step = 1, ticks = F, post = '%'),
        sliderInput(inputId = "sims", label = "Number of Simulations to Run", 
                    min = 100, max = 10000, value = 1000, step = 50, ticks = F),
        actionButton("run_sim", "Run"),
        br(),
        plotOutput(outputId = 'inc_plot')),
      mainPanel(plotOutput(outputId = "count_plot"),
                plotOutput(outputId = "sim_plot"))
    )),
    tabPanel("Overall Summary")
    
))


server <- function(input, output, session){
    
    N <- eventReactive(input$run_sim,{
      input$pop_size
    })
    
    observeEvent(input$high_prob,  {
      updateSliderInput(session = session, inputId = "low_prob", max = input$high_prob)
    })
    
    y_high <- eventReactive(input$run_sim, {
      input$high_prob/100
      })
    
    y_low <- eventReactive(input$run_sim,{
      input$low_prob/100
    })
    
    Shape <- eventReactive(input$run_sim,{
      input$shape
    })
    
    Lists <- eventReactive(input$run_sim,{
      input$lists
    })
    
    Ref <- eventReactive(input$run_sim,{
      input$referal/100
    })
    
    Rem <- eventReactive(input$run_sim,{
      input$removal/100
    })
    
    Sims <- eventReactive(input$run_sim,{
      input$sims
    })

    sidebar_plot <- reactive({inclusion(N(), y_high(), y_low(), Shape())})
  
    output$inc_plot <- renderPlot(
      sidebar_plot()
    )
    
    MSEcount_plot <- reactive({plot_MSE_counts(refer_remove(
      N(), y_high(), y_low(), Shape(), Lists(), Ref(), Rem()), 
      sets.bar.color = "cornflowerblue",
      main.bar.color = "cornflowerblue",
      matrix.color = "cornflowerblue",
      mainbar.y.label = "Potential victims count",
      sets.x.label = "")})
    
    output$count_plot <- renderPlot(
      MSEcount_plot()
    )
    
    MSEsim_plot <- reactive({MSEFixed_plot(N(), y_high(),
                                           y_low(), Shape(),
                                           Lists(), Ref(), Rem(), Sims())})
    
    output$sim_plot <- renderPlot(
      MSEsim_plot()
    )
  
}



shinyApp(ui=ui, server=server)

