library(shiny)

library(knitr)
# install.packages("kableExtra")
library(kableExtra)
library(tidyverse)

# install.packages(c("dga", "MCMCpack", "Rcapture", "LCMCR", "MASS", "coda"))
library(dga)
library(MCMCpack)
library(Rcapture)
library(LCMCR)
library(UpSetR)
source("input/silverman_code/datasets.R")
source("input/silverman_code/functions.R")

extract_dataset <- function(dataset){
  
  if(!exists("UKdat")){
    source("input/silverman_code/datasets.R")
  }
  
  if(is.character(dataset)){
    data1 <- get(dataset)
  }
  
  else if(is.data.frame(dataset)){
    data1 <- dataset
  }
  else{
    stop("dataset must be either a string referring to a data frame or a data frame.")
  }
  
  colnames(data1)[ncol(data1)] <- "count"
  
  return(data1)
}



list_choices <- function(dataset){
  
  dataset <- extract_dataset(dataset)
  
  cols <- colnames(dataset)
  
  return(cols[-length(cols)])
}

combine_lists <- function(dataset, listnames, remove_orig_groups=TRUE){
  
  dataset <- extract_dataset(dataset)
  
  listname_opts <- list_choices(dataset)
  
  if(sum(listnames %in% listname_opts) != length(listnames)){
    stop(paste0("all list names must be columns within ", dataset))
  }
  
  if(remove_orig_groups){
    listnames_keep <- listname_opts[!(listname_opts %in% listnames)]
  }
  else{
    listnames_keep <- listname_opts
  }
  
  data_comblists <- dataset[,listnames]
  
  combined_list <- as.data.frame(ifelse(rowSums(data_comblists), 1, 0))
  combined_listname <- paste(listnames, collapse="_")
  colnames(combined_list) <- combined_listname
  
  dataset_combined <- cbind(dataset[,listnames_keep], combined_list, dataset[ncol(dataset)])
  
  
  colnames(dataset_combined) <- c(listnames_keep, combined_listname, "count")
  
  dataset_grouped <- dataset_combined %>%
    group_by_at(colnames(dataset_combined)[!grepl("count", colnames(dataset_combined))]) %>%
    summarize(count = sum(count))
  
  return(dataset_grouped)
}

create_dataset <- function(dataset, lists){
  df_list <- lapply(lists, function(i){combine_lists(dataset, i, remove_orig_groups = FALSE)})
  
  data <- extract_dataset(dataset)  
  data_new <- reduce(df_list, inner_join, by=colnames(data))
  
  joining_cols <- colnames(data)[!grepl("count", colnames(data))]
  unique_cols <- setdiff(unique(unlist(map(df_list, colnames))),
                         colnames(data))
  
  data_grouped <- data_new %>%
    ungroup() %>%
    dplyr::select(-one_of(joining_cols)) %>%
    group_by_at(unique_cols) %>%
    summarize(count = sum(count))
  
  return(data_grouped)
}

plot_MSE_counts <- function(data, ...) {
  
  data <- as.data.frame(data)
  n = ncol(data) - 1
  dat = uncount(data[, 1:n], data$count)
  
  upset(dat, nsets=n, ...)
}



ui <- fluidPage(
  
  navbarPage(
    
    tabPanel("Real Data Sensitivities",
             sidebarPanel(
               selectizeInput(inputId = "list1",
                              label = "First List (select up to 6 existing lists)",
                              choices = colnames(UKdat),
                              multiple = TRUE,
                              options = list(maxItems = 6)),
                          
               selectizeInput(inputId = "list2",
                              label = "Second List (select up to 6 existing lists)",
                              choices = colnames(UKdat),
                              multiple = TRUE,
                              options = list(maxItems = 6)),
                          
               selectizeInput(inputId = "list3",
                              label = "Third List (select up to 6 existing lists)",
                              choices = colnames(UKdat),
                              multiple = TRUE,
                              options = list(maxItems = 6)),
                          
               selectizeInput(inputId = "list4",
                              label = "Fourth List (select up to 6 existing lists)",
                              choices = colnames(UKdat),
                              multiple = TRUE,
                              options = list(maxItems = 6)),
                          
               selectizeInput(inputId = "list5",
                              label = "Fifth List (select up to 6 existing lists)",
                              choices = colnames(UKdat),
                              multiple = TRUE,
                              options = list(maxItems = 6)),
                          
               selectizeInput(inputId = "list6",
                              label = "Sixth List (select up to 6 existing lists)",
                              choices = colnames(UKdat),
                              multiple = TRUE,
                              options = list(maxItems = 6)),
               div(align="right",
                   actionButton(inputId = "create_data", label="Create Customized Dataset"))),
             mainPanel(
               plotOutput(outputId = "MSE_plot"))),
    
    tabPanel("Generative Model Types"),
    
    tabPanel("Overall Summary")
    
  )
)

server <- function(input, output, session){
  
  updated_dataset <- reactive({
    
    input$create_data
    
    isolate({
      list_df <- list(input$list1, input$list2, input$list3, 
                      input$list4, input$list5, input$list6)
      
      dataset <- create_dataset(UKdat, list_df)})
    
    
    
  })
  
  output$MSE_plot <- renderPlot(
    plot_MSE_counts(updated_dataset(), 
                    # empty.intersections=T,
                    sets.bar.color = "cornflowerblue",
                    main.bar.color = "cornflowerblue",
                    matrix.color = "cornflowerblue",
                    mainbar.y.label = "Potential victims count",
                    sets.x.label = "")
  )
}

shinyApp(ui=ui, server=server)