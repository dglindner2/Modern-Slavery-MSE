# read in preprocessed simulations
estimates620 <- readRDS("src/simulation/sims/UKdatsim20.Rds") # Ukdat (6 lists) 20 simulations
estimates6100 <- readRDS("src/simulation/sims/UKdatsim100.Rds")
estimates6200 <- readRDS("src/simulation/sims/UKdatsim200.Rds")
estimates6 <- readRDS("src/simulation/sims/UKdatsim.Rds") # UKdat (6 lists) 1200 simulations
estimates520 <- readRDS("src/simulation/sims/UKdat_5sim20.Rds")
estimates5100 <- readRDS("src/simulation/sims/UKdat_5sim100.Rds")
estimates5200 <- readRDS("src/simulation/sims/UKdat_5sim200.Rds")
estimates5 <- readRDS("src/simulation/sims/UKdat_5sim.Rds")
estimates420 <- readRDS("src/simulation/sims/UKdat_4sim20.Rds")
estimates4100 <- readRDS("src/simulation/sims/UKdat_4sim100.Rds")
estimates4200 <- readRDS("src/simulation/sims/UKdat_4sim200.Rds")
estimates4 <- readRDS("src/simulation/sims/UKdat_4sim.Rds")
tablesum <- readRDS("src/simulation/sims/tablesum.Rds")

# model
model6 <- readRDS("src/simulation/models/model6.Rds") # Silverman's model with UKdat (6 lists)
model5 <- readRDS("src/simulation/models/model5.Rds")
model4 <- readRDS("src/simulation/models/model4.Rds")


library(tidyverse)
library(pbapply)
# install.packages(c("dga", "MCMCpack", "Rcapture", "LCMCR", "MASS", "coda"))
library(parallel)
source("input/silverman_code/datasets.R")
source("input/silverman_code/functions.R")


# Function to generate the simulation
simulate_data <- function(fitted, nsim, n_lists) {
  Y = simulate(fitted, nsim=nsim)
  mat = model.matrix(fitted)
  
  # Binary table with all combinations of zeros and ones
  X = eval(parse(text=
                   paste0("table(", paste0(rep("c(0,1)", n_lists), collapse=","), ")")
  )) %>% 
    as.data.frame.table %>% 
    map_dfc(as.numeric) - 1
  X = X[nrow(X):2, n_lists:1]
  colnames(X) = paste0("list", 1:n_lists)
  
  cbind(Y, X)
}


# Simulation function
simulation <- function( dataset, nsim = "1200"){
  if(dataset == "UKdat") {val <- model6$fit}
  else if(dataset == "UKdat_5") {val <- model5$fit}
  else{val <- model4$fit}
  n_lists = ifelse(dataset == "UKdat", 6, ifelse(dataset == "UKdat_5", 5, 4))
  dat = simulate_data(val, nsim = as.numeric(nsim), n_lists = n_lists)
  simulation_names = paste0("sim_", 1:as.numeric(nsim))
  list_names = paste0("list", 1:n_lists)
  
  estimates = unlist(mclapply(simulation_names, function(sim) {
    MSEfit(dat[, c(paste0("list", 1:n_lists), sim)])$CI[1]
  }, 
  mc.cores = 12))
}


# Function to plot preprocessed simulations
plot_est <- function(dataset, nsim){
  # UKdat
  if(dataset == "UKdat"){
    if(nsim == "20"){
      hist(x = estimates620, xlab="Estimated abundance", main = "Generated Estimates",
          col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates620)^.33) + 3)
      abline(v = model6$CI[1])
      abline(v = model6$CI[2], lty=2)
      abline(v = model6$CI[3], lty=2)
      legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
             col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
             title = "* According to Silverman et al.")}
    else if(nsim == "100"){
      hist(estimates6100, xlab="Estimated abundance", main = "Generated Estimates",
           col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates620)^.33) + 3)
      abline(v = model6$CI[1])
      abline(v = model6$CI[2], lty=2)
      abline(v = model6$CI[3], lty=2)
      legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
             col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
             title = "* According to Silverman et al.")}
    else if (nsim == "200"){
      hist(estimates6200, xlab="Estimated abundance", main = "Generated Estimates",
           col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates6200)^.33) + 3)
      abline(v = model6$CI[1])
      abline(v = model6$CI[2], lty=2)
      abline(v = model6$CI[3], lty=2)
      legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
             col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
             title = "* According to Silverman et al.")}
    else if(nsim == "1200"){
      hist(estimates6, xlab="Estimated abundance", main = "Generated Estimates",
           col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates6)^.33) + 3)
      abline(v = model6$CI[1])
      abline(v = model6$CI[2], lty=2)
      abline(v = model6$CI[3], lty=2)
      legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
             col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
             title = "* According to Silverman et al.")}
  }
  
  #UKdat_5
  else if(dataset == "UKdat_5"){
    if(nsim == "20"){
      hist(estimates520, xlab="Estimated abundance", main = "Generated Estimates",
        col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates520)^.33) + 3)
      abline(v = model5$CI[1])
      abline(v = model5$CI[2], lty=2)
      abline(v = model5$CI[3], lty=2)
      legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
             col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
             title = "* According to Silverman et al.")}
    else if(nsim == "100"){
      hist(estimates5100, xlab="Estimated abundance", main = "Generated Estimates",
           col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates5100)^.33) + 3)
      abline(v = model5$CI[1])
      abline(v = model5$CI[2], lty=2)
      abline(v = model5$CI[3], lty=2)
      legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
             col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
             title = "* According to Silverman et al.")
    }
    else if (nsim == "200"){
      hist(estimates5200, xlab="Estimated abundance", main = "Generated Estimates",
           col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates5200)^.33) + 3)
      abline(v = model5$CI[1])
      abline(v = model5$CI[2], lty=2)
      abline(v = model5$CI[3], lty=2)
      legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
             col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
             title = "* According to Silverman et al.")
    }
    else if(nsim == "1200"){
      hist(estimates5, xlab="Estimated abundance", main = "Generated Estimates",
           col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates5)^.33) + 3)
      abline(v = model5$CI[1])
      abline(v = model5$CI[2], lty=2)
      abline(v = model5$CI[3], lty=2)
      legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
             col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
             title = "* According to Silverman et al.")
    }
  }
  
  
  # UKdat_4
  if(dataset == "UKdat_4"){
    if(nsim == "20"){
      hist(estimates420, xlab="Estimated abundance", main = "Generated Estimates",
        col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates420)^.33) + 3)
      abline(v = model4$CI[1])
      abline(v = model4$CI[2], lty=2)
      abline(v = model4$CI[3], lty=2)
      legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
             col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
             title = "* According to Silverman et al.")
      }
    else if(nsim == "100"){
      hist(estimates4100, xlab="Estimated abundance", main = "Generated Estimates",
           col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates4100)^.33) + 3)
      abline(v = model4$CI[1])
      abline(v = model4$CI[2], lty=2)
      abline(v = model4$CI[3], lty=2)
      legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
             col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
             title = "* According to Silverman et al.")
    }
    else if (nsim == "200"){
      hist(estimates4200, xlab="Estimated abundance", main = "Generated Estimates",
           col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates4200)^.33) + 3)
      abline(v = model4$CI[1])
      abline(v = model4$CI[2], lty=2)
      abline(v = model4$CI[3], lty=2)
      legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
             col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
             title = "* According to Silverman et al.")
    }
    else if(nsim == "1200"){
      hist(estimates4, xlab="Estimated abundance", main = "Generated Estimates",
           col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates4)^.33) + 3)
      abline(v = model4$CI[1])
      abline(v = model4$CI[2], lty=2)
      abline(v = model4$CI[3], lty=2)
      legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
             col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
             title = "* According to Silverman et al.")
    }
    else{hist(simulation("UKdat_4", "5000"), xlab="Estimated abundance", main = "Generated Estimates",
              col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates())^.33) + 3)
      abline(v = model4$CI[1])
      abline(v = model4$CI[2], lty=2)
      abline(v = model4$CI[3], lty=2)
      legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
             col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
             title = "* According to Silverman et al.")}
  }
}

# Function to plot real siulation
plot_sim <- function(estimates, dataset){
  
  if(dataset == "UKdat"){
    hist(estimates, xlab="Estimated abundance", main = "Generated Estimates", 
         col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates)^.33) + 3)
    abline(v = model6$CI[1])
    abline(v = model6$CI[2], lty=2)
    abline(v = model6$CI[3], lty=2)
    legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
           col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
           title = "* According to Silverman et al.")
  }
  
  else if(dataset == "UKdat_5"){
    hist(estimates, xlab="Estimated abundance", main = "Generated Estimates", 
         col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates)^.33) + 3)
    abline(v = model5$CI[1])
    abline(v = model5$CI[2], lty=2)
    abline(v = model5$CI[3], lty=2)
    legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
           col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
           title = "* According to Silverman et al.")
  }
  
  else{
    hist(estimates, xlab="Estimated abundance", main = "Generated Estimates", 
         col = "#81DAF5", border = "#FFFFFF", breaks = 3*floor(length(estimates)^.33) + 3)
    abline(v = model4$CI[1])
    abline(v = model4$CI[2], lty=2)
    abline(v = model4$CI[3], lty=2)
    legend("topright", legend = c("Estimate # of victims*", "Confidence Intervals*"), 
           col = c("black", "black"), lty = c(1,2), cex=0.8, box.lty=0, 
           title = "* According to Silverman et al.")
  }
}

TablePreprocess <- function(dataset, nsim){
  if(dataset == "UKdat"){
    if(nsim == "20"){data.frame("True Abundance" = 11416,
                                Lwr = 9982, Upr = 13181,
                                Coverage = mean(estimates620 <= model6$CI[3] & 
                                                estimates620 >= model6$CI[2]))}
    else if (nsim == "100"){data.frame("True Abundance" = 11416,
                                       Lwr = 9982, Upr = 13181,
                                       Coverage = mean(estimates6100 <= model6$CI[3] & 
                                                         estimates6100 >= model6$CI[2]))}
    else if (nsim == "200"){data.frame("True Abundance" = 11416,
                                       Lwr = 9982, Upr = 13181,
                                       Coverage = mean(estimates6200 <= model6$CI[3] & 
                                                         estimates6200 >= model6$CI[2]))}
    else if(nsim== "1200") {tablesum[1,-1]}
    else{data.frame(Error = "Sorry, no preprocessed simulation was ran for that # of simulations. Please choose a smaller # of simulations or run an actual simulation noting that it might take up to several hours.")}
  }
  else if (dataset == "UKdat_5"){
    if(nsim == "20"){data.frame("True Abundance" = 11313,
                                Lwr = 9889,Upr = 13063,
                                Coverage = mean(estimates520 <= model5$CI[3] & 
                                                  estimates520 >= model5$CI[2]))}
    else if (nsim == "100"){data.frame("True Abundance" = 11313,
                                       Lwr = 9889,Upr = 13063,
                                       Coverage = mean(estimates5100 <= model5$CI[3] & 
                                                         estimates5100 >= model5$CI[2]))}
    else if (nsim == "200"){data.frame("True Abundance" = 11313,
                                       Lwr = 9889,Upr = 13063,
                                       Coverage = mean(estimates5200 <= model5$CI[3] & 
                                                         estimates5200 >= model5$CI[2]))}
    else if(nsim== "1200") {tablesum[2,-1]}
    else{data.frame(Error = "Sorry, no preprocessed simulation was ran for that # of simulations. Please choose a smaller # of simulations or run an actual simulation noting that it might take up to several hours.")}
  }
    
  else{
    if(nsim == "20"){data.frame("True Abundance" = 11015,
                                Lwr = 9587, Upr = 12771,
                                Coverage = mean(estimates420 <= model4$CI[3] & 
                                                estimates420 >= model4$CI[2]))}
    else if (nsim == "100"){data.frame("True Abundance" = 11015,
                                       Lwr = 9587, Upr = 12771,
                                       Coverage = mean(estimates4100 <= model4$CI[3] & 
                                                       estimates4100 >= model4$CI[2]))}
    else if (nsim == "200"){data.frame("True Abundance" = 11015,
                                       Lwr = 9587, Upr = 12771,
                                       Coverage = mean(estimates4200 <= model4$CI[3] & 
                                                       estimates4200 >= model4$CI[2]))}
    else if(nsim== "1200") {tablesum[3,-1]}
    else{data.frame(Error = "Sorry, no preprocessed simulation was ran for that # of simulations. Please choose a smaller # of simulations or run an actual simulation noting that it might take up to several hours.")}
  }
}

# making first 3 columns as integer
Tab_Preprocess_int <- function(dataset, nsim){
  tab <- TablePreprocess(dataset, nsim)
  tab[,1:3] <- apply(tab[,1:3], 2, as.integer )
  return (tab)
}

# Table for actual simulations
Table_sim <- function(estimates, dataset){
  if(dataset == "UKdat"){data.frame("True Abundance" = 11416,
                                      Lwr = 9982, Upr = 13181,
                                      Coverage = mean(estimates <= model6$CI[3] & 
                                                        estimates >= model6$CI[2]))}
  else if (dataset == "UKdat_5"){data.frame("True Abundance" = 11313,
                                              Lwr = 9889,Upr = 13063,
                                              Coverage = mean(estimates <= model5$CI[3] & 
                                                                estimates >= model5$CI[2]))}
  else{data.frame("True Abundance" = 11015,
                  Lwr = 9587, Upr = 12771,
                  Coverage = mean(estimates <= model4$CI[3] & 
                                    estimates >= model4$CI[2]))
  }
}

# making first 3 columns as integer
Tab_sim_int <- function(estimates, dataset){
  tab <- Table_sim(estimates, dataset)
  tab[,1:3] <- apply(tab[,1:3], 2, as.integer )
  return (tab)
}
