
library(parallel)

# Simulate data from a fitted glm MSE model
simulate_data <- function(fitted, nsim, nlists) {
  Y = simulate(fitted, nsim=nsim)

  # Binary table with all combinations of zeros and ones
  X = eval(parse(
      text=paste0("table(", paste0(rep("c(0,1)", nlists), collapse=","), ")")
    )) %>% 
    as.data.frame.table %>% 
    map_dfc(as.numeric) - 1
  X = X[nrow(X):2, nlists:1]
  colnames(X) = paste0("list", 1:nlists)
  
  cbind(Y, X)
}


simulation <- function(fitted, nlists, nsim = 120, ncores=1){
  dat = simulate_data(fitted, nsim=nsim, nlists=nlists)
  
  simulation_names = paste0("sim_", 1:nsim)
  list_names = paste0("list", 1:nlists)
  
  estimates = unlist(mclapply(simulation_names, function(sim) {
    MSEfit(dat[, c(paste0("list", 1:nlists), sim)])$CI[1]
  }, 
  mc.cores = ncores))
  
  return(estimates)
}