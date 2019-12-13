
# install.packages(c("dga", "MCMCpack", "Rcapture", "LCMCR", "MASS", "coda"))
source("src/simulation/models_functions.R")

# Models with each dataset
model6 <- MSEfit(UKdat)
saveRDS(model6, file.path("src/simulation/model6.Rds"))
model5 <- MSEfit(UKdat_5)
saveRDS(model5, file.path("src/simulation/model5.Rds"))
model4 <- MSEfit(UKdat_4)
saveRDS(model4, file.path("src/simulation/model4.Rds"))

# save hist function
plot.est <- function(dataset, estimates, n_lists, xlim, breaks){
  dataset.name = deparse(substitute(dataset))
  png(file = sprintf("src/simulation/%splot.png", dataset.name))
  hist(estimates, xlab="Estimated abundance", main = sprintf(
    "Generated Estimates -- %s lists", n_lists), freq = T, xlim = xlim, 
    col = "#81DAF5", border = "#FFFFFF", breaks = breaks)
  abline(v = MSEfit(dataset)$CI[1])
  abline(v = MSEfit(dataset)$CI[2], lty=2)
  abline(v = MSEfit(dataset)$CI[3], lty=2)
  dev.off()
}

# UKdat
set.seed(12)
estimates6 <- simulation(dataset = "UKdat")
saveRDS(estimates6, file.path(sprintf("src/simulation/UKdatsim.Rds")))
estimates620 <- simulation(dataset = "UKdat", nsim = "20")
saveRDS(estimates620, file.path(sprintf("src/simulation/UKdatsim20.Rds")))
estimates6100 <- simulation(dataset = "UKdat", nsim = "100")
saveRDS(estimates6100, file.path(sprintf("src/simulation/UKdatsim100.Rds")))
estimates6200 <- simulation(dataset = "UKdat", nsim = "200")
saveRDS(estimates6200, file.path(sprintf("src/simulation/UKdatsim200.Rds")))
plot6 <- plot.est(dataset = UKdat, estimates6, n_lists = 6,
                  xlim = range(8000, 20000), breaks = 16)
coverage6 = mean(estimates6 <= model6$CI[3] & estimates6 >= model6$CI[2])

# UKdat_5
set.seed(14)
estimates5 <- simulation(dataset = "UKdat_5")
saveRDS(estimates5, file.path(sprintf("src/simulation/UKdat_5sim.Rds")))
estimates520 <- simulation(dataset = "UKdat_5", nsim = "20")
saveRDS(estimates520, file.path(sprintf("src/simulation/UKdat_5sim20.Rds")))
estimates5100 <- simulation(dataset = "UKdat_5", nsim = "100")
saveRDS(estimates5100, file.path(sprintf("src/simulation/UKdat_5sim100.Rds")))
estimates5200 <- simulation(dataset = "UKdat_5", nsim = "200")
saveRDS(estimates5200, file.path(sprintf("src/simulation/UKdat_5sim200.Rds")))
plot5 <- plot.est(dataset = UKdat_5, estimates5, n_lists = 5)
coverage5 = mean(estimates5 <= model5$CI[3] & estimates5 >= model5$CI[2])


# UKdat_4
set.seed(2019)
estimates4 <- simulation(dataset = "UKdat_4")
saveRDS(estimates4, file.path(sprintf("src/simulation/UKdat_4sim.Rds")))
estimates420 <- simulation(dataset = "UKdat_4", nsim = "20")
saveRDS(estimates420, file.path(sprintf("src/simulation/UKdat_4sim20.Rds")))
estimates4100 <- simulation(dataset = "UKdat_4", nsim = "100")
saveRDS(estimates4100, file.path(sprintf("src/simulation/UKdat_4sim100.Rds")))
estimates4200 <- simulation(dataset = "UKdat_4", nsim = "200")
saveRDS(estimates4200, file.path(sprintf("src/simulation/UKdat_4sim200.Rds")))
plot4 <- plot.est(dataset = UKdat_4, estimates4, n_lists = 4)
coverage4 = mean(estimates4 <= model4$CI[3] & estimates4 >= model4$CI[2])


tablesum <- data.frame(models = c("UKdat\n(6 lists)", "UKdat_5\n(5 lists)", "UKdat_4\n(4 lists)"),
           true_abundance = c(11416, 11313,11015),
           Lwr = c(9982, 9889, 9587),
           upr = c(13181, 13063, 12771),
           coverage= c(coverage6, coverage5, coverage4))
saveRDS(tablesum, file.path("src/simulation/tablesum.Rds"))
