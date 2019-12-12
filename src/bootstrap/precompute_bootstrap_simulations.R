library(tidyverse)
library(Rcapture)
source("./src/bootstrap/bootstrap_functions.R")
source("./input/silverman_code/functions.R")
source("./input/silverman_code/datasets.R")


# Run bootstrap simulations
nsim = 100
ncores = min(1, detectCores()-1)

bootstrap_ests_UKdat_4 = simulation(MSEfit(UKdat_4)$fit, 
                                    nlists=4, 
                                    nsim=nsim, 
                                    ncores=ncores
                                    )

bootstrap_ests_UKdat_5 = simulation(MSEfit(UKdat_5)$fit, 
                                    nlists=5, 
                                    nsim=nsim, 
                                    ncores=ncores
                                    )

bootstrap_ests_UKdat_6 = simulation(MSEfit(UKdat)$fit, 
                                    nlists=6, 
                                    nsim=nsim, 
                                    ncores=ncores
                                    )

saveRDS(bootstrap_ests_UKdat_4, "./output/bootstrap/bootstrap_ests_UKdat_4.rds")
saveRDS(bootstrap_ests_UKdat_5, "./output/bootstrap/bootstrap_ests_UKdat_5.rds")
saveRDS(bootstrap_ests_UKdat_6, "./output/bootstrap/bootstrap_ests_UKdat_6.rds")

