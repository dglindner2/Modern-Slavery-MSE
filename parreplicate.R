library(parallel)

cl <- makeCluster(detectCores()-1, 'PSOCK')

parReplicate <- function(cl, n, expr, simplify=TRUE, USE.NAMES=TRUE){
  parSapply(cl, integer(n), function(i, ex) eval(ex, envir=.GlobalEnv),
            substitute(expr), simplify=simplify, USE.NAMES=USE.NAMES)
}

clusterExport(cl, varlist=c("list_intersection", "pop", "y_0", "y_n", "shape",
                            "n_lists", "parReplicate"))

clusterEvalQ(cl, library(tidyverse))
clusterEvalQ(cl, library(dga))
clusterEvalQ(cl, library(MCMCpack))
clusterEvalQ(cl, library(Rcapture))
clusterEvalQ(cl, library(LCMCR))

parReplicate(cl, 1000000, list_intersection(pop, y_0, y_n, shape, n_lists))
