complete_MSE_data <- function(dat) {
  # Completes MSE data with zero counts. Lists are renamed to `c1`, `c2`, ..., 
  # for consistency and the counts are listed under the `count` column.
  # 
  # Examples:
  # ========
  #  > source("datasets.R") # Silverman (2020) datasets
  #  > data = complete_MSE_data(UKdat)
  #  > glm(count ~ c1+c2+c3, family="poisson", data=data)
  
  n_cols = ncol(dat)
  
  # Binary table with all combinations of zeros and ones
  X = eval(parse(text=
                   paste0("table(", paste0(rep("c(0,1)", n_cols-1), collapse=","), ")")
  )) %>% 
    as.data.frame.table %>% 
    map_dfc(as.numeric) - 1
  
  # Removing the count for unobserved cases and removing superfluous column
  X = X[2:nrow(X), 1:(n_cols-1)]
  
  
  # Match column names of the data to those of the binary matrix
  covariate_names = paste0("c", 1:(n_cols-1))
  colnames(X) = covariate_names
  colnames(dat) = c(covariate_names, "count")
  
  # Join the binary table with the observed counts
  result = left_join(X, dat, by=covariate_names)
  
  # Set NA counts to zero
  result[is.na(result[,"count"]), "count"] = 0
  
  return(result)
}
