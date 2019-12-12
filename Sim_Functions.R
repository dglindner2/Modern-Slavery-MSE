# Functions for George's Tab

library(dplyr)
library(ggplot2)
library(cowplot)
library(kableExtra)
library(dga)
library(MCMCpack)
library(Rcapture)
library(LCMCR)
library(tidyverse)
library(UpSetR)
library(resample)

MSEfit <- function(zdat = UKdat,
                   mainonly = F,
                   pthresh = 0.05,
                   returnfit
                   = T){
  require(Rcapture)
  # if returnfit = T then return the fitted model in the form provided by closedpCI.t
  #   otherwise return 95% and 80% confidence intervals and the point estimate
  #
  # if mainonly= F carry out the stepwise model fitting procedure as set out in Bales,
  # Hesketh and Silverman, using pthresh as the p-value that new effects have to
  # achieve before being included.
  # if mainonly= T then fit main effects only
  m = dim(zdat)[2] - 1
  # create and fit a formula with just the main effects
  fmain = "~."
  mXform = as.formula(fmain)
  zfit = closedpCI.t(zdat, dfreq = T, mX = mXform)
  if (mainonly)
    return(MSEretconf(zdat, zfit, mXform, returnfit))
  #
  aic0 = zfit$fit$aic
  # set up a list of interactions for the fitting of interactive models
  # let ints be the matrix of all possible interactions
  ints = NULL
  for (i in (1:(m - 1)))
    for (j in ((i + 1):m)) {
      ints = cbind(ints, c(i, j))
    }
  nints = m * (m - 1) / 2
  intsincluded = rep(F, nints)
  # add interactions one at a time trying each possible interaction not yet included
  for (jcycle in (1:nints)) {
    # add each possible new interaction and check the improvement in aic
    aicnew = rep(aic0, nints)
    for (j in (1:nints)) {
      if (!intsincluded[j]) {
        form1 = paste(fmain, "+c", ints[1, j], "*c",
                      ints[2, j], sep = "")
        mXf = as.formula(form1)
        zf = closedpCI.t(zdat, dfreq = T, mX = mXf)
        aicnew[j] = zf$fit$aic
      }
    }
    # now test if any interaction has made a difference
    if (min(aicnew) >= aic0)
      return(MSEretconf(zdat, zfit, mXform, returnfit))
    aic0 = min(aicnew)
    jintmax = min((1:nints)[aicnew == aic0])
    fmain1 = paste(fmain, "+c", ints[1, jintmax], "*c",
                   ints[2, jintmax], sep = "")
    mXf = as.formula(fmain1)
    zf1 = closedpCI.t(zdat, dfreq = T, mX = mXf)
    pval = rev(summary(zf1$fit)$coefficients[, 4])[1]
    if (pval > pthresh)
      return(MSEretconf(zdat, zfit, mXform, returnfit))
    zfit = zf1
    mXform = mXf
    fmain = fmain1
    intsincluded[jintmax] = T
  }
  return(MSEretconf(zdat, zfit, mXform, returnfit))
}

resample <- function(x, ...) x[sample.int(length(x), ...)]


MSEretconf <-
  function (zdat, zfit, mX, retfit)
  {
    # choose whether to return confidence levels or the full fit
    if (retfit)
      return(zfit)
    # construct vector of estimate and confidence levels
    xx = rep(NA, 5)
    names(xx) = c("2.5%", "10%", "point estimate", "90%", "97.5%")
    xx[c(1, 3, 5)] = as.numeric(zfit$CI)[c(2, 1, 3)]
    zfit1 = closedpCI.t(zdat,
                        dfreq = T,
                        mX = mX,
                        alpha = 0.2)
    xx[c(2, 4)] = as.numeric(zfit1$CI)[c(2, 3)]
    return(xx)
  }

generate_incl_prob <- function(pop, y_0, y_n, shape){
  x <- seq(0,1,1/(pop-1))
  x_n <- 1
  y <- (y_n - y_0)/(x_n^2) * x^shape + y_0
  df <- data.frame(person = x, prob = y)
  
  return(df)
}


plot_MSE_counts <- function(data, ...) {
  n = ncol(data) - 1
  dat = uncount(data[, 1:n], data$count)
  upset(dat, nsets=n, ...)
}


MSE_hist <- function(data, pop, nsims){
  require(ggplot2)
  ggplot(data, mapping = aes(x = est)) + 
    geom_histogram(bins = 22, color = "black", fill = "#8dd3c7") +
    labs(x = "MSE Estimate of Population", y = "Frequency", 
         caption = paste("Simulation is based on", nsims, "replications"), title = "") +
    theme_bw(base_size = 10) +
    geom_vline(aes(xintercept = pop), color = 'red', linetype = 2, size = 1.01)
}

inclusion <- function(pop, y_0, y_n, shape){
  
  require(ggplot2)
  require(dplyr)
  
  df <- generate_incl_prob(pop, y_0, y_n, shape)
  
  ggplot(data = df, aes(x=person,y=prob)) + 
    geom_line(color='lightblue', size = 1.1) + 
    geom_line(data = data.frame(x = 1, y = seq(0,y_n,y_n/10)), 
              aes(x = x, y = y), color = '#FF9933', linetype = 2) +
    scale_x_continuous('Individual', 
                       breaks = c(0,.5,1), 
                       labels = c('1',round(pop/2),pop), 
                       limits = c(0,1.2)) + 
    scale_y_continuous('Inclusion Probability (%)', 
                       breaks = c(0,y_0,y_n), 
                       labels = c("0%", paste(y_0*100,"%",sep=''), paste(y_n*100,"%",sep='')), 
                       limits = c(0,y_0)) +
    ggtitle('List Inclusion Probabilty for Individuals') + 
    theme(axis.line = element_line(colour = "black", 
                                   size = 1, linetype = "solid"),
          plot.title = element_text(hjust=0.5, 
                                    margin = margin(b = 20, r=0, l=0, t=20)),
          panel.background = element_blank()) 
  
}


list_df <- function(pop, y_0, y_n, shape, n_lists){
  # This function takes in inputs of population size (pop),
  #    Highest Inclusion Probability (y_0)
  #    Lowest Inclusion Probabiliy (y_n)
  #    The shape of the line (can be concave up or down, or line)
  #    The number of lists to generate
  
  # This function outputs whether individual i is on list j,
  # and how many lists individual i is captured by
  # and the true population size (used in later function)
  
  require(dplyr)
  
  df <- generate_incl_prob(pop, y_0, y_n, shape)
  
  for (i in 1:n_lists){
    df[,paste("list",i,sep='')] <- rbinom(pop, 1, df$prob)
  }
  
  df <- df %>%
    dplyr::select(-c(person,prob))
  
  df$count <- rowSums(df)
  
  return(df)
}


refer_remove <- function(pop, y_0, y_n, shape, n_lists, refer, remove){
  data <- list_df(pop, y_0, y_n, shape, n_lists)
  p <- ncol(data)
  data <- data[rowSums(data[,-p]) != 0, ]
  
  if(refer == 0 & remove == 0){
    return(data)
  } else {
    for(i in 1:nrow(data)){
      beta <- 1 - (1 - remove)^data[i,p]
      alpha <- 1 - (1 - refer)^data[i,p]
      
      r_beta <- rbinom(1, 1, beta)
      r_alpha <- rbinom(1, 1, alpha)
      
      if(r_beta == 1){
        col <- (1:(p-1))[data[i,-p] == 1]
        choose <- resample(col, 1)
        drop <- (1:(p-1)) != choose
        data[i, drop] = 0
      }
      
      if(r_alpha == 1){
        choose <- sample(p-1, 1)
        data[i,choose]=1
      }
      data[i,p] <- sum(data[i,1:(p-1)])
    }
    return(data)
  }
}

list_intersection <- function(pop, y_0, y_n, shape, n_lists, refer, remove){
  # This function takes in data generated from the function 'refer_remove'
  # It outputs every combination of lists and the number of individuals captured
  # on each on these chosen lists
  
  require(dplyr)
  data <- refer_remove(pop, y_0, y_n, shape, n_lists, refer, remove)
  p <- ncol(data) - 1
  df <- data %>% group_by_at(paste0("list", 1:p)) %>% count() 
  df <- df %>% arrange(desc(n))
  return(df)
}

MSEFixed_plot <- function(pop, y_0, y_n, shape, n_lists, refer, remove, nsims){
  
  EST <- data.frame(est = replicate(nsims, expr = {
    df <- list_intersection(pop, y_0, y_n, shape, n_lists, refer, remove)
    closedpCI.t(df, dfreq = T, mX = ~ 1 + .)$CI[1]}))
  
  MSE_hist(EST, pop, nsims)
  
}

MSEfit_plot <- function(pop, y_0, y_n, shape, n_lists, refer, remove, nsims){
  
  EST <- data.frame(est = replicate(nsims, expr = {
    df <- list_intersection(pop, y_0, y_n, shape, n_lists, refer, remove)
    closedpCI.t(df, dfreq = T, mX = ~ 1 + .)$CI[1]}
  ))
  
  MSE_hist(EST, pop, nsims)
  
}

