# Clears Everything
graphics.off()
rm(list=ls())
gc()
cat("\014")


# An "industrial strength" Metropolis implementation
# for STATS 331

setwd("H:/2Masters Project/Meeting 11")


# How many steps to take
steps = 10000

# Thinning. Saving the state every 100th iteration
thin = 10

# Load functions that are model-specific
source("model_BJBedits_Github.R")

# Starting position in parameter space
params = c(mu = 100, log_L = 0.9, log_beta = 2) # [Named] numeric vectors are allowed

# Check that it's okay
if(log_prior(params) == -Inf)
{
  cat("Your starting point has zero prior probability! Start somewhere else!")
  stopifnot(FALSE)
}

# Measure how good it is
(logh = log_prior(params) + log_likelihood(params))


# Set up 2D array for storage of results
keep = array(NA, dim = c(steps/thin, length(params)))

# Set up 1D array
logl_keep = array(NA, dim = steps/thin)

# Count number of accepted proposals
accepted = 0



# Do Metropolis
for(i in 1:steps) {
    # Propose to move somewhere else
    params2 = proposal(params)

    # Measure how good it is
    logh2 = log_prior(params2)
    
    ###### Original code - commented out below:########
    # if(logh2 != -Inf)
    #   logh2 = logh2 + log_likelihood(params2)
    logh2 = ifelse(logh2 != -Inf, logh2 + log_likelihood(params2), logh2)
      
    
    
    
    
    # Acceptance probability
    log_alpha = logh2 - logh
    
    ###### Original code - commented out below:########
    # if(log_alpha > 0)
    # {
    #   log_alpha = 0
    # }
    log_alpha = ifelse(log_alpha > 0, 0, log_alpha)

    
    
    # Accept the proposal with probability alpha
    ###### Original code - commented out below:########
    if(runif(1) < exp(log_alpha))
    {
      params = params2
      logh = logh2
      accepted = accepted + 1
    }
#    params = ifelse(runif(1) < exp(log_alpha), params2, params)
#    logh = ifelse(runif(1) < exp(log_alpha), logh2, logh)
#    accepted = ifelse(runif(1) < exp(log_alpha), accepted + 1, accepted)
    
    
    
    
    # Store results
    if(i %% thin == 0){
        keep[i/thin, ] = params
        logl_keep[i/thin] = log_likelihood(params)
        cat("Done", i, "iterations.\n")
    }
}
cat("Acceptance rate =", accepted/steps, "\n")





#############################################################################################################################################
## PLOTS ##

# mu, should be around 150
plot(keep[, 1], type = "l", ylab = "mu, Mean")



# alpha, should be around 0.97
plot(exp(-1/exp(keep[, 2])), type = "l", ylab = "alpha, Proportion Remembered")



# beta, should be around 3
plot(exp(keep[, 3]), type = "l", ylab = "beta, Jump")
mean(exp(keep[, 3]))









####################################################################### 10 April, Tue
plot(keep[, 1], type = "l", ylab = "mu, Mean", ylim = c(40, 60))


# keep100 <- read.csv("keep_N100.csv")
# lines(keep100$x, col = "red")


keep500 <- read.csv("keep_N500.csv")
lines(keep500$x, col = rainbow(5)[1], lwd = 2)


keep1000 <- read.csv("keep_N1000.csv")
lines(keep1000$x, col = rainbow(5)[3], lwd = 2)


keep5000 <- read.csv("keep_N5000.csv")
lines(keep5000$x, col = rainbow(5)[4], lwd = 2)


keep10000 <- read.csv("keep_N10000.csv")
lines(keep10000$x, col = rainbow(5)[5], lwd = 2)


# keep25000 <- read.csv("keep_N25000.csv")
# lines(keep1000$x, col = "blue")


