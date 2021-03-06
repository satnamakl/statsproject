# #Clears Everything
# source('H:/2Masters Project/Meeting 14/ClearHistory.R')
# clearhistory()
# graphics.off()
# rm(list=ls())
# gc()
# cat("\014")

p = proc.time()

# An "industrial strength" Metropolis implementation
# for STATS 331

#setwd("H:/2Masters Project/Meeting 13/")

# Starting position in parameter space
params = c(sigma = 5.3, l = 9, nu = 1.03) # [Named] numeric vectors are allowed

# How many steps to take
steps = 1000000

# Thinning. Saving the state every 100th iteration
thin = 10

# Load functions that are model-specific
source("model.R")


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

# Progress Bar: line alpha

# Do Metropolis
for(i in 1:steps) {
    # Propose to move somewhere else
    
    
    params2 = proposal(params)

    # Measure how good it is
    logh2 = log_prior(params2)
    
    
    if(logh2 != -Inf)
    {
      logh2 = logh2 + log_likelihood(params2)
    }

      
  
    
    
    # Acceptance probability
    log_alpha = logh2 - logh
    
    if(log_alpha > 0)
    {
      log_alpha = 0
    }


    
    
    # Accept the proposal with probability alpha
    if(runif(1) < exp(log_alpha))
    {
      params = params2
      logh = logh2
      accepted = accepted + 1
    }
    
    
    # Store results
    if(i %% thin == 0){
        keep[i/thin, ] = params
        logl_keep[i/thin] = log_likelihood(params)
        cat("Done", i, "iterations.\n")
    }
    
}

cat("Acceptance rate =", accepted/steps, "\n")



(timeTaken <- proc.time() - p)




save.image("keep1500_image.RData")
write.table(keep, "keep1500.txt", quote = FALSE, sep = "|", row.names = FALSE)


  





