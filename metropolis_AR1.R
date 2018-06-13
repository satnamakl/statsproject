# An "industrial strength" Metropolis implementation
# for STATS 331

# How many steps to take
steps = 100000

# Thinning. Saving the state every 100th iteration
thin = 100

# Load functions that are model-specific
source("model_AR1.R")

# Starting position in parameter space
params = c(mu = 90, log_L = 0.93, log_beta = 1) # [Named] numeric vectors are allowed

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
  #    params = ifelse(runif(1) < exp(log_alpha), params2, params)
  #    logh = ifelse(runif(1) < exp(log_alpha), logh2, logh)
  #    accepted = ifelse(runif(1) < exp(log_alpha), accepted + 1, accepted)
  
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






#############################################################################################################################################
## PLOTS ##
# par(mfrow = c(3,1))

# mu, should be around 150
yl = expression(paste(mu, " (mean)"))
plot(keep[, 1], type = "l", ylab = yl, xlab = "MCMC simulations", col = "dodgerblue4")

# alpha, should be around 0.97
yl2 = expression(paste(alpha, " (proportion remembered)"))
plot(exp(-1/exp(keep[, 2])), type = "l", ylab = yl2, xlab = "MCMC simulations", col = "dodgerblue4")

# beta, should be around 3
yl3 = expression(paste(beta, " (innovation/jump)"))
plot(exp(keep[10:1000, 3]), type = "l", ylab = yl3, xlab = "MCMC simulations", col = "dodgerblue4")
# graphics.off()







