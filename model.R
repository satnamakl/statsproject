# Prior widths for each parameter (these help set scale for proposal)
widths = c(10, 1, 1)
N = 10000

# Functions for prior, likelihood, and proposal
log_prior = function(params)
{
  
    # Mean level
    mu <- dnorm(params[1], mean = 0, sd = 1/1000^2)
    
    
    # Timescale
    log_L = dunif(x = params[2], min = -10, max = 10)
    L <- exp(log_L)
    alpha <- exp(-1/L)
    
    
    # Standard deviation of innovations
    log_beta = dunif(x = params[3], min = -10, max = 10)
    beta <- exp(log_beta)
    
    return(sum(alpha, mu, beta))
  
}



# Might need to load data from a file here - cool, done!!!
setwd("H:/2Masters Project/Meeting 5_AR1/FINAL")
ar1 <- read.csv("ar1_10000dummysamples_v2.csv", header = TRUE)



# Log likelihood
log_likelihood = function(params) {
  
    sigma1 <- params[3]/sqrt(1 - params[2]^2)
    
    y = numeric(N)
    y[1] = 50
    
    for(i in 2:N) {
      y[i] = params[1] + params[2]*(y[i-1] - params[1]) + params[3]*rnorm(1)
    }
  
    lik = dnorm(ar1$yvalues, mean = y, sd = sigma1, log = TRUE)
    return(sum(lik))
}

  


# Proposal distribution
proposal = function(params){
  
    # Copy the parameters
    params2 = params

    # Which parameter to change?
    i = sample(1:length(params), 1)

    # Step size - Brendon's favourite magic
    step_size = widths[i]*10^(1.5 - 3*abs(rt(1, df=3))) #BB
    
    params2[i] = params2[i] + step_size * rnorm(1)
    return(params2)
}


















