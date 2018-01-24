# Prior widths for each parameter (these help set scale for proposal)
widths = c(1000, 20, 20)
N = 10000

# Functions for prior, likelihood, and proposal
log_prior = function(params)
{
  
    # Mean level
    logp_mu <- dnorm(params[1], mean = 0, sd = 1000, log=TRUE)
    
    
    # Timescale
    logp_log_L = dunif(x = params[2], min = -10, max = 10, log=TRUE)
#    L <- exp(log_L)
#    alpha <- exp(-1/L)
    
    
    # Standard deviation of innovations
    logp_log_beta = dunif(x = params[3], min = -10, max = 10, log=TRUE)
#    beta <- exp(log_beta)
    
    return(logp_mu + logp_log_L + logp_log_beta)
  
}



# Might need to load data from a file here - cool, done!!!
#setwd("H:/2Masters Project/Meeting 5_AR1/FINAL")
ar1_data <- read.csv("ar1_10000dummysamples_v2.csv", header = TRUE)[1:100,2]



# Log likelihood
log_likelihood = function(params) {

    mu = params[1]
    L = exp(params[2])
    alpha = exp(-1/L)
    beta = exp(params[3])
  
    sigma1 <- beta/sqrt(1 - alpha^2)

    # First term
    log_lik = dnorm(ar1_data[1], mu, sigma1, log=TRUE)

    # Other terms
    for(i in 2:length(ar1_data))
    {
        mean = mu + alpha*(ar1_data[i-1] - mu)
        sd = beta
        log_lik = log_lik + dnorm(ar1_data[i], mean, sd, log=TRUE)
    }

#    y = numeric(N)
#    y[1] = 50
#    
#    for(i in 2:N) {
#      y[i] = params[1] + params[2]*(y[i-1] - params[1]) + params[3]*rnorm(1)
#    }
  
#    lik = dnorm(ar1$yvalues, mean = y, sd = sigma1, log = TRUE)
    return(log_lik)
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


















