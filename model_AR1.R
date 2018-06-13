# Prior widths for each parameter (these help set scale for proposal)
widths = c(100, 2, 20)

# Functions for prior, likelihood, and proposal
log_prior = function(params)
{
  
  # Mean level
  logp_mu <- dnorm(params[1], mean = 0, sd = 1000, log=TRUE)

  
  # Timescale
  logp_log_L = dunif(x = params[2], min = -10, max = 10, log=TRUE)
  
  
  # Standard deviation of innovations
  logp_log_beta = dunif(x = params[3], min = -10, max = 10, log=TRUE)
  
  
  return(logp_mu + logp_log_L + logp_log_beta)
  
}


# GENERATing DATA
N = 500 #Sample, data points

alpha = 0.95 #Proportion of the previous value remembered

B = 3 #The Jump, variance

mu = 50 #Mean
##############################################################################
set.seed(3); ar1 = mu + arima.sim(model = list(ar=alpha), n = N, sd = sqrt(B))
ar1_data = as.numeric(ar1)


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


