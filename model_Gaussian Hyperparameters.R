# Prior widths for each parameter (these help set scale for proposal)
widths = c(100, 100, 1)

# Functions for prior, likelihood, and proposal
log_prior = function(params)
{
  
    # sigma
    sigma <- dunif(params[1], min = 0, max = 100, log = TRUE)
    
    # l
    l = dunif(x = params[2], min = 0, max = 100, log = TRUE)

    # nu
    nu = dunif(x = params[3], min = 1, max = 2, log = TRUE)
    
    return(sigma + l + nu)
  
}



# Generating a short model, so we can use its' observations
################################## PARAMETERS #################################
# Number of values
N = 1500

# Proportion of the previous value remembered
alpha = 0.90

# The Jump, variance
B = 3

# Mean
mu = 0

##############################################################################
set.seed(2); ar1 = mu + arima.sim(model = list(ar=alpha), n = N, sd = sqrt(B))

## Data observed with noise
set.seed(2);samp_index = sort(sample(x = 1:N, size = floor(length(ar1) * 0.75), replace = FALSE))
samp = ar1[samp_index]

# Data observed with noise
set.seed(3); data_observed_noise = samp  + rnorm(n = length(samp), mean = 0, sd = 1) #WITH NOISE
#############################################################################
y = as.numeric(data_observed_noise)
x = samp_index

y_vector <- as.matrix(y)


# Log likelihood
log_likelihood = function(params){
  sigma = params[1]
  l = params[2]
  nu = params[3]
  
  ## Covariance Matrix
  ## ABSOLUTE VALUE - Stationary Covariance Function
  stat.cov <- function(X, Y) {outer(X, Y, function(Xi, Xj, l) {sigma^2 * exp(-(abs((Xi - Xj)/l))^nu)}, l)}
  C = stat.cov(x, x)
  
  cholC <- chol(C)
  
  log_lik = (-0.5*N*log(2*pi))    -    (0.5*2*sum(diag(log(cholC))))   -   (0.5*t(y_vector) %*% chol2inv(cholC) %*% y_vector)
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




















