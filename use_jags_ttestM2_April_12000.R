# graphics.off()
# rm(list=ls())
# gc()
# cat("\014")

# Random number seed
seed = 42

p = proc.time()

# Variables to monitor
p0 <- numeric()
p1 <- numeric()
H <- numeric()
pvalue <- numeric() 
post_mu2_gt_m1 <- numeric()
sample_Mean1 <- numeric()
sample_Mean2 <- numeric()
sample_size <- numeric()
you <- numeric()
sizeofdiff <- numeric()
std_dev <- numeric()


N = 12000

for (i in 1:N) {

model = "model
{
  # Priors for the means
  # First mean
  mu1 ~ dnorm(0, 1/1000^2)
  
  # Prior for difference, mu2 - mu1
  u ~ dunif(-1, 1)
  
  # Length scale of exponential prior given that difference is nonzero
  L <- 100
  size_of_difference <- step(u)*(-L*log(1 - u))
  
  # To make the difference positive or negative
  C ~ dbin(0.5, 1)
  difference <- (2*C - 1)*size_of_difference
  
  # Define mu2 in terms of mu1 and the difference
  mu2 <- mu1 + difference
  
  
  # Log-uniform prior for the s.d.
  log_sigma ~ dnorm(log(L), 1)
  sigma <- exp(log_sigma)
  
  # Sampling distribution/likelihood
  for(i in 1:N1)
  {
    y1[i] ~ dnorm(mu1, 1/sigma^2)
  }
  for(i in 1:N2)
  {
    y2[i] ~ dnorm(mu2, 1/sigma^2)
  }
}
"


data_gen <- function(){
  
  sample_size <- sample(c(100, 200, 300, 500, 1000, 5000), size = 1)
  
  # First mean
  mu1 = rnorm(1, 0, 1000)
  
  # Prior for difference, mu2 - mu1
  u = runif(1, -1, 1)
  
  # Length scale of exponential prior given that difference is nonzero
  L = 100
  size_of_difference = ifelse(u < 0, 0, -L*log(1 - u))
  
  # To make the difference positive or negative
  C = sample(0:1, 1)
  difference = (2*C - 1)*size_of_difference
    
  # Define mu2 in terms of mu1 and the difference
  mu2 = mu1 + difference
  
  # Log-uniform prior for the s.d.
  log_sigma = rnorm(1, log(L), 1)
  sigma = exp(log_sigma)
  
  # Now generate the data
  y1 = rnorm(sample_size, mu1, sigma)
  y2 = rnorm(sample_size, mu2, sigma)
  
  pval = t.test(y1, y2)
  d = list(y1 = y1, 
           y2 = y2, 
           N1 = length(y1), 
           N2 = length(y1),
           sampleMean1 = mean(y1), 
           sampleMean2 = mean(y2),
           std_dev = sigma,
           pval = pval$p.value,
           you = u,
           sizeofdiff = size_of_difference,
           sample_Mean1 = mu1,
           sample_Mean2 = mu2,
           sample_size = sample_size)
  return(d)
}

data = data_gen()

# Variables to monitor
variable_names = c("mu1", "mu2")

# How many burn-in steps?
burn_in = 1000

# How many proper steps?
steps = 10000

# Thinning?
thin = 10



# NO NEED TO EDIT PAST HERE!!!
# Just run it all and use the results list.

suppressMessages(library('rjags'))

# Write model out to file
fileConn=file("model.temp")
writeLines(model, fileConn)
close(fileConn)

if(all(is.na(data)))
{
	m = jags.model(file="model.temp", inits=list(.RNG.seed=seed, .RNG.name="base::Mersenne-Twister"))
} else
{
	m = jags.model(file="model.temp", data=data, inits=list(.RNG.seed=seed, .RNG.name="base::Mersenne-Twister"))
}
update(m, burn_in)
draw = jags.samples(m, steps, thin=thin, variable.names = variable_names)
# Convert to a list
make_list <- function(draw)
{
	results = list()
	for(name in names(draw))
	{
		# Extract "chain 1"
		results[[name]] = as.array(draw[[name]][,,1])
		
		# Transpose 2D arrays
		if(length(dim(results[[name]])) == 2)
			results[[name]] = t(results[[name]])
	}
	return(results)
}
results = make_list(draw)


p0[i] = mean(results$mu2 == results$mu1)
pvalue[i] = data$pval

p1[i] = 1-p0[i]

cat(c(p0[i], pvalue[i]))
cat("\n")

# Calculating Entropy
H_entropy = (-p0[i] * log2(p0[i])) - (p1[i] * log2(p1[i]))
H[i] = ifelse(is.nan(H_entropy), 0, H_entropy)



# last six
you[i] <- data$you
sizeofdiff[i] <- data$sizeofdiff

sample_Mean1[i] = data$sample_Mean1
sample_Mean2[i] = data$sample_Mean2
sample_size[i] = data$sample_size
std_dev[i] = data$std_dev


# Posterior probability that Nz exam score are better than Aus. Proportion of the points over that H0 line
post_mu2_gt_m1[i] <- mean(results$mu2 >= results$mu1)

}


final <- data.frame(u_priorForDifference = you, 
                    sizeofdiff = sizeofdiff, 
                    sample_Mean1 = sample_Mean1,
                    sample_Mean2 = sample_Mean2,
                    std_dev = std_dev,
                    sample_size = sample_size,
                    p0 = p0, 
                    p1 = p1, 
                    post_mu2_gt_m1 = post_mu2_gt_m1, 
                    pvalue = pvalue, 
                    H = H)
proc.time() - p






######################  ALL OF THIS BELOW WORKS FOR N=1 RUN ################################

# par(mfrow = c(2,1))
# plot(results$mu1, type ="l", xlab = "Aus", main = paste("Mean (Simulation) = ", round(mean(results$mu1), 2), "\n Sample mean = ", round(data$sampleMean1, 2))); abline(h = mean(results$mu1), col = "turquoise2", lwd = 2)
# plot(results$mu2, type ="l", xlab = "Nz", main = paste("Mean (Simulation) = ", round(mean(results$mu2), 2), "\n Sample mean = ", round(data$sampleMean2, 2))); abline(h = mean(results$mu2), col = "turquoise2", lwd = 2)


c(Nz = mean(results$mu2), Aus = mean(results$mu1))



# Posterior probability of Ho
(p0 = mean(results$mu2 == results$mu1))

# Posterior probability of H1
(p1 = 1-p0)



# Joint Posterior Distribution
# Hence, the posterior probability of Ho = Proportion of the points that lie on the diagonal line.
graphics.off();plot(results$mu1, results$mu2, main = paste("Posterior probability of H0 (Mu1 = Mu2) =", p0))

# Posterior probability that Nz exam score are better than Aus. Proportion of the points over that H0 line
mean(results$mu2 >= results$mu1)


# Classical P-value
pvalue = data$pval


# Calculating Entropy
H = (-p0 * log(p0)) - (p1 * log(p1))
H = ifelse(is.nan(H), 0, H)









