setwd("H:/2Masters Project/Meeting 8")

model = "model
{
  # Priors for the means
  # First mean
  mu1 ~ dnorm(0, 1/1000^2)
  
  # Prior for difference, mu2 - mu1
  u ~ dunif(-1, 1)
  
  # Length scale of exponential prior given that difference is nonzero
  L <- 5
  size_of_difference <- step(u)*(-L*log(1 - u))
  
  # To make the difference positive or negative
  C ~ dbin(0.5, 1)
  difference <- (2*C - 1)*size_of_difference
  
  # Define mu2 in terms of mu1 and the difference
  mu2 <- mu1 + difference
  
  
  # Log-uniform prior for the s.d.
  log_sigma ~ dunif(-10, 10)
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

# The data (use NA for no data)
# source("widgets.R")
examdata <- read.csv("ttestdata.csv", header = TRUE)[1:100, ]
data = structure(list(y1 = examdata$Aus, y2 = examdata$Nz, N1 = nrow(examdata), 
                 N2 = nrow(examdata)), .Names = c("y1", "y2", "N1", "N2"))


data_gen <- function(){
  
  AusMean <- sample(x = 35:65, size = 1)
  Aus <- rnorm(n = sample(x = 50:500, size = 1), mean = AusMean, sd = 20)
  Aus <- ifelse(Aus < 0, 0, ifelse(Aus > 100, 100, round(Aus, digits = 1)))
  # boxplot(Aus, main = paste("Sample Size = ", length(Aus)))
  
  NzMean <- sample(x = 35:65, size = 1)
  Nz <- rnorm(n = sample(x = 50:500, size = 1), mean = NzMean, sd = 20)
  Nz <- ifelse(Nz < 0, 0, ifelse(Nz > 100, 100, round(Nz, digits = 1)))
  # boxplot(Nz, main = paste("Sample Size = ", length(Nz)))
  
  
  # Two side-by-side boxplots
  exam <- data.frame(score = c(Aus, Nz), country = c(rep("Aus", length(Aus)), rep("NZ", length(Nz))))
  pval = t.test(score ~ country, data = exam)
  # 
  # boxplot(score ~ country, data = exam, ylab = "Exam Score",
  #         main = paste("Mu=", round(mean(Aus), 1), " sd=", round(sd(Aus), 1), " n=", length(Aus),
  #                      "\n                          Mu=", round(mean(Nz), 1), " sd=", round(sd(Nz), 1), " n=", length(Nz),
  #                      "\n Pvalue = ", ifelse(pval$p.value < 0.001, formatC(pval$p.value, format = "e", digits = 3), format(pval$p.value, digits = 3)),
  #                      sep = ""))
  
  d = list(y1 = Aus, y2 = Nz, N1 = length(Aus), N2 = length(Nz), AusMean = mean(Aus), NzMean = mean(Nz), pval = pval$p.value)
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

# Random number seed
seed = 42


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


######################

par(mfrow = c(2,1))
plot(results$mu1, type ="l", xlab = "Aus", main = paste("Mean (Simulation) = ", round(mean(results$mu1), 2), "\n Mean (True) = ", round(data$AusMean, 2))); abline(h = mean(results$mu1), col = "turquoise2", lwd = 2)
plot(results$mu2, type ="l", xlab = "Nz", main = paste("Mean (Simulation) = ", round(mean(results$mu2), 2), "\n Mean (True) = ", round(data$NzMean, 2))); abline(h = mean(results$mu2), col = "turquoise2", lwd = 2)


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



