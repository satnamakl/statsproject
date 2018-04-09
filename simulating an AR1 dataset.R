graphics.off()
rm(list=ls())
gc()
cat("\014")


################################## PARAMETERS #################################

# Number of values
N = 100

# Proportion of the previous value remembered
alpha = 0.90

# The Jump, variance
B = 3
  
# Mean
mu = 150

##############################################################################
ar1 = mu + arima.sim(model = list(ar=alpha), n = N, sd = sqrt(B))


main = paste("AR1 simulated dataset\nmu=", mu, ", alpha=", alpha, ", Beta=", B,       sep = "")
plot(ar1, main = main, type = "o", pch = 16)



## Data observed with noise
samp_index = sample(x = 1:N, size = floor(length(ar1) * 0.5), replace = FALSE)
samp = ar1[samp_index]



# Data observed with noise
data_observed_noise = samp + rnorm(n = length(samp), mean = 0, sd = 1)

points(x = samp_index, y = data_observed_noise, pch = 16, col = "red")

######################### Note to self: ^^DON'T TOUCH THE ABOVE [good demo & and works]^^ ############################
 














######################################### Trying changable Betas (for fun) ###################################
graphics.off()
rm(list=ls())
gc()
cat("\014")



N = 1000
alpha = 0.90
mu = 150
# B = seq(1,1000, length.out = 300)
B = seq(300,1000, length.out = 200)



for (i in B){

ar1_data = mu + arima.sim(model = list(ar=alpha), n = N, sd = sqrt(i))
main = paste("AR1 simulated dataset\nmu=", mu, ", alpha=", alpha, ", Beta=", round(i, 3), sep = "")
plot(ar1_data, main = main, ylim = c(-300+mu, 300+mu), col = sample(rainbow(15), 1), lwd = 4)


## Data observed with noise
samp_index = sample(x = 1:N, size = floor(length(ar1_data) * 0.10), replace = FALSE)
samp = ar1_data[samp_index]


data_observed_noise = samp + rnorm(n = length(samp), mean = 0, sd = 5)
points(x = samp_index, y = data_observed_noise, pch = 16, cex = 0.5)



Sys.sleep(2) 

}











################################## PARAMETERS #################################
N = 1000
alpha = 0.90
B = 3
mu = 150

##############################################################################
ar1 = mu + arima.sim(model = list(ar=alpha), n = N, sd = sqrt(B))


main = paste("AR1 simulated dataset\nmu=", mu, ", alpha=", alpha, ", Beta=", B, sep = "")
plot(ar1, main = main, pch = 16, lwd = 2, type = "o")



# Data observed with noise
data_observed_noise = ar1 + rnorm(n = length(ar1), mean = 0, sd = 0)

points(data_observed_noise, pch = 16, col = "red")







