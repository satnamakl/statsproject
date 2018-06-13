library(MASS)
library(reshape)
library(ggplot2)


graphics.off()
rm(list=ls())
gc()
cat("\014")

##################################### The 'Squared Exponential' Covariance Function #############################################
# The 'Squared Exponential' Covariance Function - Slide Xray
# 'The prior can be summarized by a covariance function'. Equation 45.30 - Chapter 45, pg 541 (David Mackay)

# http://www.carlboettiger.info/2012/10/17/basic-regression-in-gaussian-processes.html
# Squared Exponential, also called the radical basis - as the covariance function


# Vertical lengthscale
sigmaf = 1

# Horizontal lengthscale
l = 1

# Creating the matrix
x = 1:5
sigmaf^2 * (exp(-(1/(2*(l^2))) * as.matrix(dist(x, upper = T, diag = T)^2)))




################################################# Sun #################################################
### Another way - http://www.carlboettiger.info/2012/10/17/basic-regression-in-gaussian-processes.html 
set.seed(12345)
x_predict <- seq(-5,5,len=50)
l <- 1


## SQUARED EXPONENTIALS
SE <- function(Xi,Xj, l) {exp(-0.5 * (Xi - Xj) ^ 2 / l ^ 2)}
cov2 <- function(X, Y) {outer(X, Y, SE, l)}
COV <- cov2(x_predict, x_predict)


# ## ABSOLUTE VALUE
# SE <- function(Xi,Xj, l) {exp(-(abs(Xi - Xj)))}
# cov2 <- function(X, Y) {outer(X, Y, SE, l)}
# COV <- cov2(x_predict, x_predict)
#######################################################################################################


set.seed(12345)
mvrnorm_n <- 3 #Number of samples required_SS
values <- matrix(mvrnorm(n = mvrnorm_n, rep(0, length=length(x_predict)), COV), nrow = mvrnorm_n)


dat <- data.frame(x = x_predict, t(values))
dat <- melt(dat, id = "x")




fig2a <- ggplot(dat,aes(x=x,y=value)) +
  geom_rect(xmin=-Inf, xmax=Inf, ymin=-2, ymax=2, fill="grey80") +
  geom_line(aes(group=variable)) +   theme_bw() +
  scale_y_continuous(lim=c(-3,3), name="output, f(x)") +
  xlab("input, x")
fig2a



######################################### POSTERIOR DISTRIBUTION GIVEN SOME DATA #############################################
obs <- data.frame(x = c(-4, -3, -1, 0, 2),
                  y = c(-2, 0, 1, 2, -1))



## Taking the inverse of the Covariance Matrix
cov_xx_inv <- solve(cov2(obs$x, obs$x))

Ef <- cov2(x_predict, obs$x) %*% cov_xx_inv %*% obs$y

Cf <- cov2(x_predict, x_predict) - cov2(x_predict, obs$x)  %*% cov_xx_inv %*% cov2(obs$x, x_predict)


values <- mvrnorm(n = 3, mu = Ef, Sigma = Cf)

dat <- data.frame(x = x_predict, t(values))
dat <- melt(dat, id = "x")


ymins = rep(Ef-2*sqrt(diag(Cf)), 3)
ymaxs = rep(Ef+2*sqrt(diag(Cf)), 3)


ggplot(dat, aes(x = x, y = value)) +
  geom_ribbon(aes(ymin = ymins, ymax = ymaxs), fill = "grey85") + 
  geom_line(aes(color=variable)) +
  geom_point(data = obs, aes(x = x, y = y)) +
  geom_line(data = NULL, aes(x = rep(x_predict, 3), y = rep(Ef, 3)), size = 1) + #MEAN
  scale_y_continuous(lim = c(-3, 3), name = "Output, f(x)") +
  xlab("input, x")









# ### THIS WORKS TOO - VERY MANUAL THOUGH ###
# plot(y = dat$value, x = dat$x, type = "l")
# 
# lines(y = Ef-2*sqrt(diag(Cf)), x = dat$x[1:50])
# lines(y = Ef-2*sqrt(diag(Cf)), x = dat$x[1:50], col = "red", lwd = 3, lty = 6)
# 
# lines(y = Ef+2*sqrt(diag(Cf)), x = dat$x[1:50])
# lines(y = Ef+2*sqrt(diag(Cf)), x = dat$x[1:50], col = "#2E8B57", lwd = 3, lty = 6)
# 
# 
# # note, check the above, and see if the black mean line is infact the mean
# x1 = dat[1:50, ]
# x2 = dat[51:100, ]
# x3 = dat[101:150, ]
# 
# plot(x1$value ~ x1$x, type = "l", col = "orange")
# lines(x2$value ~ x2$x, type = "l", col = "#00CD66")
# lines(x3$value ~ x3$x, type = "l", col = "#4876FF")
# 
# total = data.frame(x1 = x1$value, x2 = x2$value, x3 = x3$value)
# mean1 <- apply(total, 1, mean)
# 
# lines(mean1 ~ x1$x, type = "l", col = "black", lwd = 2)








### BRING IN OUR AR1 ###
library(MASS)
library(reshape)
library(ggplot2)


graphics.off()
rm(list=ls())
gc()
cat("\014")




################################## PARAMETERS #################################
# Number of values
N = 500

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

# Plotting
main = paste("AR1 Simulated Dataset\nmu=", mu, ", alpha=", alpha, ", Beta=", B, ", N=", N, ",", sep = "")
plot(ar1, main = main, type = "o", pch = 16, col = "dodgerblue3", lwd = 2, ylab = "AR(1)")
title(paste("\n", paste(rep(" ", nchar(main)+17), collapse = ""), "n=", length(samp_index), sep = ""), col.main = "deeppink2")

p = 1
for (i in samp_index){
  segments(x0 = i, x1 = i, y0 = data_observed_noise[p], y1 = ar1[i], col = "deeppink2", lty =2)
  p <- p + 1
}

points(x = samp_index, y = data_observed_noise, pch = 16, col = "deeppink2")





# THE DATA
obs <- data.frame(x = samp_index, y = samp)

# This may or may not make a difference, but lets order it
(obs <- obs[order(obs$x), ])


# FUNCTIONS
set.seed(2)
x_predict <- attributes(ar1)$tsp[1:2][1]:attributes(ar1)$tsp[1:2][2]
l <- 1
nu <- 2
sigma <- 1

# 'Stationary' Covariance Function - Using absolutes insead of Squared distances
SE <- function(Xi, Xj, l) {sigma^2 * exp(-(abs(Xi - Xj)/l)^nu)}
cov2 <- function(X, Y) {outer(X, Y, SE, l)}

## Taking the inverse of the Covariance Matrix
cov_xx_inv <- solve(cov2(obs$x, obs$x))

# Mean Vector
Ef <- cov2(x_predict, obs$x) %*% cov_xx_inv %*% obs$y

# Covariance Matrix
Cf <- cov2(x_predict, x_predict) - cov2(x_predict, obs$x)  %*% cov_xx_inv %*% cov2(obs$x, x_predict)

# Replacing the negatives with 0 in the diagonals of the Covariance Matrix - Cannot be zero
diag(Cf)[diag(Cf) < 0] <- 0


no_mvrSample <- 5
values <- mvrnorm(no_mvrSample, Ef, Cf)

dat_original <- data.frame(x = x_predict, t(values))
dat <- melt(dat_original, id = "x")



ymins = rep(Ef - 2*sqrt(diag(Cf)), no_mvrSample)
ymaxs = rep(Ef + 2*sqrt(diag(Cf)), no_mvrSample)

# png(filename = "gaussian_ggplot_375.png", width = 7.5, height = 5, res = 400, units = "in")
ggplot(dat, aes(x = x, y = value)) +
  geom_ribbon(aes(ymin = ymins, ymax = ymaxs), fill = "grey85") + 
  geom_line(aes(color=variable)) +
  geom_point(data = obs, aes(x = x, y = y)) +
  geom_line(data = NULL, aes(x = rep(x_predict, no_mvrSample), y = rep(Ef, no_mvrSample)), size = 1) + #MEAN
  scale_y_continuous(name = "Output, f(x)") +
  xlab("Time") + 
  ggtitle("Prediction by drawing 5 MVN samples from a Gaussian") +
  theme(plot.title = element_text(hjust = 0.5))
# graphics.off() 

############ PLOTS #########################


# 1
# Mean Vector & the observed points
plot(Ef, type = "l", main = "Mean Vector and the observed points", ylab = "Mean") # Mean Vector
points(obs$y ~ obs$x, pch = 16, col = "red") # Observed Points




# 2
# Mean Vector & the observed points - with samples from mvrnorm
plot(Ef, type = "l") # Mean Vector
points(obs$y ~ obs$x, pch = 16, col = "red") # Observed Points

mycols_ColorBrewer <- c("#1f78b4", "#33a02c", "#ff7f00", "#6a3d9a",  "#b15928")
set.seed(1); samp <- sample(x = mycols_ColorBrewer, size = 3, replace = FALSE)

mycols_rainbow <- rainbow(3)


for (i in 1:3){
lines(dat_original[, i+1], col = mycols_rainbow[i])
}





# 3 - ONE OF THE IMPORTANT ONES
# Only the observed points - with samples from mvrnorm
plot(obs$y ~ obs$x, pch = 16, col = "red", xlab = "Time", ylab = "ar1") # Observed Points

mycols_rainbow <- rainbow(3)
for (i in 1:3){
  lines(dat_original[, i+1], col = mycols_rainbow[i], lty = 6)
}








# 4 - ANOTHER IMPORTANT ONE
# Only the observed points - with samples from mvrnorm
plot(obs$y ~ obs$x, pch = 16, col = "red", xlab = "Time", ylab = "ar1") # Observed Points

mycols_rainbow <- rainbow(3)
for (i in 1:3){
  lines(dat_original[, i+1], col = mycols_rainbow[i], lty = 6)
}
lines(ar1, type = "l", col = "#00CD66", lwd = 2)



# png(filename = "observed_vs_mvnAverage.png", width = 7.5, height = 5, res = 400, units = "in")
# 5 - And finally, Only looking at the average of all the multivriate samples
# COMPARISONS 
plot(y = data_observed_noise, x = samp_index, pch = 16, col = "deeppink2", main = "Observed points vs. the average of the 5 MVN samples", ylab = "Output, f(x)", xlab = "Time")


# The mean of our multivariate samples
dat_original$mean_Xn <- apply(dat_original[2:ncol(dat_original)], 1, mean)
lines(dat_original$mean_Xn, lwd = 2, col = "#23397d")
# Aside
# lines(ar1, col = "#9ccaf8")
# graphics.off




