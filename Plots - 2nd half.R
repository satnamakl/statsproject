# Clears Everything
source('H:/2Masters Project/Meeting 14/ClearHistory.R')
clearhistory()
graphics.off()
rm(list=ls())
gc()
cat("\014")

load("H:/2Masters Project/Meeting 14/AR(1)_N=500, n=300 plus noise data/Files for batch submission/keep1500000_image_MT.RData")


png(filename = "H:/2Masters Project/0 knitR Project/150000abc.png", width = 7.5, height = 10, res = 400, units = "in")
par(mfrow = c(3,1))

# Check posterior for convergence
op <- par(mar = c(5.1, 4.3, 4.1, 2.1))

yl = expression(paste(sigma^2, " (verticle lengthscale)"))
plot(keep[, 1], type = "l", ylab = yl, xlab = "MCMC simulations", main = "N = 1,500,000 Iterations, recorded at every 10th position", col = "dodgerblue4")
abline(h = mean(keep[, 1]), col = "green2", lwd = 2)

plot(keep[, 2], type = "l", ylab = "l (horizontal lengthscale)", xlab = "MCMC simulations", col = "dodgerblue4")
abline(h = mean(keep[, 2]), col = "green2", lwd = 2)

yl3 = expression(paste(nu, " (power of cov function)"))
plot(keep[, 3], type = "l", ylab = yl3, xlab = "MCMC simulations", col = "dodgerblue4")
abline(h = mean(keep[, 3]), col = "green2", lwd = 2)

graphics.off()






png(filename = "H:/2Masters Project/0 knitR Project/threeMusketeers.png", width = 7.5, height = 10, res = 400, units = "in")
par(mfrow = c(3,1))
# At any given iteration (excluding the burn-in, which we don't have anyway), could be the true estimate of our hyperparameters.

op <- par(mar = c(5.1, 4.3, 4.1, 2.1))
# Example 1
val <- 35695
yl = expression(paste(sigma^2, " (verticle lengthscale)"))
plot(keep[, 1], type = "l", ylab = yl, xlab = "MCMC Simulations", col = "dodgerblue4")
segments(x0 = -10000, x1 = val, y0 = keep[, 1][val], y1 = keep[, 1][val], col = "#00e532", lty = 2, lwd = 2)
points(x = val, y = keep[, 1][val], pch = 16, col = "#00e532")
text(x = val+2500, y = 10, labels = paste("sigma^2 = ", format(keep[, 1][val], digits = 3), " [i = ", val, "]", sep = ""), col = "#00cc2c", font = 2)


# 2
plot(keep[, 2], type = "l", ylab = "l (horizontal lengthscale)", xlab = "MCMC Simulations", col = "dodgerblue4")
segments(x0 = -10000, x1 = val, y0 = keep[, 2][val], y1 = keep[, 2][val], col = "#00e532", lty = 2, lwd = 2)
points(x = val, y = keep[, 2][val], pch = 16, col = "#00e532")
text(x = val, y = 50, labels = paste("l = ", format(keep[, 2][val], digits = 3), " [i = ", val, "]", sep = ""), col = "#00cc2c", font = 2)


# 3
yl3 = expression(paste(nu, " (power of cov function)"))
plot(keep[, 3], type = "l", ylab = yl3, xlab = "MCMC Simulations", col = "dodgerblue4")
segments(x0 = -10000, x1 = val, y0 = keep[, 3][val], y1 = keep[, 3][val], col = "#00e532", lty = 2, lwd = 2)
points(x = val, y = keep[, 3][val], pch = 16, col = "#00e532")
text(x = val-10000, y = 1.22, labels = paste("nu = ", format(keep[, 3][val], digits = 3), " [i = ", val, "]", sep = ""), col = "#00cc2c", font = 2)
graphics.off()




# Example 2
png(filename = "H:/2Masters Project/0 knitR Project/threeMusketeers2.png", width = 7.5, height = 10, res = 400, units = "in")
par(mfrow = c(3,1))

# 1
val <- 140615
plot(keep[, 1], type = "l", ylab = "Sigma")
segments(x0 = -10000, x1 = val, y0 = keep[, 1][val], y1 = keep[, 1][val], col = "#00e532", lty = 2, lwd = 2)
points(x = val, y = keep[, 1][val], pch = 16, col = "#00e532")
text(x = val-7500, y = 10, labels = paste("Sigma = ", format(keep[, 1][val], digits = 3), " [i = ", val, "]", sep = ""), col = "#00cc2c", font = 2)


# 2
plot(keep[, 2], type = "l", ylab = "l")
segments(x0 = -10000, x1 = val, y0 = keep[, 2][val], y1 = keep[, 2][val], col = "#00e532", lty = 2, lwd = 2)
points(x = val, y = keep[, 2][val], pch = 16, col = "#00e532")
text(x = val-7500, y = 50, labels = paste("Sigma = ", format(keep[, 2][val], digits = 3), " [i = ", val, "]", sep = ""), col = "#00cc2c", font = 2)


# 3
plot(keep[, 3], type = "l", ylab = "nu")
segments(x0 = -10000, x1 = val, y0 = keep[, 3][val], y1 = keep[, 3][val], col = "#00e532", lty = 2, lwd = 2)
points(x = val, y = keep[, 3][val], pch = 16, col = "#00e532")
text(x = val-7500, y = 1.22, labels = paste("Sigma = ", format(keep[, 3][val], digits = 3), "[i = ", val, "]", sep = ""), col = "#00cc2c", font = 2)
graphics.off()

#


load("H:/2Masters Project/Meeting 14/AR(1)_N=500, n=300 plus noise data/Files for batch submission/keep1500000_image_MT.RData")
library(xtable)
df1 <- data.frame(".", ".", ".")
names(df1) <- c("X1", "X2", "X3")
test <- rbind(data.frame(head(keep)), df1, df1, df1, data.frame(tail(keep)))

colnames(test) <- c("sigma", "l", "nu")
xtable(test)

#





### # A Simple Example
png(filename = "H:/2Masters Project/0 knitR Project/asimpleexample.png", width = 7.5, height = 10, res = 600, units = "in")

par(mfrow = c(2,1))
load("H:/2Masters Project/Meeting 8/final_myMultiple10000.rda")
final8 <- final; rm(final)
smoothScatter(final8$H ~ final8$pvalue, ylab = "Entropy, H", xlab = "P-value",
              main = "N = 10,000 datasets",
              pch = 19, cex = 0.5, nrpoints = nrow(final8), col = "#78b8e2")


smoothScatter(final8$pvalue1, final8$p0, ylab = "Posterior Probability of H0", xlab = "P-value",
              pch = 19, cex = 0.5, nrpoints = nrow(final8), col = "#78b8e2")

graphics.off()
#^^^ SUNDAY before the dissertation due date







library(MASS)
################################## PARAMETERS #################################
# Number of values
N = 500
alpha = 0.90
B = 3
mu = 0

##############################################################################
set.seed(2); ar1 = mu + arima.sim(model = list(ar=alpha), n = N, sd = sqrt(B))

## Data observed with noise
set.seed(2);samp_index = sort(sample(x = 1:N, size = floor(length(ar1) * 0.75), replace = FALSE))
samp = ar1[samp_index]

# Data observed with noise
set.seed(3); data_observed_noise = samp  + rnorm(n = length(samp), mean = 0, sd = 1) #WITH NOISE

# Plotting
main = paste("AR1 Simulated Dataset\nmu=", mu, ", alpha=", alpha, ", beta=", B, ", N=", N, ",", sep = "")
plot(ar1, main = main, type = "o", pch = 16, col = "#9ccaf8", lwd = 2, ylab = "AR(1)", cex = 0.7)
title(paste("\n", paste(rep(" ", nchar(main)+19), collapse = ""), "n=", length(samp_index), sep = ""), col.main = "deeppink2")

points(x = samp_index, y = data_observed_noise, pch = 16, col = "deeppink2", cex = 0.85)





# THE DATA
obs <- data.frame(x = samp_index, y = samp)

# This may or may not make a difference, but lets order it
(obs <- obs[order(obs$x), ])


# FUNCTIONS
set.seed(2)
x_predict <- attributes(ar1)$tsp[1:2][1]:attributes(ar1)$tsp[1:2][2]
(sigma = mean(keep[, 1]))
(l = mean(keep[, 2]))
(nu = mean(keep[, 3]))

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


no_mvrSample <- 4
values <- mvrnorm(4, Ef, Cf)

dat_original <- data.frame(x = x_predict, t(values))
dat <- melt(dat_original, id = "x")



ymins = rep(Ef - 2*sqrt(diag(Cf)), 4)
ymaxs = rep(Ef + 2*sqrt(diag(Cf)), 4)


ggplot(dat, aes(x = x, y = value)) +
  geom_ribbon(aes(ymin = ymins, ymax = ymaxs), fill = "grey85") + 
  geom_line(aes(color=variable)) +
  geom_point(data = obs, aes(x = x, y = y)) +
  geom_line(data = NULL, aes(x = rep(x_predict, 4), y = rep(Ef, 4)), size = 1) + #MEAN
  scale_y_continuous(name = "Output, f(x)") +
  xlab("Time")


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



# 5 - And finally, Only looking at the average of all the multivriate samples
# COMPARISONS 
plot(ar1, type = "l", col = "#00CD66", xlab = "Time", ylab = "ar1",
     main = "Average of the 4 samples (black),
     and the original AR(1) curve (green)")















