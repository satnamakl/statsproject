set.seed(123)





# Make an example covariance matrix
(C = as.matrix(array(c(2,1,1,2), dim=c(2,2))))

# Determinant
cat(paste("log determinant =", log(det(C)), "\n"))

# This is called Cholesky decomposition - kinda the square root of a matrix
U = chol(C)

# This is another way of calculating the log of the determinant
# using U
cat(paste("log determinant =", 2*sum(diag(log(U))), "\n "))

# Generate a vector
y = rnorm(5)

# C^-1 * y - the bit in the exponent of a multivariate gaussian
result = solve(C) %*% y
print(result)

# This should be faster
result2 = chol2inv(U) %*% y
print(result)












########################### SATNAM ################################
############### Stationary Covariance Functions ###################

(x = 1:5)

## ABSOLUTE VALUE
SE <- function(Xi,Xj, l) {exp(-(abs(Xi - Xj)))}
stat.cov <- function(X, Y) {outer(X, Y, SE, l)}
stat.cov(x, x)




## OR using the 'fields' library
suppressMessages(library(fields))
stationary.cov(x)









############### Log likelihood Implementation ###################
N = 5
y = rnorm(5)

mu_vector <- as.matrix(y - mean(y))

log_likelihood <- -0.5*N*log(2*pi) - 0.5*log(det(C)) - 0.5*t(mu_vector) %*% solve(C) %*% mu_vector








# Note: However, we want the [absolute] stationary covariance function to depend on 3 hyperparameters. So we cannot use 'stationary.cov' function 
# Hyperparameters
sigma = 1
l = 1
nu = 2

(x = 1:5)



## ABSOLUTE VALUE - Initially, According to Brendon
SE = function(Xi, Xj, l) {sigma^2 * exp(-(abs(Xi - Xj)/l)^nu)}
stat.cov = function(X, Y) {outer(X, Y, SE, l)}
(C = stat.cov(x, x))





## SQUARED EXPONENTIALS
SE <- function(Xi, Xj, l) {exp(-0.5 * (Xi - Xj)^2 / l^2)}
stat.cov <- function(X, Y) {outer(X, Y, SE, l)}
stat.cov(x, x)







### Note to self: I know the below gives the same answer as line 95, but I feel like it's more right.

## ABSOLUTE VALUE - Initially, According to Brendon
SE = function(Xi, Xj, l) {sigma^2 * exp(                    -(abs((Xi - Xj)/l))^nu                          )}
stat.cov = function(X, Y) {outer(X, Y, SE, l)}
(C = stat.cov(x, x))




