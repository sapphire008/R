# Fitting linear model by maximum likelihood
library(stats4)
LL <- function(m, beta, mu, sigma){
  # Residuals
  a = beta[0]
  b = beta[1]
  c = beta[2]
  R = z - x * a - y * b - c
  
  R = suppressWarnings(dnorm(R, mu, sigma, log=TRUE))
  
  -sum(R)
}

fit <- mle(LL, start = list(beta0 = 4, beta1 = 2, mu = 0, sigma=1))