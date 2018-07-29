library(plyr)
library(bbmle)

source_data <- "/Users/Edward/Documents/Ubuntu/Analysis/data/movement/movement_data_v1_c.rds"
data <- readRDS(source_data)

# Get good data
data.good = subset(data, subset= data$Good.Runs == 1 & data$Instructed.to.move == 0) #remove bad data
# relabel region run order
A<- ddply(data.good, .(Subject), function(x) data.frame(x,NewID=1:nrow(x)))
data.good$Region.Run.Order <- as.factor(A$NewID)

# Aggregate to subject level data
data.good.GABA.D <- aggregate(. ~ Subject , data = subset(data.good,select=c(Subject, GABA.Cr, D)), FUN = mean, na.rm = T)
data.good.GABA.S <- aggregate(. ~ Subject , data = subset(data.good,select=c(Subject, GABA.Cr, S)), FUN = mean, na.rm = T)
data.good.GABA.DS<- aggregate(. ~ Subject , data = subset(data.good,select=c(Subject, GABA.Cr, D,S)),FUN= mean, na.rm = T)
data.good.NAA.D  <- aggregate(. ~ Subject , data = subset(data.good,select=c(Subject, NAA.Cr, D)), FUN = mean, na.rm = T)
data.good.NAA.S  <- aggregate(. ~ Subject , data = subset(data.good,select=c(Subject, NAA.Cr, S)), FUN = mean, na.rm = T)
data.good.NAA.DS <- aggregate(. ~ Subject , data = subset(data.good,select=c(Subject, NAA.Cr, D,S)),FUN= mean, na.rm = T)
data.good.Glx.D  <- aggregate(. ~ Subject , data = subset(data.good,select=c(Subject, Glx.Cr, D)), FUN = mean, na.rm = T)
data.good.Glx.S  <- aggregate(. ~ Subject , data = subset(data.good,select=c(Subject, Glx.Cr, S)), FUN = mean, na.rm = T)
data.good.Glx.DS <- aggregate(. ~ Subject , data = subset(data.good,select=c(Subject, Glx.Cr, D,S)),FUN= mean, na.rm = T)


# Use maximum likelihood to do simple linear regression: better than least square
LL <- function(params){
  beta = matrix(NA, nrow = length(params) - 2, ncol = 1)
  beta[,1] = params[1:(length(params)-2)]
  mu       = params[[length(params)-1]]
  sigma    = params[[length(params)]]
  minusll  = -sum(suppressWarnings(dnorm(Y - X %*% beta, 0, sigma, log=T)))
  return(minusll)
}

residuals.mle<- function(m) return(Y -X %*%as.matrix(coef(params)[1:(length(coef(m))-2)]))
predict.mle <- function(m, x) return(x %*%as.matrix(coef(params)[1:(length(coef(m))-2)]))

# GABA
X <- model.matrix(lm(GABA.Cr ~ D + S, data = data.good.GABA.DS))
Y <- data.good.GABA.DS$GABA.Cr
parnames(LL) <- c('Intercept', 'D', 'S', 'mu', 'sigma')
mle.GABA.DS <- mle2(LL, start = c(Intercept = 0.1, D = 0.2, S = 0.1, mu = 0, sigma = 1), 
                    vecpar = TRUE, parnames = c('Intercept', 'D', 'S', 'mu', 'sigma'))
resid.GABA.DS <- residuals.mle(mle.GABA.DS)
# Maximum likelihood estimation
# 
# Call:
#   mle2(minuslogl = LL, start = c(Intercept = 0.1, D = 0.2, S = 0.1, 
#                                  mu = 0, sigma = 1), vecpar = TRUE, parnames = c("Intercept", 
#                                                                                  "D", "S", "mu", "sigma"))
# 
# Coefficients:
#   Estimate  Std. Error z value     Pr(z)    
# Intercept  1.5482e-01  5.0524e-03 30.6419 < 2.2e-16 ***
#   D         -1.8290e-03  3.4413e-03 -0.5315   0.59507    
# S         -8.5165e-02  5.0000e-02 -1.7033   0.08851 .  
# mu         0.0000e+00  8.7497e-15  0.0000   1.00000    
# sigma      1.2778e-02  1.6907e-03  7.5581 4.088e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -170.873 

X <- model.matrix(lm(GABA.Cr ~ D, data = data.good.GABA.D))
Y <- data.good.GABA.D$GABA.Cr
parnames(LL) <- c('Intercept', 'D', 'mu', 'sigma')
mle.GABA.D <- mle2(LL, start = c(Intercept = 0.1, D = 0.2, mu = 0, sigma = 1),
                   vecpar = TRUE, parnames=c('Intercept', 'D', 'mu', 'sigma'))

# Maximum likelihood estimation
# 
# Call:
#   mle2(minuslogl = LL, start = c(Intercept = 0.1, D = 0.2, mu = 0, 
#                                  sigma = 1), vecpar = TRUE, parnames = c("Intercept", "D", 
#                                                                          "mu", "sigma"))
# 
# Coefficients:
#   Estimate  Std. Error z value     Pr(z)    
# Intercept  1.5045e-01  4.5639e-03 32.9660 < 2.2e-16 ***
#   D         -4.2252e-03  3.2884e-03 -1.2849    0.1988    
# mu         0.0000e+00  1.7745e-17  0.0000    1.0000    
# sigma      1.3386e-02  1.7666e-03  7.5773 3.529e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -168.0819 

X <- model.matrix(lm(GABA.Cr ~ S, data = data.good.GABA.S))
Y <- data.good.GABA.S$GABA.Cr
parnames(LL) <- c('Intercept', 'S', 'mu', 'sigma')
mle.GABA.S <- mle2(LL, start = c(Intercept = 0.1, S = 0.2, mu = 0, sigma = 1),
                   vecpar = TRUE, parnames=c('Intercept', 'S', 'mu', 'sigma'))

# Maximum likelihood estimation
# 
# Call:
#   mle2(minuslogl = LL, start = c(Intercept = 0.1, S = 0.2, mu = 0, 
#                                  sigma = 1), vecpar = TRUE, parnames = c("Intercept", "S", 
#                                                                          "mu", "sigma"))
# 
# Coefficients:
#   Estimate  Std. Error z value     Pr(z)    
# Intercept  1.5363e-01  4.5073e-03 34.0846 < 2.2e-16 ***
#   S         -9.6123e-02  4.5620e-02 -2.1070   0.03511 *  
#   mu         0.0000e+00  2.0829e-14  0.0000   1.00000    
# sigma      1.2781e-02  1.6791e-03  7.6118 2.703e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -170.5897 

X <- model.matrix(lm(NAA.Cr ~ D + S, data = data.good.NAA.DS))
Y <- data.good.NAA.DS$NAA.Cr
parnames(LL) <- c('Intercept', 'D', 'S', 'mu', 'sigma')
mle.NAA.DS <- mle2(LL, start = c( Intercept = 0.1, D = 0.2, S = 0.1,  mu = 0, sigma = 1),
                   vecpar = TRUE, parnames=c('Intercept', 'D', 'S', 'mu', 'sigma'))

# Maximum likelihood estimation
# 
# Call:
#   mle2(minuslogl = LL, start = c(Intercept = 0.1, D = 0.2, S = 0.1, 
#                                  mu = 0, sigma = 1), vecpar = TRUE, parnames = c("Intercept", 
#                                                                                  "D", "S", "mu", "sigma"))
# 
# Coefficients:
#   Estimate  Std. Error z value     Pr(z)    
# Intercept  1.7364e+00  3.9245e-02 44.2454 < 2.2e-16 ***
#   D          2.5901e-02  2.6731e-02  0.9690    0.3326    
# S         -8.8357e-02  3.8838e-01 -0.2275    0.8200    
# mu         0.0000e+00  3.7363e-13  0.0000    1.0000    
# sigma      9.9258e-02  1.3034e-02  7.6154 2.629e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -51.68566 

X <- model.matrix(lm(NAA.Cr ~ D, data = data.good.NAA.D))
Y <- data.good.NAA.D$NAA.Cr
parnames(LL) <- c('Intercept', 'D', 'mu', 'sigma')
mle.NAA.D <- mle2(LL, start = c(Intercept = 0.1, D = 0.2, mu = 0, sigma = 1),
                  vecpar = TRUE, parnames=c('Intercept', 'D', 'mu', 'sigma'))
# Maximum likelihood estimation
# 
# Call:
#   mle2(minuslogl = LL, start = c(Intercept = 0.1, D = 0.2, mu = 0, 
#                                  sigma = 1), vecpar = TRUE, parnames = c("Intercept", "D", 
#                                                                          "mu", "sigma"))
# 
# Coefficients:
#   Estimate Std. Error z value     Pr(z)    
# Intercept 1.7319e+00 3.3870e-02 51.1351 < 2.2e-16 ***
#   D         2.3394e-02 2.4404e-02  0.9586    0.3378    
# mu        0.0000e+00 7.0069e-15  0.0000    1.0000    
# sigma     9.9342e-02 1.3044e-02  7.6160 2.616e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -51.63328

X <- model.matrix(lm(NAA.Cr ~ S, data = data.good.NAA.S))
Y <- data.good.NAA.S$NAA.Cr
parnames(LL) <- c('Intercept', 'S', 'mu', 'sigma')
mle.NAA.S <- mle2(LL, start = c(Intercept = 0.1, S = 0.2, mu = 0, sigma = 1),
                  vecpar = TRUE, parnames=c('Intercept', 'S', 'mu', 'sigma'))

# Maximum likelihood estimation
# 
# Call:
#   mle2(minuslogl = LL, start = c(Intercept = 0.1, S = 0.2, mu = 0, 
#                                  sigma = 1), vecpar = TRUE, parnames = c("Intercept", "S", 
#                                                                          "mu", "sigma"))
# 
# Coefficients:
#   Estimate Std. Error z value     Pr(z)    
# Intercept 1.7537e+00 3.5568e-02 49.3051 < 2.2e-16 ***
#   S         6.5272e-02 3.5999e-01  0.1813    0.8561    
# mu        0.0000e+00 1.0891e-12  0.0000    1.0000    
# sigma     1.0086e-01 1.3245e-02  7.6148  2.64e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -50.76162 


# Glx
X <- model.matrix(lm(Glx.Cr ~ D + S, data = data.good.Glx.DS))
Y <- data.good.Glx.DS$Glx.Cr
parnames(LL) <- c('Intercept', 'D', 'S', 'mu', 'sigma')
mle.Glx.DS <- mle2(LL, start = c( Intercept = 0.1, D = 0.2, S = 0.1,  mu = 0, sigma = 1),
                   vecpar = TRUE, parnames=c('Intercept', 'D', 'S', 'mu', 'sigma'))

# Maximum likelihood estimation
# 
# Call:
#   mle2(minuslogl = LL, start = c(Intercept = 0.1, D = 0.2, S = 0.1, 
#                                  mu = 0, sigma = 1), vecpar = TRUE, parnames = c("Intercept", 
#                                                                                  "D", "S", "mu", "sigma"))
# 
# Coefficients:
#   Estimate  Std. Error z value     Pr(z)    
# Intercept  1.3327e-01  5.2354e-03 25.4554 < 2.2e-16 ***
#   D         -3.8728e-03  3.5659e-03 -1.0861    0.2775    
# S         -8.1642e-03  5.1811e-02 -0.1576    0.8748    
# mu         0.0000e+00  3.7436e-15  0.0000    1.0000    
# sigma      1.3241e-02  1.7509e-03  7.5624 3.957e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -168.7882 

X <- model.matrix(lm(Glx.Cr ~ D, data = data.good.Glx.D))
Y <- data.good.Glx.D$Glx.Cr
parnames(LL) <- c('Intercept', 'D', 'mu', 'sigma')
mle.Glx.D <- mle2(LL, start = c(Intercept = 0.1, D = 0.2, mu = 0, sigma = 1),
                  vecpar = TRUE, parnames=c('Intercept', 'D', 'mu', 'sigma'))

# Maximum likelihood estimation
# 
# Call:
#   mle2(minuslogl = LL, start = c(Intercept = 0.1, D = 0.2, mu = 0, 
#                                  sigma = 1), vecpar = TRUE, parnames = c("Intercept", "D", 
#                                                                          "mu", "sigma"))
# 
# Coefficients:
#   Estimate  Std. Error z value     Pr(z)    
# Intercept  1.3285e-01  4.5168e-03 29.4127 < 2.2e-16 ***
#   D         -4.1027e-03  3.2545e-03 -1.2606    0.2074    
# mu         0.0000e+00  8.6280e-17  0.0000    1.0000    
# sigma      1.3248e-02  1.7520e-03  7.5615 3.984e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -168.7631 

X <- model.matrix(lm(Glx.Cr ~ S, data = data.good.Glx.S))
Y <- data.good.Glx.S$Glx.Cr
parnames(LL) <- c('Intercept', 'S', 'mu', 'sigma')
mle.Glx.S <- mle2(LL, start = c(Intercept = 0.1, S = 0.2, mu = 0, sigma = 1),
                  vecpar = TRUE, parnames=c('Intercept', 'S', 'mu', 'sigma'))

# Maximum likelihood estimation
# 
# Call:
#   mle2(minuslogl = LL, start = c(Intercept = 0.1, S = 0.2, mu = 0, 
#                                  sigma = 1), vecpar = TRUE, parnames = c("Intercept", "S", 
#                                                                          "mu", "sigma"))
# 
# Coefficients:
#   Estimate  Std. Error z value     Pr(z)    
# Intercept  1.3071e-01  4.7555e-03 27.4856 < 2.2e-16 ***
#   S         -3.1313e-02  4.8131e-02 -0.6506    0.5153    
# mu         0.0000e+00  1.2653e-14  0.0000    1.0000    
# sigma      1.3485e-02  1.7781e-03  7.5840 3.351e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -167.6223 