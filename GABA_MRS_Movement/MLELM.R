library(plyr)
library(bbmle)

source_data <- "D:/Edward/Documents/Assignments/Imaging Research Center/GABA Movement Second Submission/Analysis/data/movement/movement_data_v1_c.rds"
data <- readRDS(source_data)

remove_outlier<- function(x, mode = "") {
  # remove all the outliers. If the data is near normal distribution,
  # it should keep the data only within 2.7sigma within the mean, or
  #[Q1-1.5*IQR, Q3+1.5*IQR] non-parametrically
  # Boxplot is non-parametric, so it does not assume normality
  BP = boxplot(x,plot=FALSE)
  switch(mode,
         higher={x[(x %in% BP$out[BP$out>BP$stats[5]])]=NA},
         lower={x[(x %in% BP$out[BP$out<BP$stats[1]])]=NA},
{x[(x %in% BP$out)] = NA})
# return x
return(x)
}

# Get good data
data.good = subset(data, subset= data$Good.Runs == 1 & data$Instructed.to.move == 0) #remove bad data
# relabel region run order
A<- ddply(data.good, .(Subject), function(x) data.frame(x,NewID=1:nrow(x)))
data.good$Region.Run.Order <- as.factor(A$NewID)
# Remove outlier
data.clean <- data.good
data.clean[c("D","S")] <- sapply(data.clean[c("D","S")], remove_outlier)
# Aggregate to subject level data
data.clean.GABA.D <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, GABA.Cr, D)), FUN = mean, na.rm = T)
data.clean.GABA.S <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, GABA.Cr, S)), FUN = mean, na.rm = T)
data.clean.GABA.DS <- aggregate(.~ Subject , data = subset(data.clean,select=c(Subject, GABA.Cr, D,S)),FUN= mean, na.rm = T)
data.clean.NAA.D  <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, NAA.Cr, D)), FUN = mean, na.rm = T)
data.clean.NAA.S  <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, NAA.Cr, S)), FUN = mean, na.rm = T)
data.clean.NAA.DS <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, NAA.Cr, D,S)),FUN= mean, na.rm = T)
data.clean.Glx.D  <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, Glx.Cr, D)), FUN = mean, na.rm = T)
data.clean.Glx.S  <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, Glx.Cr, S)), FUN = mean, na.rm = T)
data.clean.Glx.DS <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, Glx.Cr, D,S)),FUN= mean, na.rm = T)


# Use maximum likelihood to do simple linear regression: better than least square
LL <- function(params){
  beta = matrix(NA, nrow = length(params) - 2, ncol = 1)
  beta[,1] = params[1:(length(params)-2)]
  mu       = params[[length(params)-1]]
  sigma    = params[[length(params)]]
  minusll  = -sum(suppressWarnings(dnorm(Y - X %*% beta, 0, sigma, log=T))) # Minus of log likelihood
  return(minusll)
}

residuals.mle<- function(params) return(Y -X %*%as.matrix(params[1:(length(params)-2)]))
predict.mle <- function(params) return(X %*%as.matrix(params[1:(length(params)-2)]))

# GABA
X <- model.matrix(lm(GABA.Cr ~ D + S, data = data.clean.GABA.DS))
Y <- data.clean.GABA.DS$GABA.Cr
parnames(LL) <- c('Intercept', 'D', 'S', 'mu', 'sigma')
mle.GABA.DS <- mle2(LL, start = c(Intercept = 0.1, D = 0.2, S = 0.1, mu = 0, sigma = 1), 
     vecpar = TRUE, parnames = c('Intercept', 'D', 'S', 'mu', 'sigma'))
# Maximum likelihood estimation
# 
# Call:
#   mle2(minuslogl = LL, start = c(Intercept = 0.1, D = 0.2, S = 0.1, 
#                                  mu = 0, sigma = 1), vecpar = TRUE, parnames = c("Intercept", 
#                                                                                  "D", "S", "mu", "sigma"))
# 
# Coefficients:
#   Estimate  Std. Error z value     Pr(z)    
# Intercept  1.6877e-01  6.6436e-03 25.4031 < 2.2e-16 ***
#   D         -9.5368e-03  4.7700e-03 -1.9993  0.045574 *  
#   S         -1.8287e-01  6.1756e-02 -2.9612  0.003064 ** 
#   mu         0.0000e+00  3.0317e-15  0.0000  1.000000    
# sigma      1.1007e-02  1.4784e-03  7.4454 9.667e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -173.2451

X <- model.matrix(lm(GABA.Cr ~ D, data = data.clean.GABA.D))
Y <- data.clean.GABA.D$GABA.Cr
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
# Intercept  1.5577e-01  5.5588e-03 28.0222 < 2.2e-16 ***
#   D         -1.0391e-02  5.1171e-03 -2.0305    0.0423 *  
#   mu         0.0000e+00  9.6376e-16  0.0000    1.0000    
# sigma      1.2387e-02  1.6345e-03  7.5788 3.487e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -172.5711

X <- model.matrix(lm(GABA.Cr ~ S, data = data.clean.GABA.S))
Y <- data.clean.GABA.S$GABA.Cr
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
# Intercept  1.6043e-01  5.1357e-03 31.2385 < 2.2e-16 ***
#   S         -1.8650e-01  6.0571e-02 -3.0791  0.002076 **
#   mu         0.0000e+00  2.8010e-14  0.0000  1.000000    
# sigma      1.1508e-02  1.5228e-03  7.5576 4.105e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -176.9467 

# NAA
X <- model.matrix(lm(NAA.Cr ~ D + S, data = data.clean.NAA.DS))
Y <- data.clean.NAA.DS$NAA.Cr
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
#   Estimate  Std. Error z value   Pr(z)    
# Intercept  1.8018e+00  5.9850e-02 30.1058 < 2e-16 ***
# D         -5.0059e-02  4.2585e-02 -1.1755  0.2398    
# S          5.9440e-03  5.7699e-01  0.0103  0.9918    
# mu         0.0000e+00  1.3518e-11  0.0000  1.0000    
# sigma      9.8290e-02  1.3136e-02  7.4824 7.3e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -50.45492 

X <- model.matrix(lm(NAA.Cr ~ D, data = data.clean.NAA.D))
Y <- data.clean.NAA.D$NAA.Cr
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
#   Estimate  Std. Error z value     Pr(z)    
# Intercept  1.7875e+00  4.5878e-02 38.9626 < 2.2e-16 ***
#   D         -2.8772e-02  4.2114e-02 -0.6832    0.4945    
# mu         0.0000e+00  6.1171e-14  0.0000    1.0000    
# sigma      1.0190e-01  1.3382e-02  7.6149 2.639e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -50.16316

X <- model.matrix(lm(NAA.Cr ~ S, data = data.clean.NAA.S))
Y <- data.clean.NAA.S$NAA.Cr
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
#   Estimate  Std. Error z value     Pr(z)    
# Intercept  1.7679e+00  4.5673e-02 38.7084 < 2.2e-16 ***
# S         -1.2854e-01  5.4529e-01 -0.2357    0.8136    
# mu         0.0000e+00  7.7801e-13  0.0000    1.0000    
# sigma      9.9816e-02  1.3108e-02  7.6151 2.636e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -51.36223 

# Glx
X <- model.matrix(lm(Glx.Cr ~ D + S, data = data.clean.Glx.DS))
Y <- data.clean.Glx.DS$Glx.Cr
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
# Intercept  1.3020e-01  7.3627e-03 17.6838 < 2.2e-16 ***
#   D         -4.0622e-03  5.2388e-03 -0.7754    0.4381    
# S          3.6026e-02  7.0981e-02  0.5075    0.6118    
# mu         0.0000e+00  1.1135e-13  0.0000    1.0000    
# sigma      1.2092e-02  1.6256e-03  7.4382 1.021e-13 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -168.0179 

X <- model.matrix(lm(Glx.Cr ~ D, data = data.clean.Glx.D))
Y <- data.clean.Glx.D$Glx.Cr
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
# Intercept  1.3287e-01  5.5395e-03 23.9864 < 2.2e-16 ***
#   D         -4.2456e-03  5.0850e-03 -0.8349    0.4038    
# mu         0.0000e+00  2.6552e-16  0.0000    1.0000    
# sigma      1.2304e-02  1.6204e-03  7.5930 3.127e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -172.8912 

X <- model.matrix(lm(Glx.Cr ~ S, data = data.clean.Glx.S))
Y <- data.clean.Glx.S$Glx.Cr
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
# Intercept  1.3132e-01  6.4165e-03 20.4657 < 2.2e-16 ***
#   S         -4.7113e-02  7.6607e-02 -0.6150    0.5386    
# mu         0.0000e+00  1.0891e-14  0.0000    1.0000    
# sigma      1.4023e-02  1.8475e-03  7.5902 3.195e-14 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# -2 log L: -165.3215 