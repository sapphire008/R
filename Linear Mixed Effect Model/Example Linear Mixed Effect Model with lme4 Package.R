# linear mixed effect model tutorial with lme4 package
# Based on: http://www.unt.edu/rss/class/Jon/Benchmarks/LinearMixedModels_JDS_Dec2010.pdf

# loading example data
lmm.data <- read.table("http://www.unt.edu/rss/class/Jon/R_SC/Module9/lmm.data.txt",
                       header = TRUE, sep = ",", na.string = "NA",
                       dec = ".", strip.white = TRUE);
# preview example data
summary(lmm.data);

# load lme4 package
library(lme4);

# fit the model with lmer function in the package
# arguments explanation:
# formula: linear mixed model formula
# Look up The "Big Five" to see details about their definitions
# Dependent variable, extro (measurement of degree of extroversion of a student)
# Independent variables / predictors:
# 1). open: Openess to experience; scale
# 2). agree: Agreeableness; scale
# 3). social: Social engagement; scale
# 4). class: Class the student is in; nominal
# 5). school: School the student is in; nominal
# 1|school/class: random effect of class within school

# data: sepcify source data
# family: what kind of distribution to assum
# REML: use REstricted Maximum Likelihood for parameter estimation
# verbose: turn on/off messages during model fitting

lmm.2 <- lmer(formula = extro ~ open + agree +social +class + (1|school/class),
              data = lmm.data, family= gaussian, REML = TRUE, verbose = FALSE);
summary(lmm.2);

# Linear mixed model fit by REML 
# Formula: extro ~ open + agree + social + class + (1 | school/class) 
# Data: lmm.data 
# AIC  BIC logLik deviance REMLdev
# 3548 3599  -1764     3509    3528
# Random effects:
#   Groups       Name        Variance Std.Dev.
# class:school (Intercept)  2.88364 1.69813 
# school       (Intercept) 95.17295 9.75566 
# Residual                  0.96837 0.98406 
# Number of obs: 1200, groups: class:school, 24; school, 6
# 
# Fixed effects:
#               Estimate Std. Error t value
# (Intercept) 57.3838787  4.0559703  14.148
# open         0.0061302  0.0049634   1.235
# agree       -0.0077361  0.0056985  -1.358
# social       0.0005313  0.0018523   0.287
# classb       2.0547978  0.9837340   2.089
# classc       3.7049300  0.9837160   3.766
# classd       5.6657332  0.9837281   5.759
# 
# Correlation of Fixed Effects:
#         (Intr) open   agree  social classb classc
# open   -0.048                                   
# agree  -0.047 -0.012                            
# social -0.045 -0.006 -0.009                     
# classb -0.121 -0.002 -0.006  0.005              
# classc -0.121 -0.001 -0.005  0.001  0.500       
# classd -0.121  0.000 -0.007  0.002  0.500  0.500

# To find out what proportion of variance is contributed by each level of random factors
# add all the variance together, then divide each variance by this total
2.88364 + 95.17295 + 0.96837
#>[1] 99.02496
# now variance contributed by class within school is
2.88364/99.02496
#>[1] 0.02912033
# or 2.9%
# this may indicate that the random effect of class in school is trivial and
# it is appropriate to remove the random effect from the model.
# In this case, however, the random effect of school is very large.

# for fixed effect, it is very much the same as regular regression
# the estimate for each fixed effect is the slope (beta values) of the corresponding predictor
# for instance, 1 unit change in "open" corresponds to 0.0061302 increase in the 
# dependent variable "extro". 
# For "classb", it is interpreted as a comparison from "classa",
# which lme4 package treat as default baseline for other nominal categories.
# Therefore, it is interpreted as, change from "classa" to "classb" will result 2.0547978
# increase in the dependent variable "extro".
# NOTE: SPSS and SAS will use "classd", instead of "classa" as default baseline for comparison.
# to make R match SPSS and SAS's output, reverse the order of the nomianl variable.

# The correlation matrix assesses the correlation between predictor variables
# If several predictors are highly correlated, the phenomenon is called multicollinearlity.

# Inspecting model residuals
# a well-fitted model should have residuals that has mean of zero
# and frequency distribution of gaussian

# extract residuals from the model
residuals <- resid(lmm.2);
# summarize the residual
summary(residuals);
# plot the residuals: frequency vs. value
hist(residuals);

# Intra-class correlation (ICC)
# It represents a measure of reliability, or dependence among individuals.
# It allows us to assess whether or not the random effect is present in the data

# Create a null model, including only the intercepts from both fixed and random effect
lmm.null <- lmer(extro ~ 1 + (1|school), data = lmm.data);
summary(lmm.null);

# Linear mixed model fit by REML 
# Formula: extro ~ 1 + (1 | school) 
# Data: lmm.data 
#  AIC  BIC logLik deviance REMLdev
# 5812 5827  -2903     5811    5806
# Random effects:
# Groups   Name        Variance Std.Dev.
# school   (Intercept) 95.8721  9.7914  
# Residual              7.1399  2.6721  
# Number of obs: 1200, groups: school, 6
# 
# Fixed effects:
#             Estimate Std. Error t value
# (Intercept)   60.267      3.997   15.08

# calculate variance contributed by random effect of school
95.8621/(95.8621+7.1399)
#>[1] 0.9306819
# Therefore, ICC = 0.9306819
# This means, about 93% of the variance in extro can be explained
# by being in different schools

