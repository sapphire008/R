# Tutorial 08: 07/04/13
# Linear Regression and ANOVA

# Performing simple linear regression (lm(y~x))
#where x is the predictor variable and y is the response variable

x = seq(from = 0, to = 10, by = 0.2);
y = seq(from = 0, to = 20, by = 0.4)+rnorm(length(x),mean = 0, sd = 1);
lm(y~x);
# Call:
#   lm(formula = y ~ x)
# 
# Coefficients:
#   (Intercept)            x  
#     0.0448            1.9391  
#
# suppose y_i = B_0 + B_1 * x_i +e_i
# intercept referes to B_0, and the number under x is the coefficient of x, which is B_1
# the e_i in the model equation is the error term

# for a two columned dataframe dfrm, a linear regression model is built by
lm(y~x, data = dfrm));#where x and y refers to the column headers

# Performing multiple linear regression (lm(y~u+v+w))
# given u, v, and w as predictor variables, and y as response variables
# suppose, y_i = B_0 + B_1 * u_i + B_2 * v_i + B_3 * w_i + e_i
u = seq(from = 0, to = 10, by = 1) + rnorm(11, mean = 0, sd = 1);
v = seq(from = -5, to = 5, by = 1) + runif(11, min = 0, max = 2);
w = seq(from = 0, to = 1, by = 0.1) + rexp(11, rate = 0.2);
y = seq(from = 0, to = 20, by = 2) + rnorm(11, mean = 2, sd = 5);
lm(y~u+v+w)

# Call:
#   lm(formula = y ~ u + v + w)
# 
# Coefficients:
#   (Intercept)            u            v            w  
#      23.4892         -2.7615       5.1580       -0.3704 

# similarly, with a dataframe dfrm, the model can be built as 
lm(y~u+v+w, data = dfrm);#where u v w and y are column headers


# Getting regression statistics
m = lm(y~u+v+w); #saves the regression results to a variable
# use the following function to get regressino statistics

# ANOVA table
anova(m);
# Analysis of Variance Table
# 
# Response: y
#           Df  Sum Sq Mean Sq F value  Pr(>F)  
# u          1 212.400 212.400 11.8104 0.01089 *
#   v        1 106.269 106.269  5.9090 0.04536 *
#   w        1  36.015  36.015  2.0026 0.19994  
# Residuals  7 125.890  17.984                  
# ---
# Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1

# model coefficients
coefficients(m);# or use coef(m)
# (Intercept)           u           v           w 
#  23.4892261  -2.7614948   5.1579797  -0.3703507 

# confidence intervals for the regression coefficients
confint(m);
#                    2.5 %     97.5 %
# (Intercept)  9.068686659 37.9097655
# u           -6.786207525  1.2632179
# v            0.007835371 10.3081241
# w           -0.989195333  0.2484939
# 
# 

# residual sum of squares
deviance(m);
#[1] 125.8895

# vector of orthogonal effects
effects(m);
# (Intercept)           u           v           w                                                                         
# -46.8756547  14.5739461 -10.3086675  -6.0012194  -5.9077817   2.7035059  -0.6258348   1.8590843  -3.5359748  -7.8881864 
# 
# 2.2592525 
# attr(,"assign")
# [1] 0 1 2 3
# attr(,"class")
# [1] "coef"

# vector of fitted y values
fitted(m);
#     1         2         3         4         5         6         7         8         9        10        11 
# 9.446107  6.589097 11.032602 10.536942  7.097795 10.314934 19.609836 18.889369 18.259783 21.320696 22.371798 

# model residuals
residuals(m);# or use resid(m)
#       1          2          3          4          5          6          7          8          9         10         11 
# 3.0377119 -1.9981474 -1.4350622 -2.7403207 -1.4549360  5.5630171 -0.2732539  3.1266532 -1.8100226 -6.2460569  4.2304176 

# key statistics, such as R^2, the F statistics, and the residual standard error(sigma)
summary(m);
# Call:
#   lm(formula = y ~ u + v + w)
# 
# Residuals:
#   Min     1Q  Median     3Q    Max 
# -6.246 -1.904 -1.435  3.082  5.563 
# 
# Coefficients:
#              Estimate,   Std. Error,    t value,  Pr(>|t|)   
# (Intercept)    23.4892     6.0984       3.852     0.00628 **
#   u            -2.7615     1.7021      -1.622     0.14874   
#   v             5.1580     2.1780       2.368     0.04974 * 
#   w            -0.3704     0.2617      -1.415     0.19994   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01'*'0.05'.'0.1' '1
# 
# Residual standard error: 4.241 on 7 degrees of freedom
# Multiple R-squared:  0.738,  Adjusted R-squared:  0.6258 
# F-statistic: 6.574 on 3 and 7 DF,  p-value: 0.01911

# Some comments on the summary ouput
# Using Coeffieints can help us determine useful variables. The smaller Pr(>|t|)
#means the variable is significantly contributing to the model
# Signif. codes: *** 0<p<0.001, ** 0.001<p<0.01, * 0.01<p<0.05, . 0.05<p<0.1, (blank) 0.1<p<1

# R^2 is a measure of the model's equality, the bigger the better. It is the fractino of the variance of y
#that is explained by the regression model. Adjusted R-squared is recommended to be used

# F statistics assess how well the model fit / how significant the model is. It is significant if any 
#of the coefficients are nonzero, and it is insignificant if all coefficients are zero. Most directly,
# p-value must be less than 0.05.


# variance-covariance matrix of the main parameters
vcov(m);
#             (Intercept)           u           v           w
# (Intercept)  37.1910752 -9.86259714 12.13107880 -0.27686634
# u            -9.8625971  2.89697998 -3.63048684 -0.01919625
# v            12.1310788 -3.63048684  4.74367061  0.02433073
# w            -0.2768663 -0.01919625  0.02433073  0.06849186

# Performing linear regression without an intercept 
lm(y~m+0);#forcing zero intercept
# the regression equation is now: y_i = B * x_i + e_i
#Only do this after performing a normal regression and see if the model's intercept can be reasonably zero

# Performing linear regression with interaction terms (lm(y~u*v))
#which corresponds to the model:
# y_i = B_0 + B_1 * u_i + B_2 * v_i + B_3 * u_i * v_i + e_i
# where the term B_3 * u_i * v_i is the interaction term
# if we do lm(y~u*V*w), R will include all possible interaction terms (all combination of 
# arbitrary two terms--first order interaction, and U*V*w term--secondary interaction)

# to specify a single interaction, use
# y~u+v+w+u:v:w; #this will only include u*v*w interaction term

# to sepcify all possible interaction of a certain level
# (u+v+w+...)^2 will specify all variables, and all possible 1st level interactions (up to 1st level)
# (u+v+w+...)^3 will specify all variables, and all possible 1s and 2nd level interactions (up to 2nd level), 
# ...etc

# distribution law
# x*(u+v+w+...) is equivalent to x*u + x*v + x*w + ... or x + u + v + w + ... + x:u + x:v + x:w +...
# x:(u+v+w+...) is equivalent to x:u + x:v + x:w + ...

# Selecting best regression varialbes
# backward model: start with full model, then reduce the variables
full.mode = lm(y~x1+x2+x3+x4);
reduced.model = step(full.model, direction = "backward");
# forward model: start with minimal model, then add more variables to make it better fit
min.model = lm(y~1);
fwd.model = step(min.model, direction = "forward", scope = (~ x1+x2+x3+x4));
# the "step" function will produce a large amount of output each time it explores a model
# with certain variables included/excluded; to turn if off, specify trace = 0 in the function

# Regression on a subset of data
lm(y ~ x, subset = 1:100); #only use data x[1:100]
lm(y ~ x, subset = (lab=="NJ")); # select only data with label/factor being "NJ"

# Using an expression inside a regression formula
lm(y~I(u+v)); #regression equation becomes: y_i = B_0 + B_1 * (u_i + v_i) + e_i
# surround the regression term by I(...) operator will make R interpret the expression as calculations

# Regression on a polynomial
lm(y~poly(x,3,raw = TRUE));
# the regression equation is:
# y_i = B_0 + B_1 * x_i + B_2 * (x_i)^2 + B_3 * (x_i)^3 + e_i

# Regression on transformed data
lm(log(y) ~ x); #log(y) = B_0 + B_1 * x_i + e_i
lm(sqrt(y) ~ x); # sqrt(y) =  B_0 + B_1 * x_i + e_i
lm(y ~ sqrt(x)); # y =  B_0 + B_1 * sqrt(x_i) + e_i
# can apply transformation to both sides of the data

# Finding the best power transformation (Box-Cox Procedure)
# apply a power transformation to your data to improve the fit of the model
# library(MASS);
# m = lm(y~x);
# boxcox(m);

library(MASS);
x = 10:100;
eps = rnorm(length(x), sd = 5);
y = (x+eps)^(-1/1.5); # y^(-1.5) = x + e
m = lm(y~x); # assuming we didn't know the transformation, and model as if y_i = B_0 + B_1 * x_i + e_i
summary(m);
# Call:
#   lm(formula = y ~ x)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.034279 -0.017143 -0.005978  0.010642  0.106279 
# 
# Coefficients:
#              Estimate   Std. Error  t value Pr(>|t|)    
# (Intercept)  1.636e-01  6.059e-03   27.00   <2e-16 ***
#   x           -1.421e-03  9.941e-05  -14.29   <2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.02491 on 89 degrees of freedom
# Multiple R-squared:  0.6965,  Adjusted R-squared:  0.6931 
# F-statistic: 204.2 on 1 and 89 DF,  p-value: < 2.2e-16
plot(m, which = 1); #plot residual vs. fitted, seen non-linear relationship
# run boxcox procedure
bcm = boxcox(m);
#in the plot, we want to find a lambda that maximizes log-Likelihood
#there will be three vertical dotted lines, the middle one corresponds to the maximum log likelihood
# the other two are confidence intervals of the maximum log likelihood

which.max(bcm$y); # we find the maximum log-Likelihood
lambda = bcm$x[which.max(bcm$y)];#we find lambda, which is the power that y being transformed to
#lambda should be very close to -1.5, which is the inverse of the power that we transformed y with
#we simply apply lambda to transform y back when building the model
m2 = lm(I(y^lambda)~x);
summary(m2);
# Call:
#   lm(formula = z ~ x)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -10.5595  -2.3045   0.2227   2.8030  10.7371 
# 
# Coefficients:
#           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.44695    0.98887   1.463    0.147    
# x            0.80756    0.01622  49.776   <2e-16 ***
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 4.065 on 89 degrees of freedom
# Multiple R-squared:  0.9653,  Adjusted R-squared:  0.9649 
# F-statistic:  2478 on 1 and 89 DF,  p-value: < 2.2e-16

# Here R-squared is greatly improved after the transformation
# By default, the range of lambda is between -2 and 2. specify lambda argument to define the range
# If the confidence interval (indicated by the vertical dotted line) includes 1, then the transformation may not be valid

# Plotting regression residuals
m = lm(y~m);
plot(m, which = 1);
#which = 1; # Residuals vs Fitted
#which = 2; # Normal Q-Q
#which = 3; # Scale-Location
#which = 4; # Residuals vs Leverage
# the red solid curve should give a visual guide for the patterns lies within the plot

# Diagnosing a linear regression
#plot diagnositcs
plot(m);
# characteristics of good regression model
#1). the points in the Residual vs. Fitted plot are randomly scattered with no particular pattern
#usually, the pattern of the Residual vs. Fitted plot tells us about what are left out. If there is no pattern,
#it means only noise are left out, and everything has been modeled.
#2). The points in Normal Q-Q plot are more-or-less on the line, indicating that the residuals
#follow a normal distribution
#3). In both the Scale-Location plot and the Residuals vs. Leverage plots, the points are
# in a group with none too far from the center

#identify outliers
library(Car);# requires installation of car pacakge, as it does not come with standard distribution
outlier.test(m);

#identify influential observations
influence.measures(m);
#influential observations will be marked with *
#if an observation is called "influential", it means that removing the observation can significantly
#change the fitted regression model
#If this influential observation happens to be an outlier, then it can distort the model
#Very likely, the influential observation is an outlier at the same time.

#         dfb.1_     dfb.x    dffit cov.r   cook.d    hat inf
#   1   0.826948 -7.17e-01  0.82979 0.777 2.97e-01 0.0432   *
#   2   0.587901 -5.07e-01  0.59043 0.897 1.62e-01 0.0418   *
#   3   0.518185 -4.45e-01  0.52094 0.926 1.28e-01 0.0404   *
#   4   0.977512 -8.34e-01  0.98382 0.660 3.85e-01 0.0391   *
#   5   0.220427 -1.87e-01  0.22214 1.033 2.46e-02 0.0378    
#   6   0.321343 -2.71e-01  0.32432 0.998 5.16e-02 0.0365    
#   7   0.391213 -3.28e-01  0.39549 0.964 7.54e-02 0.0352    
#   8  -0.026327  2.19e-02 -0.02666 1.058 3.59e-04 0.0340    

# Testing residuals for autocorrelation (Durbin-Watson Test)
library(lmtest);#lmtest is not in standard distribution of R
m = lm(y~x);
dwtest(m);
# Durbin-Watson test
# 
# data:  m
# DW = 0.3983, p-value < 2.2e-16
# alternative hypothesis: true autocorrelation is greater than 0

# if p-value is less than 0.05, residuals are significantly auto-correlated.
# This will be bad, and it means the model is lacking some significant predictor variables
# The p-value output simply answers: is the autocorrelation of the residuals greater than zero?
# If the autocorrelation is negative, then use "alternative" argument in "dwtest"
dwtest(m, alternative = "two.sided");

# Predicting new values from regression model
# given a value for each predictor, what is the predicted value of response variable?
u = seq(from = 0, to = 10, by = 1) + rnorm(11, mean = 0, sd = 1);
v = seq(from = -5, to = 5, by = 1) + runif(11, min = 0, max = 2);
w = seq(from = 0, to = 1, by = 0.1) + rexp(11, rate = 0.2);
y = seq(from = 0, to = 20, by = 2) + rnorm(11, mean = 2, sd = 5);

m = lm(y~u+v+w);#build the regression model
#now given 4 groups of values for each predictor, stored in a data frame
preds = data.frame(u = c(3.1,3.2,3.3,3.4), 
                   v = c(4.0,4.1,4.2,4.3), 
                   w = c(5.5,5.6,5.7,5.8));
#predict the response variable y for each group of u, v, and w
predict(m, newdata = preds);
#        1        2        3        4 
# 14.72077 14.97128 15.22180 15.47232 

# Forming prediction intervals
#finds the confidence interval of the predicted value
#by default gives 95% confidence interval.
#To Specify confidence interval, use "level" argument
predict(m, newdata = preds, interval = "prediction");
#        fit       lwr      upr
# 1 14.72077 -5.032682 34.47422
# 2 14.97128 -4.740736 34.68331
# 3 15.22180 -4.449568 34.89317
# 4 15.47232 -4.159183 35.10382

# The prediction interval strongly follows normality, therefore, very sensitive to it.
# If the distribution is not normal, use non-parametric techniques

# Performing One-Way ANOVA
oneway.test(x~f); #x is observations, f is a factor that defines groups
#it assumes that the data has normal distribution.
#if not, use Kruskal-Wallis test
# use "subset" argument for testing only a subset of the data
# To assume all groups have equal variance, use "var.equal = TRUE" argument
# aov(x~f) always assume equal variance, which is not as flexible as oneway.test(x~f)

# Creating an interaction plot
# interaction.plot(pred1,pred2,resp);
#pred1 and pred2 are two predictor variables, and resp is response variable
library(faraway);
data(rats);
interaction.plot(rats$poison, rats$treat, rats$time);
# the bend of the rats$time vs rats$poison at rats$treat group D and B suggests that there is an interaction
#between poison and treat

# Finding differences between means of groups
m = aov(x~f);
TukeyHSD(m);
plot(TukeyHSD(m));# a graphical representation of the group difference, with confidence interval

# Performing Robust ANOVA(Kruskal-Wallis Test)
kruskal.test(x~f);#for not normally distributed data, but each group has similar distribution
#Kruskal-Wallis Test is non-parametric

# Comparing models by using ANOVA
anova(m1, m2);
#m1 and m2 are model objects returned by lm
#if p-value is less than 0.05, then the models are significantly different from each other
# Requirement: one model must be contained within the other.
# For instance m1 = lm(y~x1+x2+x3+x4+x5), and m2 = lm(y~x1+x2), where m2 is contained within m1
# This will allow m1 and m2 comparable.
# It can be used to determine whether adding a term is really affecting the model or not. If the newly
# added term does not really change the model, then it is possible to say the extra term is not worth to
# be added to increase the complexity of the model

# This concludes today's study.