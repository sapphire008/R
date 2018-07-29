# Tutorial 07: 06/13/13
# General Statistics

# basic statistical summary of the data
vect = c(1,2,3,1,3,4,2,1,3,4,6,7,2, 0);
summary(vect);
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.250   2.500   2.786   3.750   7.000 

mat = matrix(vect,nrow = 2, ncol = 7);
summary(mat);#summarize matrix by columns
# V1             V2            V3             V4             V5             V6             V7     
# Min.   :1.00   Min.   :1.0   Min.   :3.00   Min.   :1.00   Min.   :3.00   Min.   :6.00   Min.   :0.0  
# 1st Qu.:1.25   1st Qu.:1.5   1st Qu.:3.25   1st Qu.:1.25   1st Qu.:3.25   1st Qu.:6.25   1st Qu.:0.5  
# Median :1.50   Median :2.0   Median :3.50   Median :1.50   Median :3.50   Median :6.50   Median :1.0  
# Mean   :1.50   Mean   :2.0   Mean   :3.50   Mean   :1.50   Mean   :3.50   Mean   :6.50   Mean   :1.0  
# 3rd Qu.:1.75   3rd Qu.:2.5   3rd Qu.:3.75   3rd Qu.:1.75   3rd Qu.:3.75   3rd Qu.:6.75   3rd Qu.:1.5  
# Max.   :2.00   Max.   :3.0   Max.   :4.00   Max.   :2.00   Max.   :4.00   Max.   :7.00   Max.   :2.0  

fac = factor(c('Yes','Yes','No','Maybe','Yes','No','Maybe','No','Yes'));
summary(fac)
# Maybe    No   Yes 
#   2      3     4 

#summary can also be applied to data frame (no example necessary)

# find relative frequency
mean(vect>2)
#[1] 0.5
mean(vect==0)
#[1] 0.07142857

fac = factor(c('Yes','Yes','No','Maybe','Yes','No','Maybe','No','Yes'));
fac2 = factor(c('up','down','down','up','down','down','up','up','down'));
table(fac,fac2);#create a contigency table
#           fac2
# fac     down up
# Maybe    0  2
# No       2  1
# Yes      3  1

# Testing independence of categorical variables using Chi-square test
summary(table(fac,fac2))
# Number of cases in table: 9 
# Number of factors: 2 
# Test for independence of all factors:
#   Chisq = 3.263, df = 2, p-value = 0.1957
# Chi-squared approximation may be incorrect
#            OR
# can also use chisq.test
chisq.test(table(fac,fac2))
#Pearson's Chi-squared test
#data:  table(fac, fac2)
# X-squared = 3.2625, df = 2, p-value = 0.1957

# calculate quantile
quantile(vect,c(0.25,0.95))
#    25%  95% 
#   1.25 6.35 

#if not specifying the percentage of the qunatile, R will assume by default 0%, 25%, 50%, 75%, 100%

# inverted quantile, simply use mean(vect<x) to give the fraction of vect that is less than x

# calculate z-score
vect = c(1,2,3,4,5,6,7,1,1,4,2,4,5,6,1,2,4);
scale(vect);
#         [,1]
# [1,] -1.2240169
# [2,] -0.7164977
# [3,] -0.2089785
# [4,]  0.2985407
# [5,]  0.8060599
# [6,]  1.3135792
# [7,]  1.8210984
# [8,] -1.2240169
# [9,] -1.2240169
# [10,]  0.2985407
# [11,] -0.7164977
# [12,]  0.2985407
# [13,]  0.8060599
# [14,]  1.3135792
# [15,] -1.2240169
# [16,] -0.7164977
# [17,]  0.2985407
# attr(,"scaled:center")
# [1] 3.411765
# attr(,"scaled:scale")
# [1] 1.970369


# t-test
x = rnorm(50,mean=100,sd=15);
t.test(x,mu=95);
#         One Sample t-test
# 
# data:  x
# t = 0.6577, df = 49, p-value = 0.5138
# alternative hypothesis: true mean is not equal to 95
# 95 percent confidence interval:
#   92.3205 100.2866
# sample estimates:
#   mean of x 
# 96.30357 

# find confidence interval of a mean (use t.test)
t.test(x, conf.level = 0.99)
# One Sample t-test
# 
# data:  x
# t = 48.5879, df = 49, p-value < 2.2e-16
# alternative hypothesis: true mean is not equal to 0
# 99 percent confidence interval:
#   90.99178 101.61536
# sample estimates:
#   mean of x 
# 96.30357 

# find confidence interval of a median (use wilcox.test)
wilcox.test(x,conf.int = TRUE,conf.level = 0.99)
# Wilcoxon signed rank test with continuity correction
# 
# data:  x
# V = 1275, p-value = 7.79e-10
# alternative hypothesis: true location is not equal to 0
# 99 percent confidence interval:
#   90.70928 101.73172
# sample estimates:
#   (pseudo)median 
# 96.19501 
# NOTE: Pseduo mean is not the same as media(x)


# test sample proportion is equal to the expected proportion (use prop.test(x,n,p))
#test observed 11 success out of 20 trial, if we can conclude that probability is "greater" than 0.5
prop.test(11,20,0.5,alternative = "greater");
# 1-sample proportions test with continuity correction
# 
# data:  11 out of 20, null probability 0.5
# X-squared = 0.05, df = 1, p-value = 0.4115
# alternative hypothesis: true p is greater than 0.5
# 95 percent confidence interval:
#   0.349615 1.000000
# sample estimates:
#   p 
# 0.55 

# confidence interval of a proportion
prop.test(6,9,conf.level=0.99)#default conf.level=0.95
# 1-sample proportions test with continuity correction
# 
# data:  6 out of 9, null probability 0.5
# X-squared = 0.4444, df = 1, p-value = 0.505
# alternative hypothesis: true p is not equal to 0.5
# 99 percent confidence interval:
#   0.2429060 0.9345588
# sample estimates:
#   p 
# 0.6666667 

# Testing for normality
shapiro.test(x); #large p value indicates sample is probably normally distributed
# Shapiro-Wilk normality test
# 
# data:  x
# W = 0.9711, p-value = 0.2562

# testing is a series is random
s = c(0,1,1,0,0,1,1,1,0,0,0,1,1,0);
library(tseries)#currently not in R 3.0
runs.test(as.factor(s))

# comparing means of two samples (use t.test(x,y))
x = runif(100,min=0,max=1);
y = rnorm(100,mean=0,sd = 1);
t.test(x,y);# by default, it assumes the samples are unpaired
# Welch Two Sample t-test
# 
# data:  x and y
# t = 4.5175, df = 116.633, p-value = 1.508e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.2450026 0.6275344
# sample estimates:
#   mean of x  mean of y 
# 0.53008239 0.09381386 

t.test(x,y,paired = TRUE); 
# Paired t-test
# 
# data:  x and y
# t = 4.4365, df = 99, p-value = 2.374e-05
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.2411501 0.6313870
# sample estimates:
#   mean of the differences 
# 0.4362685 

# comparing location of two samples non-parametrically
wilcox.test(x,y,paried = TRUE)
# Wilcoxon rank sum test with continuity correction
# 
# data:  x and y
# W = 6610, p-value = 8.402e-05
# alternative hypothesis: true location shift is not equal to 0

# testing significance of correlation
cor.test(x,y)
# Pearson's product-moment correlation
# 
# data:  x and y
# t = -0.6641, df = 98, p-value = 0.5082
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# -0.2599389  0.1312048
# sample estimates:
# cor 
# -0.06693843 

# testing if a binary distribution is identical between groups (use prop.test(ns,nt))
successes = c(14,10); #one group have 14 out of 38 successes, the other has 10 out of 40 successes
trials = c(38,40);
prop.test(successes,trials);
# 2-sample test for equality of proportions with continuity correction
# 
# data:  successes out of trials
# X-squared = 0.7872, df = 1, p-value = 0.3749
# alternative hypothesis: two.sided
# 95 percent confidence interval:
#   -0.1110245  0.3478666
# sample estimates:
#   prop 1    prop 2 
# 0.3684211 0.2500000 

# pair-wise comparison between group means (use pairwise.t.wise(x,g))
x = c(rnorm(5),runif(5),runif(5));
g = factor(c(1,2,2,1,3,3,1,2,1,3,1,2,1,3,1));
pairwise.t.test(x,g)
# Pairwise comparisons using t tests with pooled SD 
# 
# data:  x and g 
# 
#    1    2   
# 2 0.57  -   
# 3 0.10 0.57
# 
# P value adjustment method: holm 

# test if two samples come from the same distribution (use ks.test(x,y))
# Kolmogorov-Smirnovtest
x = rnorm(100);
y = rnorm(100);
ks.test(x,y)
# Two-sample Kolmogorov-Smirnov test
# 
# data:  x and y
# D = 0.11, p-value = 0.5806
# alternative hypothesis: two-sided--->alternative hypothesis is that they are from different distribution

# This concludes today's study.