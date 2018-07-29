# Tutorial 08: 07/14/13
# Time Series Analysis

# create objects of data strcture to represent time series data
library(zoo);
ts = zoo(x,dt);
# OR
library(xts);#depends on zoo package
ts = xts(x,dt);

prices = c(132.45, 130.85, 130.00, 129.55, 130.85);
dates = as.Date(c("2010-01-04", "2010-01-05", "2010-01-06", "2010-01-07",
                  "2010-01-08"));
ibm.daily = zoo(prices, dates);
 print(ibm.daily)
# 2010-01-04 2010-01-05 2010-01-06 2010-01-07 2010-01-08 
#     132.45     130.85     130.00     129.55     130.85

# extract pure data from zoo object
coredata(ibm.daily);
# extract date or time from the zoo object
index(ibm.daily);

# plot time series
plot(x);
plot(v,type = "l"); #where v is a simple time series vector
plot.ts(v); # is similar

# Extracting the oldest or newest observations
head(ts); #oldest
tail(ts); #newest
#both function are generic and works on zoo or xts objects

# Subsetting a Time Series
ts[i]; #choose the ith observation
ts[i,j]; #choose ith observation (in row) of jth time series (in column)
ts[as.Date("yyyy-mm-dd")]; #index by date
window(ts, strat = startdate, end=enddate);#index by a range of dates

# Merging time series
merge(ts1,ts2);
# merge find the union of the dates, then arrange them into two columns,
# one for ts1, and one for ts2. If a value is missing on a merged date,
# the data is shown as NA, otherwise, it records the data at ts1's column
# from ts1's data, and at ts2's column from ts2's data

#to fill the NA's with data last observed
na.locf(merge(ts1,ts2));
#if there is no most recent data before the first NA,
#the observation will be removed

#if to keep only common dates (intersection of data)
merge(ts1,ts2,all=FALSE);

# filling or padding a time series
empty = zoo(,dates);# create an time series obejct with dates but no data
merge(ts, empty, all=TRUE);#merge the time series with the empty time series

# Lagging a Time Series
lag(ts, k);#where ts is the time series data and k is the number of observations to shift
# to shift forward, k > 0 (today's data become yesterday's data), 
# to shift backward, k < 0 (yesterday's data become today's data)

print(ibm.daily);
# 2010-01-04 2010-01-05 2010-01-06 2010-01-07 2010-01-08 
#     132.45     130.85     130.00     129.55     130.85 
print(lag(ibm.daily,+1));
# 2010-01-04 2010-01-05 2010-01-06 2010-01-07 
#     130.85     130.00     129.55     130.85
# compare
print(lag(ibm.daily, +1, na.pad = TRUE));
# 2010-01-04 2010-01-05 2010-01-06 2010-01-07 2010-01-08 
#     130.85     130.00     129.55     130.85         NA 

# Computing successive differences
diff(x);# takes backward difference (x(i+1)-x(i) for each i)

# Perfomring calculations on time series
# the zoo / xts objects are treated as if they are vectors
# can use regular arithmatic operations, sqrt, log, etc

# Computing a moving average
ma = rollmean(ts, k);# where ts is time series object and k is the time window
# to make sure using only historical data instead of interpolated data
ma = rollmean(ts, k, align = "right");

#mock example
x = rnorm(100, mean = 0, sd = 1);
ts = zoo(x);
ma = rollmean(ts, 10, align = "right");
length(ma);# = 91, 
#missing the first 9 data points, as it requires at least k=10 observations to calcualte the moving average
# to keep the same length as input
ma = rollmean(ts, 10, algin = "right", fill = NA);

# apply a function by calendar period
apply.daily(ts,f);
apply.weekly(ts,f);
apply.monthly(ts,f);
apply.quarterly(ts,f);
apply.yearly(ts,f);
#where f is the function, ts is the xts time series object
#must convert zoo object to xts objects first
apply.year(as.xts(zoo_ts_object),f);

# applying rolling function
rollapply(ts, width, f, align = "right");#where width is the time window and f is the function
# to calculate data by every n points (instead of by default, caculate with all successive data)
rollapply(ts, width, f, by = n, align = "right");

# plot auto correlation function
acf(ts);
# the spikes within the blue dotted line are insignificant, otherwise, the larger spikes are significant

# test a time series for autocorrelation
Box.test(ts);
# Box-Pierce test
# 
# data:  ts
# X-squared = 1.043, df = 1, p-value = 0.3071
# for a small sample, use Ljung-Box test
Box.test(ts, type = "Ljung-Box");
# Box-Ljung test
# 
# data:  ts
# X-squared = 1.0746, df = 1, p-value = 0.2999

# Plotting partial autocorrelation function
pacf(ts);

# Finding lagged cross correaltion between two time sereis
ccf(ts1, ts2);
#mock example
x = zoo(rnorm(50,mean = 0, sd = 1));
y = zoo(runif(50, min = -1, max = 1));
ccf(x,y);

# detrending a time series
m = lm(coredata(ts) ~ index(ts));# build a linear model base on time and data
detr = zoo(resid(m), index(ts));# store the residuals after taking out the linear model/trend

# fit ARIMA model
library(forecast);
auto.arima(x, order = c(p,d,q));
# p, d, q are ARIMA model orders
# p is the number of autoregressive coefficients
# d is the degree of differencing
# q is the number of moving average coefficients
# if we do not already know the order, let the function decide for us
m = auto.arima(x);
# Series: x 
# ARIMA(3,1,0)                    
# 
# Coefficients:
#            ar1      ar2      ar3
#        -0.9399  -0.5737  -0.2337
# s.e.    0.1385   0.1784   0.1398
# 
# sigma^2 estimated as 1.177:  log likelihood=-73.99
# AIC=155.98   AICc=156.89   BIC=163.55

# removing insignificant ARIMA coefficient
#first identify which coefficients are not significant, then use fixed argument to remove them
confint(m);
#              2.5 %      97.5 %
# ar1   -1.2112535 -0.66847573
# ar2     -0.9233805 -0.22399749
# ar3     -0.5076705  0.04026162

#ar3 may not be significant, as it contains zero
m = auto.arima(x, order = c(3,1,0), fixed = c(NA,NA,0));

# running diagnositcs on ARIMA model
m = arima(x, order=c(3,1,0), fixed = c(NA,NA,0), transform.pars=FALSE);
tsdiag(m);
# tsdiag produces 3 graphs
# Standards of good results
#1). Standardized Residuals don't show clusters of volatility
#2). The autocorrelation function (ACF) shows no significant autocorrelation between residuals
#3). The p-values for Ljung-Box statistics are all large, indiciating that the residuals are patternless, meaning
# we have extracted the information adn left behind only noise

# Making forecasts from an ARIMA model
predict(m);
# $pred
# Time Series:
#   Start = 51 
# End = 51 
# Frequency = 1 
# [1] 1.279875
# 
# $se
# Time Series:
#   Start = 51 
# End = 51 
# Frequency = 1 
# [1] 1.116962

# testing fro mean reversion
library(tseries)
adf.test(coredata(ts));
#perform Augmented Dickey-Fuller (ADF) test
#if a time series is mean reverting, it may stay at its long-run average
# it may sometimes wander off, but comes back later

# Augmented Dickey-Fuller Test
# 
# data:  coredata(ts)
# Dickey-Fuller = -5.5102, Lag order = 4, p-value = 0.01
# alternative hypothesis: stationary

#adf.test will remove mean and detrending first, if this is not desired use,
library(fUnitRoots);
adfTest(coredata(ts, type = "nc"));#where the type = "nc" argument turns detrending and centering off

# smoothing a time series
library(KernSmooth);
gridsize = length(y);
bw = dpill(t, y, gridsize = gridsize);#estimating good band size
lp = locpoly(x = t, y = y, bandwidth = bw, gridsize = gridsize);#t is time variable and y is time series object
smooth = lp$y;#lp at y is the smoothed data
# bandwdith argument specify the smooth kernel size
# gridsize: use how many data points to construct the smooth kernel and polynomial


#This concludes today's study