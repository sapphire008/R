# Tutorial 06: 06/02/13
# Basic Statistics

# counting
# calculate the number of combinations of n items taken k at a time: choose(n,k)
choose(5,3)
#[1] 10
# generate all combinations: combn(items, k)
combn(1:5,3); # all 3 number combinations from 1 to 5
# [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
# [1,]    1    1    1    1    1    1    2    2    2     3
# [2,]    2    2    2    3    3    4    3    3    4     4
# [3,]    3    4    5    4    5    5    4    5    5     5

# generating random numbers
#runif(#number of random numbers n, min =0, max =1)
runif(1); #uniformlly distributed random number
#rnorm(#number of random numbers n, mean=0, sd =1)
rnorm(1); #normally distributed random number
#generating reproducible random numbers
set.seed(777); #"777" only represents a state of the random number generator, use this number to retrieve the random number
runif(5);
#[1] 0.6878574 0.4921926 0.3451156 0.9950499 0.6952672
set.seed(777)
runif(5);
#[1] 0.6878574 0.4921926 0.3451156 0.9950499 0.6952672
#as long as the seed number is the same, the random number will return the same value

# generating random samples: sample(vec, n)
my_data = 1995:2012;
sample(my_data,5); #randomly select 5 samples from my_data, without replacement
#[1] 1995 2000 1997 2009 1998
sample(my_data,size=5, replace =TRUE); #sample with replacement
#[1] 2008 2006 2005 2005 2010
# bootstrapping median example:
x = runif(300,min=1.34, max = 2.38);
medians = numeric(1000);#place holding
for (i in 1:1000){
  medians[i] = median(sample(x,replace = TRUE));
}
#then, estimate confidence interval of the median
CI = quantile(medians, c(0.025, 0.975));
cat("95% confidence interval is (", CI, ")\n");
#95% confidence interval is ( 1.757926 1.907547 )


# generating random sequence: sample(set, n, replace = TRUE)
sample(c("H","T"),10, replace = T); #coin toss 10 times
#[1] "T" "T" "H" "T" "T" "H" "H" "H" "H" "H"
#sampling with different probabilities
sample(c(T, F), 20, replace = T, prob=c(0.2, 0.8)); #sample with P("T") = 0.2, and P("F") = 0.8
#[1] FALSE FALSE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE  TRUE FALSE  
#              TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
# The above can also use rbinom(n, size, prob), "n" samples at a time/trial, "size" times/trials, with P(x=success) = 0.8, P(x=0) = 0.2
A=rbinom(10000, 1, 0.8)

#shuffling a vector: sample(v)
v = 1:10;
sample(v);
#[1]  1  5  3 10  2  4  6  8  7  9
#which is equivalent to sample(v, size = length(v), replace = FALSE)

# Calculating probabilities for discrete distributions
# all built-in functions with "d" prefixed are density functions
# all built-in functions with "p" prefixed are distribution functions

#given a binomial distrubition, what is the probability of getting 7 successful trials, over 10 trials, with  single trial probability 0.5
dbinom(7, size = 10, prob = 0.5);
#[1] 0.1171875
#given the above conditions, what is the culmulative probability (P(x<=7))
pbinom(7, size = 10, prob = 0.5);
#[1] 0.9453125
#get survival function (P(X>7))
pbinom(7, size = 10, prob = 0.5, lower.tail = FALSE);
#[1] 0.0546875
#to find interval probability (P(3<x<=7))
diff(pbinom(c(3,7), size = 10, prob = 0.5))
#[1] 0.7734375

# Calculating probabilities for continuous distributions

#given mean height is 70 in, and sd of height is 3, what is the probability of a man shorter than 66 in
pnorm(66, mean=70, sd=3)
#[1] 0.09121122
#find probability of a number that is less than 20 in an exponential function with mean of 40
pexp(20, rate = 1/40);
#[1] 0.3934693
#find probability of a number that is greater than 50 in an exponential function with mean of 40
pexp(50, rate = 1/40, lower.tail = FALSE);
#[1] 0.2865048
#calculate the probability in between 20 and 50 given an exponential function with mean of 40
diff(pexp(c(20,50), rate = 1/40))
#[1] 0.3200259

# Converting probabilities to quantiles
# built-in quantile functions have "q" prefixed
#What is the quantile for p = 0.05, given a normal distribution mean 100, and sd 15
qnorm(0.05, mean = 100, sd = 15)
#[1] 75.3272  #any number below this returned number has less 5% probability

# Plot density function
#simple plots
x = seq(from = -3, to = +3, length.out = 100);
plot(x, dnorm(x));

#plot 2x2 density plots
x = seq(from = 0, to = 6, length.out = 100);
ylim=c(0, 0.6);

par(mfrow=c(2,2)); #create a 2x2 plotting area
plot(x,dunif(x,min=2,max=4),main="Uniform",
     type='l', ylim=ylim);#plot uniform density
plot(x,dnorm(x,mean=3,sd=1),main="Normal",
     type='l',ylim=ylim);#plot normal density
plot(x,dexp(x,rate=1/2),main = "Exponential",
     type='l',ylim=ylim);#plot exponential density
plot(x,dgamma(x,shape=2,rate=1),main="Gamma",
     type='l',ylim=ylim);#plot gamma density

#plot with shade
x = seq(from=-3, to=+3, length.out = 100);
y = dnorm(x);
plot(x,y,main="Standard Normal Distribution", type="l",
     ylab="Density",xlab="Quantile");
abline(h=0);#adds a stright horizontal line (horizontal indicated by h)
#now, define a region of interest, say 1<=x<=2
region.x = x[1<=x & x<=2];
region.y = y[1<=x & x<=2];
#add intial and end segments which will enclose the region of interest
region.x = c(region.x[1], region.x, tail(region.x,1));
region.y = c(0,            region.y, 0);
#call polygon to plot the boundary
polygon(region.x,region.y, density = 10);
#or to fill with color
polygon(region.x,region.y,density =-1, col = "red");

# This concludes today's study.