# Tutorial 08: 06/23/13
# Graphics

# scatter plot (plot())
x = rnorm(50, mean = 0, sd = 1);
y = rnorm(50, mean = 1, sd = 2);
plot(x,y, main = "my first plot", sub = "This is my plot", xlab = "x", ylab = "y");
#inhibit annotation, then add them
plot(x, ann = FALSE);
title(main = "The Title", xlab = "X-label", ylab = "y-label");
#adding grid: call grid first, then call lower level function to draw data, preventing grid overlay on the data
plot(x,y,type = "n"); #initialize the graphics frame without displaying the data
grid();#draw grid
points(x,y);#draw graphics overlay the grid

# Scatter plot of multiple groups (plot(x, y, pch = as.integer(f)))
with(iris,plot(Petal.Length,Petal.Width, pch=as.integer(Species)));

# Adding a legend (legend(x, y, labels, pch/lty/lwd/col = c(x1,x2,x3,...))
legend(1.5,2.4,c("setosa","versicolor","virginica"), pch = 1:3);
#"x" and "y" specifies the position of the legend, "labels" specify a vector of label names to use,
#and "pch" specifies which point types will the "labels" be applied to
#in addition, "lty" will be specifying which line types will the "labels" be applied to
#"lwd" will specify which line according to width will the "labels" be applied to
#"col" will specify which color types  will the "labels" be applied to

# Plotting a regression line of a scatter plot
# general format
# m = lm(y ~ x);
# plot(y ~ x);
# abline(m);
library(faraway);
data(strongx);
m = lm(crossx ~ energy, data = strongx);
plot(crossx ~ energy, data = strongx);
abline(m);

# Scatter plot for all pairs of variables (plot(dfrm))
# "iris" is a dataframe
plot(iris[,1:4]);#plot the first four numerical columns of the iris dataframe

# Scatter plot for each factor (coplot(y ~ x | f)), a.k.a. Conditional plot
data(Cars93, package = "MASS");#import data from "MASS" package
coplot(Horsepower ~ MPG.city | Origin, data = Cars93);#plot horsepower vs. MPG.city, given different origins

# Creating a bar chart (barplot(c(height1,height2,...)))
heights = tapply(airquality$Temp, airquality$Month, mean);#take the mean of temprature by each month
barplot(heights, 
        main = "Mean Temp. by Month",
        names.arg = c("May", "Jun", "Jul", "Aug", "Sep"), 
        ylab = "Temp (deg. F)");

# Adding confidence interval to bar chart
#library(gplots);
#barplot2(x, plot.ci = TRUE, ci.l = lower, ci.u = upper);
library(gplots);
attach(airquality);
heights = tapply(Temp, Month, mean); #take mean of temperature over each months
lower = tapply(Temp, Month, function(v) t.test(v)$conf.int[1]);#for each month, calculate lower bound of CI
upper = tapply(Temp, Month, function(v) t.test(v)$conf.int[2]);#for each month, calculate upper bound of CI
barplot2(heights, plot.ci=TRUE, ci.l = lower, ci.u = upper,
         main = "Mean Temp. by Month",
         names.arg = c("May", "Jun", "Jul", "Aug", "Sep"), 
         ylab = "Temp (deg. F)");#plot barplot, with CI

# Coloring bar chart (barplot(heights, col = colors))
barplot(c(3,5,4), col = c("red", "white", "blue"));
#shaing the bar with color according to their heights
rel.hts = rank(heights)/length(heights)
grays = gray(1-rel.hts);
barplot(heights, 
        col = grays,
        ylim = c(50,90),#set y-axis limit
        xpd = FALSE,#make sure the bars do not go outside of the plot region
        main = "Mean Temp. By Month", 
        names.arg = c("May", "Jun", "Jul", "Aug", "Sep"), 
        ylab = "Temp (deg. F)");


# Plotting a line from (x,y) points (plot(x, y, type = "l"))
plot(pressure, type="l");

# change line type of the line (x,y) line plot
plot(pressure, type = "l", lty = "dashed");
#other types of "lty" include, "solid" (default), "dotted", "longdash", etc.

# Plotting Multiple datasets in the same plot
# use plot or curve to initialize canvas;
# use xlim, ylim to size the canvas
# use lines or points to add more datasets to the plot

# adding vertical or horizontal lines, such as axes (abline)
abline(v = 0);#draw a vertical line
abline(h = 5);#draw a horizontal line
#use horizontal lines to show mean, first and second standard deviation of a dataset
samp = rnorm(100,mean = 0, sd = 2);
m = mean(samp);
stdevs = m + c(-2, -1, +1, +2)*sd(samp);
plot(samp);
abline(h=m, lty = "solid");
abline(h=stdevs, lty = "dotted");

# Creating a box plot (boxplot(x))
x = runif(500,min = -1, max = 1);
boxplot(x);

# Creating a box plot for each factor (boxplot(x ~ f))
data(UScereal, package = "MASS");
boxplot(sugars ~ shelf, data = UScereal,
        main = "Sugar Content by Shelf",
        xlab = "Shelf", ylab = "Sugar (grams per portion");

# Creating a histogram (hist(x))
data(Cars93, package = "MASS");
hist(Cars93$MPG.city,20,     #break into 20 bins
     main = "City MPG (1993)", xlab = "MPG");

# Adding density estimation to a histogram
samp = rrgamma(500,2,2);
hist(samp, 20, prob=TRUE);
lines(density(samp));#joining the points after calculating the density at each bin of samp

# Histogram of discrete data (plot(table(x), type = "h"))
# use table function to count occurence in x, then create histogram like plots
x=c(0,1,9,8,9,7,4,5,3,2,6,7,2,1,0,6,5,4,3,7,8,9,2,1);
plot(table(x), type = "h",lwd = 5, ylab = "Freq");

# Creating a normal quantile-quantile (Q-Q) plot (qqnorm(x) and qqline(x))
# This is a good way to visually check if the data is normally distributed
# if it is normally distributed, then it will fall exactly on the line
data(Cars93, package = "MASS");
qqnorm(Cars93$Price, main = "Q-Q Plot: Price");
qqline(Cars93$Price);
#plot in log scale
data(Cars93, package = "MASS");
qqnorm(log(Cars93$Price), main = "Q-Q Plot: log(Price)");
qqline(log(Cars93$Price));

# Creating other Q-Q plots
y = rexp(1000, rate = 1/10);
plot(qexp(ppoints(y), rate = 1/10), sort(y), 
     main = "Q-Q Plot", xlab = "Theoretical Quantile", ylab = "Sample Quantiles");
abline(a=0,b=1);
#if the data fits the underlying distribution specified in the "qexp" argument (if guessed normal distribution,
#then use qnorm; if guessed uniform distribution, use qunif), the data will fall exactly on the line

# Plot a variable in multiple colors (plot(x, col = colors))
x=rnorm(100,mean=0,sd=1);
colors = ifelse(x>=0, "black", "gray");#much like Excel function if
plot(x, type = "h", lwd = 5, col = colors);#plot positive values in black, and negative values in gray

# Graph a function (curve(func, from, to))
curve(sin, -3, +3);#plot sine function from -3 to +3
curve(dnorm, -3.5, +3.5,
      main = "Std. Normal Density");#normal density function
f = function(x) exp(-abs(x))*sin(2*pi*x);#dampened sine wave
curve(f,-5,+5, main = "Dampened Sine Wave");

# Pausing between plots
par(ask = TRUE);#turn pausing on
par(ask = FALSE);#turn pausing off

# Subplots: multiple plots on one canvas (par(mfrow=c(N,M)))
par(mfrow=c(2,2));#graphics will be filled row by row, if want to fill by col, use par(mfcol=c(N,M))
#see layfunction and split.screen with more elegant design
Quantile = seq(from=0, to=1, length.out = 30);
plot(Quantile, dbeta(Quantile, 2,4), type = "l", main = "First");
plot(Quantile, dbeta(Quantile, 4,2), type = "l", main = "Second");
plot(Quantile, dbeta(Quantile, 1,1), type = "l", main = "Third");
plot(Quantile, dbeta(Quantile, 0.5,0.5), type = "l", main = "Fourth");

# Open additional plot canvas
win.graph(); #opens a new graphic window
win.graph(width = 7.0, height = 5.5); #open a new window, specifying dimensions
dev.set(); #returns the current active graphic window
dev.set(which);#set active graphic window to "which"
dev.off(); #close the graph

# Saving plot to a file (savePlot(filename = "filename.ext", type = "type"))
savePlot(filename = "my_histogram.tiff", type = "tiff");
#use help(Devices) to determine the types can be saved

# Change global graphical parameters (par())
par(lwd=2); #change line width to 2
par("lty"); #list default value of "lty" argument
par("bg"); #similar to the previous, list default background color
# the change will affect all the plots, use with caution!!!

# This concludes today's study.