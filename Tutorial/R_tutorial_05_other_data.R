# Tutorial 05: 05/27/13
# Data transformation and string and dates manipulation

# Splitting vectorss
library(MASS);
#splitting city MPG based on origin of the car
split(Cars93$MPG.city, Cars93$Origin);#split(x,f), x is the vector, f is the factor
#can also store as lists after splitting
g = split(Cars93$MPG.city, Cars93$Origin);

# applying a function to each element in a list
# lapplly(lst, fun) vs. sapply(lst, fun):
# lapply returns as a list, whereas sapply will simplify (as "s" stands for) and return as vector or matrix
#suppose we have the following data (randomly generated)
scores = list(S1=round(runif(36,0.6,1)*100), S2=round(runif(39,0.6,1)*100), S3=round(runif(38,0.6,1)*100),
              S4=round(runif(36,0.6,1)*100));
#find the length of each element in the list
lapply(scores,length)
# $S1
# [1] 36
# 
# $S2
# [1] 39
# 
# $S3
# [1] 38
# 
# $S4
# [1] 36
sapply(scores,length)
# S1 S2 S3 S4 
# 36 39 38 36 
#find mean and standard deviation
sapply(scores,mean)
# S1       S2       S3       S4 
# 79.52778 76.69231 78.73684 81.91667 
sapply(scores, sd)
# S1       S2       S3       S4 
# 12.64795 12.03319 12.21360 12.32738
#if the function called will return a vector, sapply will return as a matrix
sapply(scores, range)
# S1  S2  S3  S4
# [1,]  61  60  60  60
# [2,] 100 100 100 100
#if the function called will return a list, we must use lapply
tests = lapply(scores,t.test);#do a t-test, which will return a list
#but we can extract the confidence interval and other info using sapply
sapply(tests,function(t) t$conf.int)
# S1       S2       S3       S4
# [1,] 75.24833 72.79160 74.72233 77.74568
# [2,] 83.80723 80.59302 82.75135 86.08765

# applying function to every row of a matrix
#suppose we have the following data
tmp = rnorm(15,mean=0,sd=1);
long=matrix(data=tmp,nrow=3,ncol=5,dimnames = list(c("Moe","Larry","Curly"),c("trial1","trial2","trial3",
                                                                              "trial4","trial5")));
apply(long,1,mean)#calculate the mean of each row
# Moe      Larry      Curly 
# -0.3818167  1.0736131  0.7510232 
apply(long,1,range)#calculate the range of each row
# Moe      Larry      Curly
# [1,] -1.356200 -0.5512777 -0.9771284
# [2,]  1.277139  2.0276645  1.9920940

# applying function to every column of a matrix
#change the second argument of apply into 2
apply(long,2,mean)#calculate the mean of each col
# trial1     trial2     trial3     trial4     trial5 
# 0.1565687  0.9907755 -0.5284908  0.3601996  1.4256464

# supply arguments to a function applied
pred = data.frame(v1=rnorm(10,0,1),v2=rnorm(10,0,1),v3=rnorm(10,0,1));
resp = rnorm(10,0,1);
#to calculate the correlation between each variable in the data frame pred with resp
correlations=sapply(pred,cor,y=resp);#the third argument is the argument used in the function cor
# v1         v2         v3 
# 0.4058920 -0.1141383 -0.1539008 
#find best two correlations using rank function
mask = (rank(abs(correlations))<=2);
# v1    v2    v3 
# FALSE  TRUE  TRUE  -->indicating that v2 and v3 are the best 2 
best.pred = pred[,mask];#select the two variable with the best 2 correlations
lm(resp~best.pred);#do the regression model

# applying a function to a group of data (tapply(x,f,fun))
populations = round(c(runif(20,10^3,10^6)));
county = factor(c("A","B","A","C","A","C","D","E","A","B","A","C","D","D","A","C","E","E","D","C"));
#sum population by county
tapply(populations,county,sum)
# A       B       C       D       E 
# 4258380  868588 2080727 3026370 1560572 
#count number of cities in each county
tapply(populations,county,length)
# A B C D E 
# 6 2 5 4 3 

# applying a functino to groups of rows in a data frame (by(dfrm,f,fun))
trials = data.frame(sex = c("F","F","M","F","M","F","M","M"),
                    pre=c(5.931640,4.496187,6.161944,4.322465,4.153510,3.234124,5.423423,5.234234),
                    dose1=c(2,1,1,2,1,2,1,1),dose2=c(1,2,1,1,1,2,2,1),
                    post=c(3.162600,3.293989,4.446643,3.334748,4.429382,3.23423,4.23423,2.34234));
by(trials,trials$sex,summary)#summarize by the factor "sex"
# trials$sex: F
# sex        pre            dose1           dose2            post      
# F:3   Min.   :4.322   Min.   :1.000   Min.   :1.000   Min.   :3.163  
# M:0   1st Qu.:4.409   1st Qu.:1.500   1st Qu.:1.000   1st Qu.:3.228  
# Median :4.496   Median :2.000   Median :1.000   Median :3.294  
# Mean   :4.917   Mean   :1.667   Mean   :1.333   Mean   :3.264  
# 3rd Qu.:5.214   3rd Qu.:2.000   3rd Qu.:1.500   3rd Qu.:3.314  
# Max.   :5.932   Max.   :2.000   Max.   :2.000   Max.   :3.335  
# --------------------------------------------------------------------------------------------- 
#   trials$sex: M
# sex        pre            dose1       dose2        post      
# F:0   Min.   :4.154   Min.   :1   Min.   :1   Min.   :4.429  
# M:2   1st Qu.:4.656   1st Qu.:1   1st Qu.:1   1st Qu.:4.434  
# Median :5.158   Median :1   Median :1   Median :4.438  
# Mean   :5.158   Mean   :1   Mean   :1   Mean   :4.438  
# 3rd Qu.:5.660   3rd Qu.:1   3rd Qu.:1   3rd Qu.:4.442  
# Max.   :6.162   Max.   :1   Max.   :1   Max.   :4.447  

#build two models, one by male and one by female
models = by(trials,trials$sex,function(df) lm(post~pre+dose1+dose2,data=df));
# trials$sex: F
# 
# Call:
#   lm(formula = post ~ pre + dose1 + dose2, data = df)
# 
# Coefficients:
#   (Intercept)          pre        dose1        dose2  
# 3.65187     -0.08125      0.01375           NA  
# 
# --------------------------------------------------------------------------------------------- 
#   trials$sex: M
# 
# Call:
#   lm(formula = post ~ pre + dose1 + dose2, data = df)
# 
# Coefficients:
#   (Intercept)          pre        dose1        dose2  
# 4.409        0.005           NA           NA  

#find confidence interval using confint
lapply(models,confint)

# applying a function to parallel vectors or lists (mapply(fun,v1,v2,v3,v4,...))
gcd = function(a,b) {
  if (b==0) return(a)
  else return(gcd(b,a %% b))
}

mapply(gcd,c(1,2,3,4,5),c(9,3,5,8,1))
# [1] 1 1 1 4 1
#finding gcd for (1,9), then(2,3), then (3,5), etc

#----------------------------------------------------------------------------------------------------------------

# Strings and Dates

#length of string, do not use "length" as it applies to vector, not to string
nchar("Moe")
#concatenating strings
paste("Everybody","loves","stats") #adds a space by default
#[1] "Everybody loves stats"
#to get rid of space or replace with some other separator
paste("Everybody","loves","stats",sep=",")
#[1] "Everybody,loves,stats"
#to generate a combination of concatenation
stooges = c("Moe","Larry","Curly");
paste(stooges,"love","stats");
#[1] "Moe love stats"   "Larry love stats"  "Curly love stats"

#extract substrings (substr)
substr("Statistics",1,4); #starting at position 1, end at position 4
#[1] "Stat"
#substr can apply to a vector
substr(c("Moe","Larry","Curly"),1,3)
# [1] "Moe" "Lar" "Cur"

#splitting a string according to delimiter
path = "/home/mike/data/trials.csv"
strsplit(path,"/")
#[1] ""           "home"       "mike"       "data"       "trials.csv"
#strsplit also works a vector of strings

#replace substrings
#replace first instance: sub(old,new,string)
#replace all instances: gsub(old,new,string)
s = "Curly is the smart one. Curly is funny, too.";
sub("Curly", "Moe",s)
#[1] "Moe is the smart one. Curly is funny, too."
#if we usse gsub
gsub("Curly","Moe",s)
#[1] "Moe is the smart one. Moe is funny, too."
#to remove a substring, simply assign the second argument as an empty string ""

#use "print" if there are special characters

#generating all pairwise combinations of strings (m = outer(string1,string2,paste,sep=""))
locations = c("NY","LA","CHI","HOU");
treatments= c("T1","T2","T3");
outer(locations,treatments,paste,sep="-") # sep = "-" is passed to the function "paste"
# [,1]     [,2]     [,3]    
# [1,] "NY-T1"  "NY-T2"  "NY-T3" 
# [2,] "LA-T1"  "LA-T2"  "LA-T3" 
# [3,] "CHI-T1" "CHI-T2" "CHI-T3"
# [4,] "HOU-T1" "HOU-T2" "HOU-T3"

m = outer(treatments,treatments,paste,sep="-");
#some in m are repeated, to get a unique list
m[!lower.tri(m)] #this will remove the lower half triangle of the matrix
#[1] "T1-T1" "T1-T2" "T2-T2" "T1-T3" "T2-T3" "T3-T3"

# get current date 
Sys.Date()

# convert a string into a date
as.Date("2013-12-31")
#to use the format 12/31/2013
as.Date("12/31/2013",format = "%m/%d/%Y")

#convert a date into a string
format(Sys.Date())
as.character(Sys.Date())
#to convert to mm/dd/yyyy
format(Sys.Date(),format="%m/%d/%Y")
#general rules for capital and lower case format
#capital is for the full name, for instance %b is "Jan", whereas %B is "January"

# converting year, month, and day into a Date (ISOdate(year,month,day))
as.Date(ISOdate(2012,2,29))
#[1] "2012-02-29"
#1). year, month, day, can be vectors, and the result will be a vector of dates
#2). can also sepcify hour, minute, and second (ISOdatetime(year,month,day,hour,minute, second))

# getting the Julian Date (the number of days since January 1, 1970)
d = as.Date("2010-03-15")
as.integer(d)
#[1] 14683
julian(d)
# [1] 14683
# attr(,"origin")
# [1] "1970-01-01

# extracting parts of the date, using POSIXlt object
d = as.Date("2010-03-15");
p = as.POSIXlt(d);
p$mday  #day
#[1] 15
p$mon #month
#[1] 2
p$year+1900 #4 digits year
#[1] 2010

# creating a sequence of dates (seq)
s = as.Date("2012-01-01"); #start date
e = as.Date("2012-02-01"); #end date
seq(from=s, to=e, by =1);
# [1] "2012-01-01" "2012-01-02" "2012-01-03" "2012-01-04" "2012-01-05" "2012-01-06" "2012-01-07" "2012-01-08" "2012-01-09"
# [10] "2012-01-10" "2012-01-11" "2012-01-12" "2012-01-13" "2012-01-14" "2012-01-15" "2012-01-16" "2012-01-17" "2012-01-18"
# [19] "2012-01-19" "2012-01-20" "2012-01-21" "2012-01-22" "2012-01-23" "2012-01-24" "2012-01-25" "2012-01-26" "2012-01-27"
# [28] "2012-01-28" "2012-01-29" "2012-01-30" "2012-01-31" "2012-02-01"
seq(from = s, by=1, length.out = 7); #increases by 1 day, and creates 7 days
#[1] "2012-01-01" "2012-01-02" "2012-01-03" "2012-01-04" "2012-01-05" "2012-01-06" "2012-01-07"
#by default, by is in the unit of days, but we can specify by months, years, etc
seq(from = s, by="month",length.out=12) #first of the month of the year
# [1] "2012-01-01" "2012-02-01" "2012-03-01" "2012-04-01" "2012-05-01" "2012-06-01" "2012-07-01" "2012-08-01" "2012-09-01"
# [10] "2012-10-01" "2012-11-01" "2012-12-01"

# This concludes today's study.