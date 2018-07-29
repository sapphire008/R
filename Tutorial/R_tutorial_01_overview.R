# Tutorial 01: 05/05/2013
# Overview

# declaring values
x = 3;
y <- 4;
z = 5;
#list all variables
ls()
#remove one variable or a list of variables from workspace
rm(x);
rm(y,z);
#remove all variables from workspace
rm(list = ls());#do not use this at all!!
#remove all variables except y
rm(list=(ls()[ls()!="y"]))

# Create a vector
z1 = c(1,2,3,4,5,6,7,8,9,0);
z2 = c("Everyone","loves","stats");
z3 = c(T,F,F,T);#can also be c(TRUE,FALSE,FALSE,TRUE), but much longer to write

#concatenate vectors of same data type, cannot concatenate different data types
z11 = c(2,4,3,1);
z_num = c(z1,z11);
print(z_num);
#[1] 1 2 3 4 5 6 7 8 9 0 2 4 3 1

z_char = c(z1,z2);
print(z_char);
#[1] "1" "2  "3"  "4"  "5"  "6"  "7"  "8"  "9"   "0"   "Everyone" "loves"   
#[13] "stats"

#check type of data a variable is
mode(z_char)
#[1] "character"
mode(z_num)
#[1] "numeric"

# Basic statistical functions
X = c(1,2,3,4,5);
Y = c(1,3,4,5,6);
mean(X);
median(X);
sd(X);
var(X);
cor(X,Y);
cov(X,Y);

# note that NA (not available) is different from NaN (not a number). Any functions that apply to a data with
# NA will return NA
X_hat = c(1,2,3,4,5,NA);
mean(X_hat);
#[1] NA
# to circumvent this, use
mean(X_hat,na.rm = TRUE);
#[1] 3
# if data is a matrix, then, each column is treated as a variable, and mean and sd are taken within each column
# across all the rows

# var, cor, and cov also treats each column as a variable, but returns a covariance matrix instead

# median does not understand data frames, and does not treat each column as a variable


# Create a sequence
a = 1:5;#1 2 3 4 5
b = seq(from = 1, to = 5, by = 2);
# or simply
b = seq(1,5,2);
# to repeat a sequence
c = rep(1,times = 5);
# or simply
c = rep(1,5);
#[1] 1 1 1 1 1
# to divide a range [x,y] into n arrays
d = seq(3,10,length.out = 5)#create a sequence between 3 and 10, including 3 and 10, with length 5
#[1]  3.00  4.75  6.50  8.25 10.00

# Compare vectors
n = 3;
m = 4;
n<m;
#[1] TRUE
n>m;
#[1] FALSE
k = c(1,2,3,4,5,6,7);
n < k;
#[1] FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
k==n;
#[1] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE
l = c(1,1,1,1);
k>l;
#Warning message:
#  In k > l : longer object length is not a multiple of shorter object length
#however,
a=c(4,3,2,1);
b = c(1,2,3,4,5,6,7,8)
a>b
#[1]  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
# That is, a[1]>b[1],a[1]>b[2],a[2]>b[3],a[2]>b[4],...etc
#Use [] to select a value from a vector
a[1:2]
#[1] 4 3
a[c(1,3,5)]
#[1]  4  2 NA -->for values exceeds index, return NA
a[-1]
#[1] 3 2 1 -->ignore index 1, and return the rest; if a[-2], then ignor index 2, and return the rest
#logical indexing
a[a<3]
#[1] 2 1

# Vector arithetic
#element by element operations between vectors
v = c(6,7,8,9,10);
w = c(1,2,3,4,5);
v+w
#[1]  7  9 11 13 15
v-w
#[1] 5 5 5 5 5
v*w
#[1]  6 14 24 36 50
v/w
#[1] 6.000000 3.500000 2.666667 2.250000 2.000000
v^w[1]      6     49    512   6561 100000
#if one operand is a scalr and the other operand is a vector, then
#operation is performed by the scalr on each element of the vector
w-2
#[1] -1  0  1  2  3
#functions can also be applied element by element
sin(v)
#[1] -0.2794155  0.6569866  0.9893582  0.4121185 -0.5440211
sqrt(w)
#[1] 1.000000 1.414214 1.732051 2.000000 2.236068
log(v*w)
#[1] 1.791759 2.639057 3.178054 3.583519 3.912023

# Divisions in R
5%%2 # modulo operator
#[1] 1
5%/%2 #integer division
#[1] 2
A = matrix(seq(1,6),2);
#       [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6
B = matrix(seq(8,10),3);
#       [,1]
# [1,]    8
# [2,]    9
# [3,]   10
A%*%B #matrix multiplication
#       [,1]
# [1,]   85
# [2,]  112

c(1,2,7)%in%c(1,2,3,4,5)# return true if each element of the left operand occurs in the right operand
#[1] TRUE TRUE FALSE


# Defining functions
# format: 
# function(param_1,param_2) 
#     {
#     expr_1
#     expr_2
#     ...
#     }

#define a function that returns coefficient of variation
cv = function(x)
  {
    sd(x)/mean(x)  
  }

cv(1:10)
#[1] 0.5504819
# or we can use lapply, which is a function that applies specified function to
# each element of an array. Think cellfun in MATLAB
lst = list(seq(1,10),c(1,2,5,3,1));
lapply(lst,cv);
# [[1]]
# [1] 0.5504819
# 
# [[2]]
# [1] 0.6972167  
  

# This concludes today's study.