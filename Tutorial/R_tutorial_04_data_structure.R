# Tutorial 04: 05/19/13
# Data Structures

# Vectors (c)
#must have uniform data type (mode)
#vectors have "names" property
v = c(10,20,30);
names(v) = c("Moe","Larry","Curly");#assigning names to a vector
print(v);
#> Moe Larry Curly 
#> 10    20    30 
v["Larry"];
#> Larry 
#> 20 

# Appending and inserting into a vector
v = c(1,2,3);
v1 = c(4,5,6);
v = c(v,v1);
print(v);
#> [1] 1 2 3 4 5 6
# or 
v[length(v)+1] = 7;
print(v);
#> [1] 1 2 3 4 5 6 7
# Can also use "append" function
v = append(v,8,after = length(v));
print(v);
#> [1] 1 2 3 4 5 6 7 8
# "append" function can also be used to insert elements
v = append(v,0,after = 3);
print(v);
#> [1] 1 2 3 0 4 5 6 7 8

# Recycling Rule
# suppose there are two vectors of unequal length
a = 1:10;
b = 1:7;
# then, do a+b operation
print(a+b);
#> [1]  2  4  6  8 10  7  9 11 13 15
#therefore, it appears that for the first 5 elements of the result obtained:
# a(n) + b(n)
# for the last 5 elements of the result obtained
# a(length(b)+n) + b(n)
# To make this legal, the length of the longer vector must be the multiple of the length of the shorter vector
# otherwise,
# Warning message:
#   In a + b : longer object length is not a multiple of shorter object length

# Stack vectors
#suppose we have three vectors, freshmen, sophomores, juniors
comb = stack(list(fresh = freshmen, soph = sophomores, jrs = juniors));#stack three vectors
#we will get something like the following
#     values    ind
# 1   0.60    fresh
# 2   0.32    fresh
# 3   0.24    soph
# 4   0.52    soph
# 5   0.73    jrs
# 6   0.75    jrs
# 7   0.72    jrs

# Scalars
# a single element vector; R treat them the same as a vector

#--------------------------------------------------------------------------------------------------
# Lists (list)
#can contain data of different types/modes
L = list("Moe","Larry","Curly");
#must use double bracket to index elements inside the list
#note the difference
A=L[[2]];
print(A);
#> [1] "Larry"
mode(A);
#> [1] "character"
#whereas, in comparison
B=L[2];
print(B);
#> [[1]]
#< [1] "Larry"
mode(B);
#> [1] "list"
#list also have "names" property
names(L) = c("number1","number2","number3");
L[["number1"]];#again, use double bracket to index
# or equivalently,
L$number1 #no need for the quotes and double brackets
#> [1] "Moe"
# Create a list
lst = list(0.5,0.8,0.3);
lst = list();#create an empty list
lst = list(mid = 0.5,right = 0.84, far.right=0.977);#list with names
# Remove elements from a list: index the elements and assign NULL to the elements
lst[["mid"]] = NULL; #removes the element named "mid"
# Convert/flatten list to a vector (unlist)
vec =unlist(lst);
# remove NULL elements from a list
lst = list("A","B",NULL,"C");
lst[sapply(lst,is.null)] = NULL; #find the index of NULL elements and remove it by assigning it to NULL
# remove elements using condition
lst[lst<0]= NULL;# remove all elements that are less than 0

# Mode vs. Class
# each data type belongs to a data mode (numeric, character, list, function, etc)
# each object belongs to a class
d = as.Date("2010-03-15");
mode(d);
#> [1] "numeric"
class(d);
#> [1] "Date"

#--------------------------------------------------------------------------------------------------
# Matrix
A = 1:6;
dim(A);
#> NULL  # a vector has Null dimension
dim(A) = c(2,3); #assign a dimension to the vector to transform a vector into a 2x3 matrix
print(A);
#> [,1] [,2] [,3]
#> [1,]    1    3    5
#> [2,]    2    4    6
# this can also be done on a list. This means that one matrix can take multiple data mode
# make sure the product of the transform dimension is equal to the length of the vector/list
# notice that the index goes by column, meaning that:
# along the columns of the matrix can the original vector be recovered
# A simpler and better way
thedata = c(1.1,1.2,1.3,1.4,1.5,1.6);
mat = matrix(thedata,2,3);
#for preallocation purpose, we can create an all 0, all 1, or all NA matrix
mat = matrix(0,10,10);#a 10x10 all 0 matrix
# to index along the row instead of by the column (default); set dimenson names as "X" and "Y"
mat = matrix(thedata,nrow = 2,ncol = 3,byrow = TRUE,dimnames = list(c("X1","X2"),c("Y1","Y2","Y3")));
#can also use "colnames" and "rownames" to assign names to a matrix

# Matrix operations
A = matrix(c(1,2,3,4,5,6),nrow = 3, ncol=2);
B = matrix(c(0.1,0.2,0.3,0.4,0.5,0.6),nrow = 2, ncol = 3);
t(A);#transpose of matrix
solve(A);#inverse of matrix
A%*%B; #matrix multiplication of A and B // distinguish from A*B which is element by element multiplication
diag(n);#initiate an n x n identify matrix

# Matrix indexing
vec = A[1,];#select first row and all columns from matrix A; returns as a vector, mode: numeric
vec = B[,2];#select 2nd column and all rows from matrix B; returns as a vector, mode: numeric
# to retain the result as a matrix after selecting a row
rowvec = A[1,,drop=FALSE];#select first row; notice the double comma

#--------------------------------------------------------------------------------------------------
# Factors
# Treated as categorical data, and are used for grouping
# Each value inside a vector is uniquely treated, whereas value inside a factor is not differentiated
f = factor(c("Win","Win","Lose","Tie","Win","Lose"));
print(f);
#> [1] Win  Win  Lose Tie  Win  Lose   #not quoted: becasue they are not strings but levels
#> Levels: Lose Tie Win
f = factor(c("Mon","Tue","Thur","Tus","Wed","Mon"));
print(f);
#> [1] Mon  Tue  Thur Wed  Mon 
#> Levels: Mon Thur Tue Tues Wed
# Seems like this misses Friday as a level. To specify level in the second argument of "factor":
f = factor(c("Mon","Tue","Thur","Tue","Wed","Mon"),c("Mon","Tue","Wed","Thur","Fri"));
print(f);
#[1] Mon  Tue  Thur Tue  Wed  Mon 
# Levels: Mon Tue Wed Thur Fri
# make sure spell correctly. If one of the element in the first argument does not exist in the
# level list specified in the second argument, the output of that element will be NA
f = factor(c("Mon","Tue","Thur","Tues","Wed","Mon"),c("Mon","Tue","Wed","Thur","Fri"));
print(f);
# [1] Mon  Tue  Thur <NA> Wed  Mon  -->Tues does not exist in the level vector
# Levels: Mon Tue Wed Thur Fri

#--------------------------------------------------------------------------------------------------
# Data Frames
# They are table-like data structures, with rows and columns;
# They are not matrices, but lists containing vectors and factors (numeric and character), stored along each columns
# All elements in the list must have the same length
# Each column (for each vector or factor) must have a name

#assume each variable is given separately (column-wise data frame generation)
# each p# is an variable, and each element of p is an observation
p1 = rnorm(10,0,1);
p2 = runif(10,0,1);
p3 = rnorm(10,0,1);
p4 = runif(10,0,1);
# initiate a data frame
dfrm = data.frame(p1,p2,p3,p4);
# if data is stored inside a list
lst = list(x=p1,y=p2,z=p3,w=p4);
dfrm = as.data.frame(lst);#convert list to data frame

#assume each observation is given separately (row-wise data frame generation)
# each k# is an observation, which contains all variables
k1 = rnorm(4,0,1);
k2 = rnorm(4,0,1);
k3 = rnorm(4,0,1);
k4 = rnorm(4,0,1);
k5 = rnorm(4,0,1);
k6 = rnorm(4,0,1);
k7 = rnorm(4,0,1);
k8 = rnorm(4,0,1);
k9 = rnorm(4,0,1);
k10 = rnorm(4,0,1);
k11 = rnorm(4,0,1);
k12 = rnorm(4,0,1);
#combine all the data
rbinded_data = rbind(k1,k2,k3,k4,k5,k6,k7,k8,k9,k10,k11,k12);
# if data is a list
k = list();
for(n in 1:12)
{ 
  k[[n]] = rnorm(4,0,1);
}
rbinded_data = do.call(rbind,k);#combine data from the list

# append rows to the data frame
newRow = data.frame(V1=0.2,V2=03,V3=0.4,V4=0.5);
rbinded_data = rbind(rbinded_data,newRow);
# append columns to the data frame
newCol = as.data.frame(c(runif(13,0,1)));
rbinded_data = cbind(rbinded_data,newCol,deparse.level=2);
colnames(rbinded_data)[5] = "V5"; #change the name of the 5th column

# preallocating data frame
N = 10000;
dfrm = data.frame(dosage = numeric(N),lab = factor(character(N),levels = c("NJ","IL","CA")),response = numeric(N));

# selecting data from data frame
dfrm = data.frame(dosage = runif(5,0,1),lab = factor(c("NJ","IL","CA","CA","IL")),response = rnorm(5,0,1));
dfrm[[n]]; #returns a single column of vector/factor
dfrm[n]; #returns a single column of data frame
dfrm[c(n1,n2,n3...)]; #returns data from consisting columns indexed by n1, n2, n3, etc.
dfrm[,n]; #return nth column, all the rows; if n is a vector, with length(n)>1, then returns data frame
dfrm[n,]; #return nth row, all the columns; if n is a vector, with length(n)>1, then returns data frame
#to avoid ambiguity in the above two cases, use drop = FALSE to make sure it returns as a data frame
dfrm[["dosage"]]; #returns the column of corresponding name, as a vector/factor
dfrm$dosage;
dfrm["dosage"]; #both returns data frame of corresponding name
#The above three index by name is similar to the index by number
subset(dfrm,subset=(response>0), select = c(dosage,reponse)); #returns corresponding columns, and within these columns, select response >0 rows

# edit data
tmp = edit(dfrm);#store into another variable without modifying the original variable
dfrm = tmp;#overwrite only if satisfied with the editing

fix(dfrm); #overwrite regardless, modifies the original variable directly

# remove NA from data frame
clean = na.omit(dfrm);# remove any rows that contain NA

# excluding columns by name
subset(dfrm, select = -response); #note the "-" in front of "response", will exclude response column and keep the rest

# combining two data frames / concatenation
all.cols = cbind(dfm1,dfm2);#combine all columns
all.rows = rbind(dfm1,dfm2);#combine all rows /stacking two data frames

# merge data frames by common column name
merged_dfrm = merge(dfm1,dfm2,by = "col_name");#will discard rows that are not common in both data frames

#expose data frames and refer to vectors according to its column names as if simple variables
ZZ=with(dfrm,response/dosage); #calculate a ratio between two variables without referring to dfrm twice
attach(dfrm); #will make variables (named after column names) available to refer to, even though they are not in the workspace
# > response
# [1]  1.6913555  0.1907755 -0.1838043  0.9154252 -0.1164118
# note that if reassigning values to the exposed data, it will not change the original data frame

# converting one atomic value into another
#atomic data type: character, complex, double, integer, logical
N = "1000.1";
mode(N)
#> [1] "character"
N=as.numeric(N);
mode(N);
#> [1] "numeric"
K = "foo";
K = as.complex(K);
#> Warning message:
#>   NAs introduced by coercion 
mode(K);
#> [1] "complex"
#other conversions
as.logical(N);
as.character(N);
as.integer(N);
as.double(N); #similar to as.numeric(N)

# converting one structured data type into another
as.data.frame(x);
as.list(x);
as.matrix(x);
as.vector(x);
# conversion can be tricky. Trial and error, or refer to the book which this tutorial generated from

# This concludes today's study.