# Tutorial 02: 05/12/13
# packages and dataset

# get current directory
getwd();
# set current directory
setwd("Bayes");
# save variables in Workspace
save.image()
# view recent history
history();
# view result of previous command
.Last.valuee
# list current search path
search();
# load additional libraries
library(MASS); #where "MASS" is library name
require(MASS); #similar to library, but better, as it returns message regarding whether package loaded successfully
# unload libraries
detach(package:MASS);#note that it is necessarly to specify type of objects removed. In this case, package

# built-in datasets
# list available built-in dataset
data();
# load dataset Cars93 from MASS package
data(Cars93, package = "MASS");
summary(Cars93);#summarize the loaded object
head(Cars93);#returns only the first part of the object, whereas tail() returns last part of an object
# install pakcage from CRAN
install.package("some_package_name");

# run R commands from .R file or text file
source("myScript.R")
source("myScript.R",echo = TRUE); #this allows echo of the script lines when executing the command
#echo simply indicates that the program will print the line of the command first, then print the result of the command
# locate R home directory
Sys.getenv("R_HOME");






