# Tutorial 03: 05/13/13
# Data and file navigation

# Data Entry
# declare empty data frame
scores = data.frame();
# evoke editor, overwrite with edited data
score = edit(score);
# together as:
point = data.frame(label = c("Low", "Mid","High"),
                   lbound = c(0,     0.67, 1.64),
                   ubound = c(0.674, 1.64, 2.33));
#note that each variable, "label", lbund", and "ubound" will be column titles

# Printing
print(pi, digits = 4);# print pi with 4 digits. Note formatting with apply to all elements of a vector
#>[1] 3.142
# concatenation
cat(format(pi,digits = 4), "\n");# must use "format" to specify formatting
#>3.142 
# direct output to a file
cat("The answer is ", answer, "\n", file = "filename");#note "print" cannot redirect output like "cat"
#to use with "print" like functions without abilities to redirect output
sink("filename");# this opens the file handle
#...some other statements
sink(); #this closes the file handle, subsequent output will be written to the console
#to be less tedious, open the file and write contents; close upon finishing writing
FID = file("analysisReport.out","w"); #open a file with writing "w" permission; this is called {connection}
cat(data, file = FID);#write the first variable to the opened file (implied append = TRUE)
cat(result, file = FID);# write the second variable to the opened file (implied append = TRUE)
cat(conclusion, file = FID);# write the last variable to the opened file (implied append = TRUE)
close(FID);# close file

# list files
list.files();#list current working directory
list.files(recursive = T); #list files recursively (under subdirectories)
list.files(all.files = TRUE);#list all files, including hidden files and directories

# cannot open file error in Windows environment
# Windows uses forward slashes, whereas everyone else uses backward slashes.
# To solve this problem, use either double forward slashes or backward slashes.

# Reading and Writing files
# 1). read fixed width files
#suppose there is a fixed_width_file.txt file with 4 columns, in which first 2 columns have 10 characters,
#third and fourth columns have 4 characters, with 1 character space between them.
#to read the file,
records = read.fwf("fixed_width_file.txt", widths = c(10, 10, 4, -1, 4));
#-1 indicates that the character at this place should be ignored
# to overwrite column names when reading the file, supply argument "col.names"
records = read.fwf("fixed_width_file.txt", widths = c(10, 10, 4, -1, 4), 
                   col.names = c("Last","First","Born","Died"));

# 2). read tabular data
#tabular data means each column is separated by a delimiter (white space or comma),
#with lines separated by line character. Must row/record must contain same number of columns/fields
records = read.table("statisticians.txt",sep = ",");#input can also be an URL; "sep" arguments supplies delimiter to read
#factors are categorical data. "read" function automatically interpret non-numeric data as categorical data/factors
#to prevent this
records = read.table("statisticians.txt",stringsAsFactor = FALSE);
#in case there is NA in the data, we can specify what character we use to indicate NA, instead using NA
records = read.table("statisticians.txt",na.strings = ".");#use "." to indicate NA
#if the data contains a header line, use
records = read.table("statisticians.txt",header = TRUE);
#Also, read.table ignores any lines with pond(#) signs, treating them as comments

# 3). read .csv file
records = read.csv("filename.csv");#the read.csv input can also be an URL
#by defaults, read.csv takes the first line as header. To turn this OFF
records = read.csv("filename.csv", header = FALSE);
#to make read.csv interpret non-numeric data as non-factor
records = read.csv("filename.csv", as.is = TRUE);#this is different from read.table
#to read comment lines
records = read.csv("filename.csv", comment.char = "");#can also be used to change comment characters


# 4). write .csv file
write.csv(x, file = "filename.csv", row.name = FALSE);
#writes a csv file with "filename.csv", with delimiter ",", turning off row names


# 5). read from THML tables
library(XML); #add XML library
url = "http://www.example.com/table.html";#specify table URL
records = readHTMLTable(url);#read the table from the specified URL
#if there are multiple tables in the URL, specify which table to read using "which"
records = readHTMLTable(url,which = 3);

# 6). read a line
line = readLines("input.txt", n =10);#read up until 10th line, then stop; if not specified, will read all lines

# 7). read a specific type of data
records = scan("myfile.txt",what = numeric(0)); #by default, the second argument is double()
#can also use scan to read a repeated sequence of types
records = scan("myfile.txt", what = list(character(0), numeric(0), numeric(0)));
#the above will read data with character-numeric-numeric patterns
#it is also possible to assign names to the read variables
records = scan("myfile.txt", what = list(name = character(0), high = numeric(0), low= numeric(0)));
#the above will create three varaibles "name", "high", "low" after reading from the file

# Save and Load Data Files
save(mydata, file = "myData.RData");#save data into a file named "myData.RData"
load("myData.RData"); #load data from file "myData.RData" to workspace
#to save as ASCII format
dput(myData,file = "myData.txt");
#or
dump("myData", file= "myData.txt");#note that the double quote around "myData" is necessary for "dump"

#This concludes today's study.