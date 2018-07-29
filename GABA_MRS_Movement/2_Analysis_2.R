# Analysis 2
# analyze by clusters

library(lme4)

data_source = "C:/Users/Edward/Documents/Ubuntu/Analysis/data/movement/movement_data_v1_c.csv"
data = read.table(data_source,header = TRUE, sep = ",", na.string = "NA",  dec = ".", strip.white = TRUE)
colnames(data) <- gsub("_",".",colnames(data))


data.LL = subset(data,subset=Movement.Group==4)
data.HL = subset(data,subset=Movement.Group==3)
data.LH = subset(data,subset=Movement.Group==2)
data.HH = subset(data,subset=Movement.Group==1)

# Aggregate to subject level data
data.subj = aggregate(. ~ Subject , data = subset(data, select=c(Subject, GABA.Cr, Glx.Cr, NAA.Cr, D, S, Movement.Group)), subset=Movement.Group==4 || Movement.Group==3, FUN=mean, na.rm=T)
data.LL.subj <- aggregate(. ~ Subject , data = subset(data.LL,select=c(Subject, GABA.Cr, Glx.Cr, NAA.Cr, D, S)), FUN=mean, na.rm = T)
