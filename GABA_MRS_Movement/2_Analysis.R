# Movement Analysis 11/08/2014
rm(list=ls())

library(lme4)
# library(lmerTest)
library(plyr)
library(lmtest)
library(cluster)
library(HSAUR)

source_data <- "/Users/Edward/Documents/Ubuntu/Analysis/data/movement/movement_data_v1_c.rds"
data <- readRDS(source_data)

remove_outlier<- function(x, mode = "") {
  # remove all the outliers. If the data is near normal distribution,
  # it should keep the data only within 2.7sigma within the mean, or
  #[Q1-1.5*IQR, Q3+1.5*IQR] non-parametrically
  # Boxplot is non-parametric, so it does not assume normality
  BP = boxplot(x,plot=FALSE)
  switch(mode,
         higher={x[(x %in% BP$out[BP$out>BP$stats[5]])]=NA},
         lower={x[(x %in% BP$out[BP$out<BP$stats[1]])]=NA},
         {x[(x %in% BP$out)] = NA})
  # return x
  return(x)
}

# Get good data
data.good = subset(data, subset= data$Good.Runs == 1 & data$Instructed.to.move == 0) #remove bad data
# relabel region run order
A<- ddply(data.good, .(Subject), function(x) data.frame(x,NewID=1:nrow(x)))
data.good$Region.Run.Order <- as.factor(A$NewID)
# Remove outlier
data.clean <- data.good
data.clean[c("D","S")] <- sapply(data.clean[c("D","S")], remove_outlier)
# Aggregate to subject level data
data.clean.GABA.D <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, GABA.Cr, D)), FUN = mean, na.rm = T)
data.clean.GABA.S <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, GABA.Cr, S)), FUN = mean, na.rm = T)
data.clean.GABA.DS <- aggregate(.~ Subject , data = subset(data.clean,select=c(Subject, GABA.Cr, D,S)),FUN= mean, na.rm = T)
data.clean.NAA.D  <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, NAA.Cr, D)), FUN = mean, na.rm = T)
data.clean.NAA.S  <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, NAA.Cr, S)), FUN = mean, na.rm = T)
data.clean.NAA.DS <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, NAA.Cr, D,S)),FUN= mean, na.rm = T)
data.clean.Glx.D  <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, Glx.Cr, D)), FUN = mean, na.rm = T)
data.clean.Glx.S  <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, Glx.Cr, S)), FUN = mean, na.rm = T)
data.clean.Glx.DS <- aggregate(. ~ Subject , data = subset(data.clean,select=c(Subject, Glx.Cr, D,S)),FUN= mean, na.rm = T)

# Linear models
lm.GABA.D <- lm(GABA.Cr ~ D, data.clean.GABA.D)

#s <- which(!apply(influence.measures(lm.GABA.D)$is.inf, MARGIN=1, any))
#lm.GABA.D <- lm(GABA.Cr ~D, data.clean.GABA.DS, subset = s)

lm.GABA.S <- lm(GABA.Cr ~ S, data.clean.GABA.S)
summary(lm.GABA.DS <-glm(GABA.Cr ~ D + S, data = data.clean.GABA.DS))

lm.GABA.DS <- lm(GABA.Cr ~ D + S, data.clean.GABA.DS)

lme.D <- lmer(GABA.Cr ~ D + (1+D|Subject), data.clean, REML = T)
lme.S <- lmer(GABA.Cr ~ S + (1+S|Subject), data.clean, REML = F)
lme.DS<- lmer(GABA.Cr ~ D + S + (1+D|Subject) + (1+S|Subject), data.clean, REML = F)
lme.null <- lmer(GABA.Cr ~ 1 + (1|Subject), data.clean, REML = F)
anova(lme.null, lme.DS)

summary(lm(NAA.Cr ~ D, data.clean.NAA.D))
summary(lm(NAA.Cr ~ S, data.clean.NAA.S))
summary(lm(NAA.Cr ~ D + S, data.clean.NAA.DS))

summary(lmer(NAA.Cr ~  D + S + Region.Run.Order + (1|Subject), data.clean))

# model diagnostics
library(car)
# variance inflation factor: test for multi-colinearity
vif(lm.GABA.DS)
# non-linearity
crPlots(lm.GABA.DS)
ceresPlots(lm.GABA.DS)
# non-independence of errors
durbinWatsonTest(lm.GABA.DS)

lm.GABA.DS <- lm(GABA.Cr ~ D + S, data.clean.GABA.DS)
vif(lm.GABA.DS)
# D       S 
# 1.00406 1.00406 
lm.NAA.DS <- lm(NAA.Cr ~ D + S, data.clean.NAA.DS)
vif(lm.NAA.DS)
# D        S 
# 1.007146 1.007146 
lm.Glx.DS <- lm(Glx.Cr ~ D + S, data.clean.Glx.DS)
vif(lm.Glx.DS)
# D        S 
# 1.007146 1.007146 
