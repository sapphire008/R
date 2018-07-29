# compare patients and controls data

# plot patients histogram
library(ggplot2)

f1 = function(x){
  tmp = as.numeric(boxplot(x,plot=F)$stats)
  #tmp = quantile(x,prob=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(tmp) = c("ymin", "lower", "middle", "upper", "ymax")
  return(tmp)
}
f2 = function(x){
  tmp = boxplot(x)$out
  return(tmp)
}

# RMS_Mag_disp
d = data.frame(x=gl(1,length(data.V1.SZ$RMS_Mag_disp)),y=data.V1.SZ$RMS_Mag_disp)
p = ggplot(d,aes(x,y))+geom_violin(fill="skyblue", alpha=0.5) + coord_flip()
p = p + stat_summary(fun.data= f1, geom="boxplot",alpha=0.5) #boxplot
p = p + stat_summary(fun.y=f2, geom="point",size = 4)# outlier
#p = p + geom_point(colour="red",size=4, shape = factor(7),data=data.frame(x=gl(1,length(inst_disp)),y=inst_disp))+scale_shape(solid = FALSE)
p + xlab("") + ylab("RMS_Mag_Disp(mm)") + theme(axis.ticks.y=element_blank(),axis.text.y=element_blank(),legend.position="none")


# RMS_Mag_speed
d = data.frame(x=gl(1,length(data.V1.SZ$RMS_Mag_speed)),y=5*data.V1.SZ$RMS_Mag_speed)
p = ggplot(d,aes(x,y))+geom_violin(fill="skyblue", alpha=0.5) + coord_flip()
p = p + stat_summary(fun.data= f1, geom="boxplot",alpha=0.5) #boxplot
p = p + stat_summary(fun.y=f2, geom="point",size = 4)# outlier
#p = p + geom_point(colour="red",size=4, shape = factor(7),data=data.frame(x=gl(1,length(inst_speed)),y=inst_speed))+scale_shape(solid = FALSE)
p + xlab("") + ylab("RMS_Mag_Speed(mm/s)") + theme(axis.ticks.y=element_blank(),axis.text.y=element_blank())

# GABA/Cr
d = data.frame(x=gl(1,length(data.V1.SZ$GABA.Cr)),y=data.V1.SZ$GABA.Cr)
p = ggplot(d,aes(x,y))+geom_violin(fill="skyblue", alpha=0.5) + coord_flip()
p = p + stat_summary(fun.data= f1, geom="boxplot",alpha=0.5) + stat_summary(fun.y=f2, geom="point",size = 4)
#p = p + geom_point(aes(ymax=max(inst_gaba),,position=position_dodge(width=10)),colour="red",size=4, shape = factor(7),data=data.frame(x=gl(1,length(inst_gaba)),y=inst_gaba))+scale_shape(solid = FALSE)
p + xlab("") + ylab("GABA/Cr") + theme(axis.ticks.y=element_blank(),axis.text.y=element_blank())

#############################################################################################################
# Subject level histogram
ggplot(no_outlier_subject_data.V1.SZ, aes(x=GABA.Cr))+
  geom_histogram(aes(y=..density..),binwidth=density(no_outlier_subject_data.V1.SZ$GABA.Cr,na.rm=T)$bw)+
  geom_density(fill="magenta",alpha = 0.1)+
  xlab("GABA/Cr")+
  ylab("")+
  theme(axis.text.y=element_blank())+theme(axis.ticks.y=element_blank())
  

ggplot(no_outlier_subject_data.V1.SZ, aes(x=RMS_Mag_disp))+
  geom_histogram(aes(y=..density..),binwidth=density(no_outlier_subject_data.V1.SZ$RMS_Mag_disp,na.rm=T)$bw)+
  geom_density(fill="magenta",alpha = 0.1)+
  xlab("RMS_Mag_Disp (mm)")+
  ylab("")+
  theme(axis.text.y=element_blank())+theme(axis.ticks.y=element_blank())


ggplot(no_outlier_subject_data.V1.SZ, aes(x=RMS_Mag_speed*5))+
  geom_histogram(aes(y=..density..),binwidth=0.016)+
  geom_density(fill="magenta",alpha = 0.1)+
  xlab("RMS_Mag_Speed (mm/s)")+
  ylab("")+
  theme(axis.text.y=element_blank())+theme(axis.ticks.y=element_blank())


#############################################################################################################
# Linear model for Patients
lm.disp = lm(GABA.Cr ~ RMS_Mag_disp, no_outlier_subject_data.V1.SZ)
# Call:
#   lm(formula = GABA.Cr ~ RMS_Mag_disp, data = no_outlier_subject_data.V1.SZ)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.033865 -0.008866  0.004361  0.011156  0.017015 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.142854   0.008817  16.201 1.03e-13 ***
# RMS_Mag_disp -0.002491   0.007202  -0.346    0.733    
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.01392 on 22 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.005408,  Adjusted R-squared:  -0.0398 
# F-statistic: 0.1196 on 1 and 22 DF,  p-value: 0.7327
lm.speed = lm(GABA.Cr ~ I(RMS_Mag_speed*5),no_outlier_subject_data.V1.SZ)
# Call:
#   lm(formula = GABA.Cr ~ I(RMS_Mag_speed * 5), data = no_outlier_subject_data.V1.SZ)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.035879 -0.009646  0.005325  0.011443  0.014373 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           0.147613   0.007425  19.882 1.51e-15 ***
# I(RMS_Mag_speed * 5) -0.124196   0.116134  -1.069    0.296    
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.01392 on 22 degrees of freedom
# (1 observation deleted due to missingness)
# Multiple R-squared:  0.04942,  Adjusted R-squared:  0.006207 
# F-statistic: 1.144 on 1 and 22 DF,  p-value: 0.2965

lm.disp_speed = lm(GABA.Cr ~ RMS_Mag_disp + I(RMS_Mag_speed*5),no_outlier_subject_data.V1.SZ)
# Call:
#   lm(formula = GABA.Cr ~ RMS_Mag_disp + I(RMS_Mag_speed * 5), data = no_outlier_subject_data.V1.SZ)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.033486 -0.008922  0.003277  0.011399  0.017021 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           0.153380   0.012398  12.371 7.92e-11 ***
#   RMS_Mag_disp         -0.006338   0.008001  -0.792    0.438    
# I(RMS_Mag_speed * 5) -0.111843   0.119237  -0.938    0.359    
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.014 on 20 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:  0.06412,  Adjusted R-squared:  -0.02947 
# F-statistic: 0.6851 on 2 and 20 DF,  p-value: 0.5155



# Plot no outlier subject level data
pos_func = function(x){
  y = max(x,na.rm=T)*(1-0.05*sign(max(x,na.rm=T)))
  return(y)
}
p = ggplot(no_outlier_subject_data.V1.SZ, aes(x=RMS_Mag_disp, y=GABA.Cr)) +
  geom_point(shape=1,size=6) +
  geom_smooth(method=lm)+
  xlab("RMS_Mag_Disp (mm)")+
  ylab("V1 GABA/Cr")
L = as.character(as.expression(R^2 ~"=" ~ -0.0398012))
p + geom_text(aes(x=1.7,y = 0.17, label = L),parse=T)


p = ggplot(no_outlier_subject_data.V1.SZ, aes(x=RMS_Mag_speed*5, y=GABA.Cr)) +
  geom_point(shape=1,size=6) +
  geom_smooth(method=lm)+
  xlab("RMS_Mag_Speed (mm/s)")+
  ylab("V1 GABA/Cr")
L = as.character(as.expression(R^2 ~"=" ~ 0.006206682))
p + geom_text(aes(x=0.10,y = 0.16, label = L),parse=T)

