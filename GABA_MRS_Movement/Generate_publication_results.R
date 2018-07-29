# For publication

library(ggplot2)

ggplot(no_outlier_subject_data.V1.C, aes(x=GABA.Cr))+
  geom_histogram(aes(y=..density..),binwidth=density(no_outlier_subject_data.V1.SZ$GABA.Cr,na.rm=T)$bw)+
  geom_density(fill="magenta",alpha = 0.1)+ #theme_bw()+
  xlab("GABA/Cr")+ylab("")+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) 

ggplot(no_outlier_subject_data.V1.C, aes(x=RMS_Mag_disp))+
  geom_histogram(aes(y=..density..),binwidth=density(no_outlier_subject_data.V1.SZ$RMS_Mag_disp,na.rm=T)$bw)+
  geom_density(fill="magenta",alpha = 0.1)+#theme_bw()+
  xlab(expression("RMS of Displacement " * Delta ~ " (mm)"))+
  ylab("")+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) 

ggplot(no_outlier_subject_data.V1.C, aes(x=RMS_Mag_speed*5))+
  geom_histogram(aes(y=..density..),binwidth=0.016)+
  geom_density(fill="magenta",alpha = 0.1)+#theme_bw()+
  xlab(expression("RMS of Speed " * Sigma ~ " (mm/s)"))+
  ylab("")+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())


# Scatter plot

PlotFunc <-function(df,X,Y,G,TITLE="",LEGENDNAME=NULL,XLAB="X",YLAB="Y",XTRANSFORM=NULL, YTRANSFORM=NULL, ZOOM=2.0) {
  #  get data from data frame
  df = data.frame(X=5*df[X],Y=df[Y],G=df[G])
  colnames(df) = c("X","Y","G")
  num = as.integer(df$G)
  # get the correct plot markers
  num = 19
  # configure the layout
  #nf = layout(mat=matrix(c(rep(1,4),2),nrow=1))
  # reset margin
  par(mar=c(5,4+ZOOM,4,2)+0.1)
  # if apply transformation
  if(!is.null(XTRANSFORM))
    df$X = XTRANSFORM(df$X)
  if(!is.null(YTRANSFORM))
    df$Y = YTRANSFORM(df$X)
  # plot the scatter plot
  plot(df$X, df$Y, pch = num,xlab = XLAB, ylab=YLAB,cex.lab=ZOOM,cex.axis=ZOOM,cex=ZOOM)
  # fit the line
  m = lm(Y ~ X, data = df)
  # predicted line
  preds = data.frame(X=seq(from=min(df$X,na.rm=T),to=max(df$X,na.rm=T),by=(max(df$X,na.rm=T)-min(df$X,na.rm=T))/100))
  lines(preds$X,predict(m,newdata = preds))
  text(x=max(df$X,na.rm=T)*(1-0.05*sign(max(df$X,na.rm=T))),
       y=max(df$Y,na.rm=T)*(1-0.05*sign(max(df$Y,na.rm=T))),
       labels=bquote(R^2 ~"=" ~ .(summary(m)$adj.r.squared)))
  # put title
  title(main = TITLE,cex.main=ZOOM)
  if(!is.null(LEGENDNAME)){
    # create a dummy plot in the second one
    par(mar=c(0,0,4,0)+0.1)
    plot(1,0,pch=1,lty=1,xlim=c(-1,1),ylim=c(-2,2),type="n",axes=FALSE,ann=FALSE)
    legend(x="topleft", legend=unique(df$G),pch = unique(num), title = LEGENDNAME,
           bty="n", y.intersp = 0.8,title.adj=0.2,cex=0.9)
  }
}

PlotFunc(no_outlier_subject_data.V1.C, X = "RMS_Mag_disp", Y = "GABA.Cr", G = "Subject_ID",
          XLAB = "RMS_Mag_Disp (mm)", YLAB = "V1 GABA/Cr",ZOOM = 1.0)
PlotFunc(no_outlier_subject_data.V1.C, X = "RMS_Mag_speed", Y = "GABA.Cr", G = "Subject_ID",
         XLAB = "RMS_Mag_Speed (mm/s)", YLAB = "V1 GABA/Cr",ZOOM = 1.0)


# Plot no outlier subject level data
pos_func = function(x){
  y = max(x,na.rm=T)*(1-0.05*sign(max(x,na.rm=T)))
  return(y)
}
# controls
p = ggplot(no_outlier_subject_data.V1.C, aes(x=RMS_Mag_disp, y=GABA.Cr)) +
  geom_point(shape=1,size=6) +
  geom_smooth(method=lm)+
  xlab(expression("RMS of Displacemet " * Delta ~ " (mm)"))+
  ylab("V1 GABA/Cr")
L = as.character(as.expression(R^2 ~"=" ~ 0.121))
p + geom_text(aes(x=1.5,y = 0.17, label = L),size=6,parse=T)+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))


p = ggplot(no_outlier_subject_data.V1.C, aes(x=RMS_Mag_speed*5, y=GABA.Cr)) +
  geom_point(shape=1,size=6) +
  geom_smooth(method=lm)+
  xlab(expression("RMS of Speed " * Sigma ~ " (mm/s)"))+
  ylab("V1 GABA/Cr")
L = as.character(as.expression(R^2 ~"=" ~ 0.212))
p + geom_text(aes(x=0.12,y = 0.16, label = L),size=6,parse=T)+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))


# patients
p = ggplot(no_outlier_subject_data.V1.SZ, aes(x=RMS_Mag_disp, y=GABA.Cr)) +
  geom_point(shape=1,size=6) +
  geom_smooth(method=lm)+
  xlab(expression("RMS of Displacemet " * Delta ~ " (mm)"))+
  ylab("V1 GABA/Cr")
L = as.character(as.expression(R^2 ~"=" ~ -0.04302 ))
p + geom_text(aes(x=1.5,y = 0.17, label = L),size=6,parse=T)+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))


p = ggplot(no_outlier_subject_data.V1.SZ, aes(x=RMS_Mag_speed*5, y=GABA.Cr)) +
  geom_point(shape=1,size=6) +
  geom_smooth(method=lm)+
  xlab(expression("RMS of Speed " * Sigma ~ " (mm/s)"))+
  ylab("V1 GABA/Cr")
L = as.character(as.expression(R^2 ~"=" ~  0.02532))
p + geom_text(aes(x=0.10,y = 0.16, label = L),size=6,parse=T)+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))


# Linear model
lm.GABA.Cr.disp = lm(GABA.Cr ~ RMS_Mag_disp , data = no_outlier_subject_data.V1.C)
# Call:
#   lm(formula = GABA.Cr ~ RMS_Mag_disp, data = no_outlier_subject_data.V1.C)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.035083 -0.008750  0.000487  0.008893  0.019713 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.158586   0.006284  25.236   <2e-16 ***
#   RMS_Mag_disp -0.012604   0.005722  -2.203   0.0363 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.0126 on 27 degrees of freedom
# Multiple R-squared:  0.1523,  Adjusted R-squared:  0.1209 
# F-statistic: 4.852 on 1 and 27 DF,  p-value: 0.03633
lm.GABA.Cr.speed = lm(GABA.Cr ~ I(RMS_Mag_speed*5), data = no_outlier_subject_data.V1.C)
# Call:
#   lm(formula = GABA.Cr ~ RMS_Mag_speed, data = no_outlier_subject_data.V1.C)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.023506 -0.007402  0.002919  0.006038  0.024004 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.160410   0.005495  29.193  < 2e-16 ***
#   RMS_Mag_speed -0.944559   0.323720  -2.918  0.00702 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.01193 on 27 degrees of freedom
# Multiple R-squared:  0.2397,  Adjusted R-squared:  0.2116 
# F-statistic: 8.514 on 1 and 27 DF,  p-value: 0.007021

lm.disp_speed=lm(GABA.Cr~RMS_Mag_disp+I(RMS_Mag_speed*5),no_outlier_subject_data.V1.C)
# Call:
#   lm(formula = GABA.Cr ~ RMS_Mag_disp + I(RMS_Mag_speed * 5), data = no_outlier_subject_data.V1.C)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0256578 -0.0050213  0.0006332  0.0068668  0.0182667 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           0.168544   0.006864  24.554   <2e-16 ***
#   RMS_Mag_disp         -0.009793   0.005303  -1.847   0.0762 .  
# I(RMS_Mag_speed * 5) -0.165113   0.063359  -2.606   0.0150 *  
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.01144 on 26 degrees of freedom
# Multiple R-squared:  0.3279,  Adjusted R-squared:  0.2762 
# F-statistic: 6.342 on 2 and 26 DF,  p-value: 0.005711


lm.Glx.Cr.disp = lm(Glx.Cr ~ RMS_Mag_disp , data = no_outlier_subject_data.V1.C)
# Call:
#   lm(formula = Glx.Cr ~ RMS_Mag_disp, data = no_outlier_subject_data.V1.C)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0224128 -0.0077343  0.0003619  0.0080593  0.0303111 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.136102   0.005965  22.815   <2e-16 ***
#   RMS_Mag_disp -0.008240   0.005432  -1.517    0.141    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01196 on 27 degrees of freedom
# Multiple R-squared:  0.07855,  Adjusted R-squared:  0.04442 
# F-statistic: 2.302 on 1 and 27 DF,  p-value: 0.1409

lm.Glx.Cr.speed = lm(Glx.Cr ~ RMS_Mag_speed , data = no_outlier_subject_data.V1.C)
# Call:
#   lm(formula = Glx.Cr ~ RMS_Mag_speed, data = no_outlier_subject_data.V1.C)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.021876 -0.007082  0.001252  0.006171  0.034222 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    0.129239   0.005728  22.561   <2e-16 ***
#   RMS_Mag_speed -0.098898   0.337490  -0.293    0.772    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01244 on 27 degrees of freedom
# Multiple R-squared:  0.00317,  Adjusted R-squared:  -0.03375 
# F-statistic: 0.08587 on 1 and 27 DF,  p-value: 0.7717

# correlation
with(no_outlier_subject_data.V1.C, cor.test(RMS_Mag_disp, GABA.Cr, na.action = na.omit, method = "pearson"))
# Pearson's product-moment correlation
# 
# data:  RMS_Mag_disp and GABA.Cr
# t = -2.2028, df = 27, p-value = 0.03633
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  -0.66209559 -0.02776746
# sample estimates:
#        cor 
# -0.3903011 

with(no_outlier_subject_data.V1.C, cor.test(RMS_Mag_speed, GABA.Cr, na.action = na.omit, method = "pearson"))
# data:  RMS_Mag_speed and GABA.Cr
# t = -2.9178, df = 27, p-value = 0.007021
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   -0.7258709 -0.1500415
# sample estimates:
#   cor 
# -0.4896223 



#####################################################################################################################################################

# Shapiro Wilkinson test of normality
shapiro.test(data.V1.C$GABA.Cr)#W = 0.979, p-value = 0.2035
shapiro.test(data.V1.C$Glx.Cr)#W = 0.971, p-value = 0.06316
shapiro.test(data.V1.C$RMS_Mag_disp)#W = 0.7831, p-value = 1.395e-09
shapiro.test(data.V1.C$RMS_Mag_speed)#W = 0.7144, p-value = 2.928e-11

shapiro.test(no_outlier_data.V1.C$GABA.Cr)#W = 0.9916, p-value = 0.8876
shapiro.test(no_outlier_data.V1.C$Glx.Cr)#W = 0.9871, p-value = 0.6163
shapiro.test(no_outlier_data.V1.C$RMS_Mag_disp)#W = 0.9713, p-value = 0.08222
shapiro.test(no_outlier_data.V1.C$RMS_Mag_speed)#W = 0.9244, p-value = 0.0002279

shapiro.test(no_outlier_subject_data.V1.C$GABA.Cr)#W = 0.9887, p-value = 0.985
shapiro.test(no_outlier_subject_data.V1.C$Glx.Cr)#W = 0.9729, p-value = 0.6398
shapiro.test(no_outlier_subject_data.V1.C$RMS_Mag_disp)#W = 0.9784, p-value = 0.7968
shapiro.test(no_outlier_subject_data.V1.C$RMS_Mag_speed)#W = 0.9365, p-value = 0.08115

# Linear mixed model
library(nlme)
lme.GABA.Cr.disp = lme(fixed = Glx.Cr ~ RMS_Mag_disp, random = ~1|Subject_ID/Region_run_order, data = no_outlier_data.V1.C, na.action = na.omit)
lme.GABA.Cr.speed = lme(fixed = GABA.Cr ~ RMS_Mag_speed, random = ~1|Subject_ID/Region_run_order, data = no_outlier_data.V1.C, na.action = na.omit)
lme.GABA.Cr.log_disp = lme(fixed = GABA.Cr ~ log(RMS_Mag_disp), random = ~1|Subject_ID, data = no_outlier_data.V1.C, na.action = na.omit)
lme.GABA.Cr.log_speed = lme(fixed = GABA.Cr ~ log(RMS_Mag_speed), random = ~1|Subject_ID, data = no_outlier_data.V1.C, na.action = na.omit)

# Outlier data
p = ggplot(outlier_data.V1.C, aes(x=RMS_Mag_speed,y=GABA.Cr, group = Subject_ID, shape=Subject_ID))+geom_line() 
p + geom_point(aes(colour=Subject_ID),size=8) + scale_shape_manual(values=seq(0,8,1))


outlier_data.V1.C = read.table("C:/Users/Edward/Desktop/gaba_movement/Set04_outlier_movement/GABA_movementoutlier_data_V1_C_relabel_movement_class.csv",
                               header = TRUE, sep = ",", na.string = "NA",  dec = ".", strip.white = TRUE)
# Displacement
tmp = outlier_data.V1.C
tmp$movement_class[tmp$movement_class==2] = 1
tmp$movement_class[tmp$movement_class==4] = 0
K.disp = aggregate(cbind(GABA.Cr, RMS_Mag_disp, movement_class) ~ Subject_ID + Session_Runs,tmp,mean)
K.disp$movement_class[K.disp$movement_class>0] = 1
K.disp$movement_class = as.factor(K.disp$movement_class)
K.disp = aggregate(cbind(GABA.Cr, RMS_Mag_disp) ~ Subject_ID + movement_class, K.disp, mean)
K.disp = K.disp[c(1,2,3,4,6,8,10,11,12,13,14,15),]
lm.disp = lm(GABA.Cr ~ RMS_Mag_disp, K.disp)
lm2.disp = lm(GABA.Cr ~poly(RMS_Mag_disp, 2,raw = T), K.disp)
lm3.disp = lm(GABA.Cr ~poly(RMS_Mag_disp, 3,raw = T), K.disp)

p = ggplot(K.disp, aes(x=RMS_Mag_disp,y=GABA.Cr, group = Subject_ID, shape=Subject_ID))+geom_line(aes(colour=Subject_ID), size=1)
p = p + geom_point(aes(colour=Subject_ID),size=8) + scale_shape_manual(values=seq(0,6,1))
p = p + stat_smooth(aes(group=1),method=lm, size = 1, se=TRUE, formula=y ~ poly(x,1,raw=T), linetype = "dashed")
p+xlab("RMS_Mag_Disp")+ylab("GABA/Cr") + theme(legend.position="none")

# Speed
tmp = outlier_data.V1.C
tmp$movement_class[tmp$movement_class==2] = 0
tmp$movement_class[tmp$movement_class==4] = 1
K.speed = aggregate(cbind(GABA.Cr, RMS_Mag_speed, movement_class) ~ Subject_ID + Session_Runs, tmp, mean)
K.speed$movement_class[K.speed$movement_class>0] = 1
K.speed$movement_class = as.factor(K.speed$movement_class)
K.speed = aggregate(cbind(GABA.Cr, RMS_Mag_speed) ~ Subject_ID + movement_class,K.speed,mean)
K.speed = K.speed[c(1,3,4,5,6,7,9,10,11,12,13,14,15,16),]
K.speed$RMS_Mag_speed = K.speed$RMS_Mag_speed*5
lm.speed = lm(GABA.Cr ~ RMS_Mag_speed, K.speed)
lm2.speed = lm(GABA.Cr ~poly(RMS_Mag_speed, 2,raw = T), K.speed)
lm3.speed = lm(GABA.Cr ~poly(RMS_Mag_speed, 3,raw = T), K.speed)

p = ggplot(K.speed, aes(x=RMS_Mag_speed,y=GABA.Cr, group = Subject_ID, shape=Subject_ID))+geom_line(aes(colour=Subject_ID),size=1) 
p = p + geom_point(aes(colour=Subject_ID),size=8) + scale_shape_manual(values=seq(0,6,1))
p = p + stat_smooth(aes(group = 1), method=lm, size = 1,se=TRUE,formula=y ~ poly(x,2,raw=T),linetype="dashed")
p+ xlab("RMS_Mag_Speed (mm/s)")+ylab("GABA/Cr") + theme(legend.position="none")

# Call:
#   lm(formula = GABA.Cr ~ poly(RMS_Mag_speed, 2, raw = T), data = K.speed)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.031500 -0.009297  0.000143  0.007626  0.039220 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                       0.15701    0.01575   9.969 7.62e-07 ***
#   poly(RMS_Mag_speed, 2, raw = T)1 -0.21575    0.15409  -1.400    0.189    
# poly(RMS_Mag_speed, 2, raw = T)2  0.42372    0.30605   1.384    0.194    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.01836 on 11 degrees of freedom
# Multiple R-squared:  0.1521,  Adjusted R-squared:  -0.002046 
# F-statistic: 0.9867 on 2 and 11 DF,  p-value: 0.4035

t.test(K.speed$GABA.Cr[1:7],K.speed$GABA.Cr[8:14],paired = T)

#Paired t-test: suggesting that too much movement can generate data that is not so different from not much movement, which
#becomes an confound.

# data:  K.speed$GABA.Cr[1:7] and K.speed$GABA.Cr[8:14]
# t = 0.7053, df = 6, p-value = 0.507
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.008396748  0.015198074
# sample estimates:
#   mean of the differences 
# 0.003400663 

t.test(K.speed$RMS_Mag_speed[1:7],K.speed$RMS_Mag_speed[8:14],paired = T)
# Paired t-test
# 
# data:  K.speed$RMS_Mag_speed[1:7] and K.speed$RMS_Mag_speed[8:14]
# t = -4.9563, df = 6, p-value = 0.002562
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.318636 -0.108006
# sample estimates:
#   mean of the differences 
# -0.213321 

# Test for homogeniety of varaince
raw.data = read.table("C:/Users/Edward/Desktop/gaba_movement/relabel_movement.csv",header = TRUE, sep = ",", na.string = "NA",  dec = ".", strip.white = TRUE)
# Remove data entry with NA movement
raw.data = subset(raw.data,(!is.na(raw.data$RMS_Mag_disp) & !is.na(raw.data$GABA.Cr))) #remove the ones missing movement or spectroscopy data
# Remove data entry with runs too short according to the column "Too_Short"
raw.data = subset(raw.data,raw.data$Too_Short == 0)
# select only the controls
raw.data = subset(raw.data, raw.data$Groups == "C" & raw.data$Brain_Region == "v1",
                               select = c(Subject_ID,Groups, movement_class,Session_Runs,Region_run_order,Brain_Region,time,
                                          GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                          NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                          Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                          RMS_Y_disp,RMS_X_speed,RMS_Y_speed))

raw.data$movement_class[raw.data$movement_class > 0] = 1
raw.data$movement_class = as.factor(raw.data$movement_class)
levene.test(y=raw.data$GABA.Cr, group = raw.data$movement_class, location="median",bootstrap=F)

# modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median
# 
# data:  raw.data$GABA.Cr
# Test Statistic = 7.694, p-value = 0.006646

sd(raw.data$GABA.Cr[raw.data$movement_class==0])#0.01446517
sd(raw.data$GABA.Cr[raw.data$movement_class==1])#0.02584108

# this suggests that movement may affect variability of GABA/Cr


renumbered_no_outlier.data = read.table("C:/Users/Edward/Desktop/gaba_movement/Set03_excluded_outliers/GABA_movement_no_outlier_data_V1_C_relabeled_run.csv",header = TRUE, sep = ",", na.string = "NA",  dec = ".", strip.white = TRUE)
aov.RMS_Mag_disp = aov(RMS_Mag_disp ~ Subject_ID * Region_run_order,data = renumbered_no_outlier.data)
#                             Df Sum Sq Mean Sq F value  Pr(>F)   
# Subject_ID                  28 12.032  0.4297   3.080 0.00497 **
#   Region_run_order             1  0.539  0.5391   3.864 0.06270 . 
# Subject_ID:Region_run_order 25  5.165  0.2066   1.481 0.18210   
# Residuals                   21  2.930  0.1395                   
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 5 observations deleted due to missingness
aov.RMS_Mag_speed = aov(RMS_Mag_speed ~ Subject_ID * Region_run_order,data = renumbered_no_outlier.data)
#                             Df   Sum Sq   Mean Sq F value   Pr(>F)    
# Subject_ID                  28 0.003210 1.146e-04   5.401 0.000173 ***
#   Region_run_order             1 0.000002 1.890e-06   0.089 0.768461    
# Subject_ID:Region_run_order 27 0.000333 1.235e-05   0.582 0.903538    
# Residuals                   19 0.000403 2.123e-05                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 5 observations deleted due to missingness

aov.GABA.Cr = aov(GABA.Cr~ Subject_ID * Region_run_order,data = renumbered_no_outlier.data)
#                             Df   Sum Sq   Mean Sq F value   Pr(>F)    
# Subject_ID                  28 0.012376 0.0004420  12.712 3.19e-08 ***
#   Region_run_order             1 0.000359 0.0003592  10.330    0.004 ** 
#   Subject_ID:Region_run_order 28 0.000971 0.0000347   0.998    0.509    
# Residuals                   22 0.000765 0.0000348                     
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 1 observation deleted due to missingness

M1 = read.table("C:/Users/Edward/Desktop/gaba_movement/Set04_outlier_movement/GABA_movementoutlier_data_V1_C_relabel_movement_class.csv",
                header = TRUE, sep = ",", na.string = "NA",  dec = ".", strip.white = TRUE)
levene.test(y=M1$GABA.Cr, group = M1$movement_class, location="median",bootstrap=F)
# modified robust Brown-Forsythe Levene-type test based on the absolute deviations from the median
# 
# data:  M1$GABA.Cr
# Test Statistic = 0.4202, p-value = 0.6592


#outlier exclusion
O.disp = boxplot(data.V1.C$RMS_Mag_disp)
O.speed = boxplot(data.V1.C$RMS_Mag_speed*5)
o.GABA = boxplot(data.V1.C$GABA.Cr)
# RMS_Mag_disp
# [,1]
# [1,] 0.17955
# [2,] 0.71402
# [3,] 1.09840
# [4,] 1.44140
# [5,] 2.37140
# RMS_Mag_speed
# [,1]
# [1,] 0.0216410
# [2,] 0.0468415
# [3,] 0.0699900
# [4,] 0.0983900
# [5,] 0.1662000
# GABA/Cr
# [,1]
# [1,] 0.1105499
# [2,] 0.1361415
# [3,] 0.1455557
# [4,] 0.1546757
# [5,] 0.1777073

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

# instructed to move disp
inst_disp = c(2.8219,3.3798,4.6767,7.7149,4.5134,5.0472,5.7295,3.1377,2.3661,3.6739,1.5075,1.8518,1.3814,2.3738,0.8654)
# instructed to move speed
inst_speed= c(0.12292,0.051076,0.056594,0.085668,0.07251,0.11616,0.076222,0.073431,0.057812,0.007616,0.044545,0.082501,0.057805,0.063135,0.074874)*5
# instructed to move GABA/Cr
inst_gaba = c(0.141711086,0.127542046,0.1402721,0.122721722,0.103515287,0.132718165,0.126313152,0.191865882,0.133231846,0.162970496,0.210047177,0.14599094,0.13156627,0.0940642,0.172275835)

# RMS_Mag_disp
d = data.frame(x=gl(1,length(data.V1.SZ$RMS_Mag_disp)),y=data.V1.SZ$RMS_Mag_disp)
p = ggplot(d,aes(x,y))+geom_violin(fill="skyblue", alpha=0.5) + coord_flip()
p = p + stat_summary(fun.data= f1, geom="boxplot",alpha=0.5) #boxplot
p = p + stat_summary(fun.y=f2, geom="point",size = 4)# outlier
#p = p + geom_point(colour="red",size=4, shape = factor(7),data=data.frame(x=gl(1,length(inst_disp)),y=inst_disp))+scale_shape(solid = FALSE)
p + xlab("") + ylab(expression("RMS of Displacement " * Delta ~ " (mm)")) + 
  theme(axis.ticks.y=element_blank(),axis.text.y=element_blank(),legend.position="none",
        axis.text.x = element_text(size=18),axis.title.x = element_text(size=20))


# RMS_Mag_speed
d = data.frame(x=gl(1,length(data.V1.SZ$RMS_Mag_speed)),y=5*data.V1.SZ$RMS_Mag_speed)
p = ggplot(d,aes(x,y))+geom_violin(fill="skyblue", alpha=0.5) + coord_flip()
p = p + stat_summary(fun.data= f1, geom="boxplot",alpha=0.5) #boxplot
p = p + stat_summary(fun.y=f2, geom="point",size = 4)# outlier
#p = p + geom_point(colour="red",size=4, shape = factor(7),data=data.frame(x=gl(1,length(inst_speed)),y=inst_speed))+scale_shape(solid = FALSE)
p + xlab("") + ylab(expression("RMS of Speed " * Sigma ~ " (mm/s)"))+ 
  theme(axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        axis.text.x = element_text(size=18),axis.title.x = element_text(size=20))

# GABA/Cr
d = data.frame(x=gl(1,length(data.V1.SZ$GABA.Cr)),y=data.V1.SZ$GABA.Cr)
p = ggplot(d,aes(x,y))+geom_violin(fill="skyblue", alpha=0.5) + coord_flip()
p = p + stat_summary(fun.data= f1, geom="boxplot",alpha=0.5) + stat_summary(fun.y=f2, geom="point",size = 4)
#p = p + geom_point(aes(ymax=max(inst_gaba),,position=position_dodge(width=10)),colour="red",size=4, shape = factor(7),data=data.frame(x=gl(1,length(inst_gaba)),y=inst_gaba))+scale_shape(solid = FALSE)
p + xlab("") + ylab("GABA/Cr") + 
  theme(axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        axis.text.x = element_text(size=18),axis.title.x = element_text(size=20))


## +++++++++++++++++++++++++++++ residuals +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# control residual vs. fitted
lm_disp = lm(GABA.Cr ~ RMS_Mag_disp, no_outlier_subject_data.V1.C) # control disp
ggplot(lm_disp, aes(.fitted, .resid)) + geom_hline(linetype = 2, size = 0.2) +
  geom_point(shape=1,size=5) + geom_smooth(se = F,method = loess) + scale_x_continuous("Fitted Values") +
  scale_y_continuous("Residual") + ggtitle(expression("Control Residuals vs. Fitted y = " * beta * Delta ~ "+ " *epsilon))+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20),
        plot.title = element_text(size=20))

lm_speed = lm(GABA.Cr ~ I(RMS_Mag_speed*5), no_outlier_subject_data.V1.C) # control speed
ggplot(lm_speed, aes(.fitted, .resid)) + geom_hline(linetype = 2, size = 0.2) +
  geom_point(shape=1,size=5) + geom_smooth(se = F,method = loess) + scale_x_continuous("Fitted Values") +
  scale_y_continuous("Residual") +   ggtitle(expression("Control Residuals vs. Fitted y = " * beta * Sigma ~ "+ " *epsilon))+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20),
        plot.title = element_text(size=20))

lm_disp_speed = lm(GABA.Cr ~ RMS_Mag_disp + I(RMS_Mag_speed*5), no_outlier_subject_data.V1.C)
ggplot(lm_disp_speed, aes(.fitted, .resid)) + geom_hline(linetype = 2, size = 0.2) +
  geom_point(shape=1,size=5) + geom_smooth(se = F,method = loess) + scale_x_continuous("Fitted Values") +
  scale_y_continuous("Residual") +   ggtitle(expression("Control Residuals vs. Fitted y = " * beta * ""[1] * Sigma ~ "+ " * beta * ""[2] *Delta ~ "+ " *epsilon))+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20),
        plot.title = element_text(size=20))

#=========================================================================================================================
lm_disp = lm(GABA.Cr ~ RMS_Mag_disp, no_outlier_subject_data.V1.SZ) #patient disp
ggplot(lm_disp, aes(.fitted, .resid)) + geom_hline(linetype = 2, size = 0.2) +
  geom_point(shape=1,size=5) + geom_smooth(se = F,method = loess) + scale_x_continuous("Fitted Values") +
  scale_y_continuous("Residual") + ggtitle(expression("Patient Residuals vs. Fitted y = " * beta * Delta ~ "+ " *epsilon))+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20),
        plot.title = element_text(size=20))

lm_speed = lm(GABA.Cr ~ I(RMS_Mag_speed*5), no_outlier_subject_data.V1.SZ) # patient speed
ggplot(lm_speed, aes(.fitted, .resid)) + geom_hline(linetype = 2, size = 0.2) +
  geom_point(shape=1,size=5) + geom_smooth(se = F,method = loess) + scale_x_continuous("Fitted Values") +
  scale_y_continuous("Residual") +   ggtitle(expression("Patient Residuals vs. Fitted y = " * beta * Sigma ~ "+ " *epsilon))+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20),
        plot.title = element_text(size=20))

lm_disp_speed = lm(GABA.Cr ~ RMS_Mag_disp + I(RMS_Mag_speed*5), no_outlier_subject_data.V1.SZ)
ggplot(lm_disp_speed, aes(.fitted, .resid)) + geom_hline(linetype = 2, size = 0.2) +
  geom_point(shape=1,size=5) + geom_smooth(se = F,method = loess) + scale_x_continuous("Fitted Values") +
  scale_y_continuous("Residual") +   ggtitle(expression("Patient Residuals vs. Fitted y = " * beta * ""[1] * Sigma ~ "+ " * beta * ""[2] *Delta ~ "+ " *epsilon))+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20),
        plot.title = element_text(size=20))






