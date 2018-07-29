# Raw Data
imported_raw_data_dir = "/nfs/r21_gaba/reprocessing/analysis/imported_data/Set01/"
Item.raw_data_frames = "R_imported_data_frames.Rda"
Item.raw_data_list = "R_imported_data_list.Rda"
Item.raw_subject_data_frames = "R_imported_subject_data_frames.Rda"
Item.raw_subject_data_list = "R_imported_subject_data_list.Rda"
# Data removed outliers
imported_clean_data_dir = "/nfs/r21_gaba/reprocessing/analysis/imported_data/Set02/"
Item.clean_data_frames = "R_imported_no_outlier_data_frames.Rda"
Item.clean_data_list = "R_imported_no_outlier_data_list.Rda"
Item.clean_subject_data_frames = "R_imported_no_outlier_subject_data_frames.Rda"
Item.clean_subject_data_list = "R_imported_no_outlier_subject_data_list.Rda"
# Output saving directory
result_save_dir = "/nfs/r21_gaba/reprocessing/analysis/results/"
# Defining numerical columns
test_cols = c("GABA.Cr","Glx.Cr","Gaba.AdjPeak","Glx.AdjPeak","Cr","Off.Res.Cr","NAA","NAA.Cr",
              "RMS_Mag_disp","RMS_Mag_speed","Magnitude.of.Displacement.mm.",
              "Magnitude.of.Speed.mm.","Jaggedness_X","Jaggedness_Y","RMS_X_disp",
              "RMS_Y_disp","RMS_X_speed","RMS_Y_speed")

# Functions that help plotting
PlotFunc <-function(df,X,Y,G,TITLE="",LEGENDNAME=NULL,XLAB="X",YLAB="Y", CURVEDEGREE = 1, XTRANSFORM=NULL, YTRANSFORM=NULL, ZOOM=2.0) {
  #  get data from data frame
  df = data.frame(X=df[X],Y=df[Y],G=df[G])
  colnames(df) = c("X","Y","G")
  num = as.integer(df$G)
  # get the correct plot markers
  num = num-min(num)
  num[num>25] = num[num>25]-26+48 # use numbers
  num[num>57] = num[num>57]-58+65 #use capital letters
  num[num>90] = num[num>90]-91+97 #use lowercase letters
  # configure the layout
  #nf = layout(mat=matrix(c(rep(1,4),2),nrow=1))
  # reset margin
  par(mar=c(5,4+ZOOM,4,0)+0.1)
  # if apply transformation
  if(!is.null(XTRANSFORM))
    df$X = XTRANSFORM(df$X)
  if(!is.null(YTRANSFORM))
    df$Y = YTRANSFORM(df$X)
  # plot the scatter plot
  plot(df$X, df$Y, pch = num,xlab = XLAB, ylab=YLAB,cex.lab=ZOOM,cex.axis=ZOOM,cex=ZOOM)
  # fit the line
  if(CURVEDEGREE!=1)
    m = lm(Y ~ poly(X, degree = CURVEDEGREE,raw = TRUE), data = df)
  else
    m = lm(Y ~ X, data = df)
  # predicted line
  preds = data.frame(X=seq(from=min(df$X,na.rm=T),to=max(df$X,na.rm=T),by=(max(df$X,na.rm=T)-min(df$X,na.rm=T))/100))
  lines(preds$X,predict(m,newdata = preds))
  text(x=max(df$X,na.rm=T)*(1-0.05*sign(max(df$X,na.rm=T))),
       y=max(df$Y,na.rm=T)*(1-0.05*sign(max(df$Y,na.rm=T))),
       labels=bquote(R^2 ~"=" ~ .(summary(m)$r.squared)))
  # put title
  title(main = TITLE,cex.main=ZOOM)
  if(!is.null(LEGENDNAME)){
    # create a dummy plot in the second one
    par(mar=c(0,0,4,0)+0.1)
    plot(1,0,pch=1,lty=1,xlim=c(-1,1),ylim=c(-2,2),type="n",axes=FALSE,ann=FALSE)
    legend(x="topleft", legend=unique(df$G),pch = unique(num), title = LEGENDNAME,
           bty="n", y.intersp = 0.8,title.adj=0.2,cex=0.9)
  }
  return(m)
}

# Functions that help plotting
PlotFunc2 <-function(df,X,Y,G,TITLE="",LEGENDNAME=NULL,XLAB="X",YLAB="Y", CURVEDEGREE = 1, ZOOM=2.0) {
  #  get data from data frame
  df = data.frame(X=df[X],Y=df[Y],G=df[G])
  colnames(df) = c("X","Y","G")
  num = as.integer(df$G)
  # get the correct plot markers
  num = num-min(num)
  num[num>25] = num[num>25]-26+48 # use numbers
  num[num>57] = num[num>57]-58+65 #use capital letters
  num[num>90] = num[num>90]-91+97 #use lowercase letters
  # configure the layout
  #nf = layout(mat=matrix(c(rep(1,4),2),nrow=1))
  # reset margin
  par(mar=c(5,4+ZOOM,4,0)+0.1)
  # plot the scatter plot
  #plot(df$X, df$Y, pch = num,xlab = XLAB, ylab=YLAB,cex.lab=ZOOM,cex.axis=ZOOM,cex=ZOOM)
  scatter.smooth(df$X, df$Y, degree = CURVEDEGREE, xlab = XLAB, ylab = YLAB,cex.lab=ZOOM, cex.axis=ZOOM, cex=ZOOM,pch = num)
  # put title
  title(main = TITLE,cex.main=ZOOM)
  if(!is.null(LEGENDNAME)){
    # create a dummy plot in the second one
    par(mar=c(0,0,4,0)+0.1)
    plot(1,0,pch=1,lty=1,xlim=c(-1,1),ylim=c(-2,2),type="n",axes=FALSE,ann=FALSE)
    legend(x="topleft", legend=unique(df$G),pch = unique(num), title = LEGENDNAME,
           bty="n", y.intersp = 0.8,title.adj=0.2,cex=0.9)
  }
  m = loess.smooth(df$X, df$Y, degree = CURVEDEGREE)
  return(m)
}

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&   RAW DATA     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# load raw data
load(paste(imported_raw_data_dir,Item.raw_subject_data_frames,sep=""))


# ************************ First Degree ****************************************************
# set up layout
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
# Plotting control V1 RMS_Mag_disp vs GABA/Cr
lm.C.V1.GABA.RMS_Mag_disp = PlotFunc(df = outlier_subject_data.V1.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
                                       TITLE="Subject Averaged Control V1 GABA/Cr vs. RMS_Mag_disp",
                                       XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_disp vs GABA/Cr
lm.C.MFG.GABA.RMS_Mag_disp = PlotFunc(df = subject_data.MFG.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
                                        TITLE="Subject Averaged Control MFG GABA/Cr vs. RMS_Mag_disp",
                                        XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control V1 RMS_Mag_speed vs GABA/Cr
lm.C.V1.GABA.RMS_Mag_speed = PlotFunc(df = subject_data.V1.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
                                        TITLE="Subject Averaged Control V1 GABA/Cr vs. RMS_Mag_speed",
                                        XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_speed vs GABA/Cr
lm.C.MFG.GABA.RMS_Mag_speed = PlotFunc(df = subject_data.MFG.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
                                         TITLE="Subject Averaged Control MFG GABA/Cr vs. RMS_Mag_speed",
                                         XLAB="RMS_Mag_speed",YLAB="GABA/Cr", LEGENDNAME="Controls")
# ************************* Second Degree *****************************************************
# set up layout
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
fit2.C.V1.GABA.RMS_Mag_disp = PlotFunc(df = subject_data.V1.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Control V1 GABA/Cr vs. RMS_Mag_disp",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr",CURVEDEGREE = 2)
# Plotting control MFG RMS_Mag_disp vs GABA/Cr
fit2.C.MFG.GABA.RMS_Mag_disp = PlotFunc(df = subject_data.MFG.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Control MFG GABA/Cr vs. RMS_Mag_disp",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr",CURVEDEGREE = 2)
# Plotting control V1 RMS_Mag_speed vs GABA/Cr
fit2.C.V1.GABA.RMS_Mag_speed = PlotFunc(df = subject_data.V1.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Control V1 GABA/Cr vs. RMS_Mag_speed",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr",CURVEDEGREE = 2)
# Plotting control MFG RMS_Mag_speed vs GABA/Cr
fit2.C.MFG.GABA.RMS_Mag_speed = PlotFunc(df = subject_data.MFG.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Control MFG GABA/Cr vs. RMS_Mag_speed",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr", LEGENDNAME="Controls",CURVEDEGREE = 2)

# ************************* Third Degree (Best) *****************************************************
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
fit3.C.V1.GABA.RMS_Mag_disp = PlotFunc(df = outlier_data.V1.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
                                       TITLE="Subject Averaged Control V1 GABA/Cr vs. RMS_Mag_disp",
                                       XLAB="RMS_Mag_disp",YLAB="GABA/Cr",CURVEDEGREE =3)
# Plotting control MFG RMS_Mag_disp vs GABA/Cr
fit3.C.MFG.GABA.RMS_Mag_disp = PlotFunc(df = subject_data.MFG.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
                                        TITLE="Subject Averaged Control MFG GABA/Cr vs. RMS_Mag_disp",
                                        XLAB="RMS_Mag_disp",YLAB="GABA/Cr",CURVEDEGREE = 3)
# Plotting control V1 RMS_Mag_speed vs GABA/Cr
fit3.C.V1.GABA.RMS_Mag_speed = PlotFunc(df = outlier_subject_data.V1.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
                                        TITLE="Subject Averaged Control V1 GABA/Cr vs. RMS_Mag_speed",
                                        XLAB="RMS_Mag_speed",YLAB="GABA/Cr",CURVEDEGREE = 2)
# Plotting control MFG RMS_Mag_speed vs GABA/Cr
fit3.C.MFG.GABA.RMS_Mag_speed = PlotFunc(df = subject_data.MFG.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
                                         TITLE="Subject Averaged Control MFG GABA/Cr vs. RMS_Mag_speed",
                                         XLAB="RMS_Mag_speed",YLAB="GABA/Cr", LEGENDNAME="Controls",CURVEDEGREE =3)

# ************************* LOESS 2nd Degree *****************************************************
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
loess2.C.V1.GABA.RMS_Mag_disp = PlotFunc2(df = subject_data.V1.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
                                       TITLE="Subject Averaged Control V1 GABA/Cr vs. RMS_Mag_disp",
                                       XLAB="RMS_Mag_disp",YLAB="GABA/Cr",CURVEDEGREE =2)
# Plotting control MFG RMS_Mag_disp vs GABA/Cr
loess2.C.MFG.GABA.RMS_Mag_disp = PlotFunc2(df = subject_data.MFG.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
                                        TITLE="Subject Averaged Control MFG GABA/Cr vs. RMS_Mag_disp",
                                        XLAB="RMS_Mag_disp",YLAB="GABA/Cr",CURVEDEGREE = 2)
# Plotting control V1 RMS_Mag_speed vs GABA/Cr
loess2.C.V1.GABA.RMS_Mag_speed = PlotFunc2(df = subject_data.V1.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
                                        TITLE="Subject Averaged Control V1 GABA/Cr vs. RMS_Mag_speed",
                                        XLAB="RMS_Mag_speed",YLAB="GABA/Cr",CURVEDEGREE = 2)
# Plotting control MFG RMS_Mag_speed vs GABA/Cr
loess2.C.MFG.GABA.RMS_Mag_speed = PlotFunc2(df = subject_data.MFG.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
                                         TITLE="Subject Averaged Control MFG GABA/Cr vs. RMS_Mag_speed",
                                         XLAB="RMS_Mag_speed",YLAB="GABA/Cr", LEGENDNAME="Controls",CURVEDEGREE =2)

# *********************** compare models ****************************************************************

# RMS_Mag_disp: nothing significant
lm.C.V1.GABA.RMS_Mag_disp = lm(GABA.Cr ~ RMS_Mag_disp, data = subject_data.V1.C)
fit2.C.V1.GABA.RMS_Mag_disp = lm(GABA.Cr ~ poly(RMS_Mag_disp, 2), data = subject_data.V1.C)
fit3.C.V1.GABA.RMS_Mag_disp = lm(GABA.Cr ~ poly(RMS_Mag_disp, 3), data = subject_data.V1.C)
fit4.C.V1.GABA.RMS_Mag_disp = lm(GABA.Cr ~ poly(RMS_Mag_disp, 4), data = subject_data.V1.C)
fit5.C.V1.GABA.RMS_Mag_disp = lm(GABA.Cr ~ poly(RMS_Mag_disp, 5), data = subject_data.V1.C)
anova(lm.C.V1.GABA.RMS_Mag_disp, fit2.C.V1.GABA.RMS_Mag_disp, fit3.C.V1.GABA.RMS_Mag_disp, fit4.C.V1.GABA.RMS_Mag_disp, fit5.C.V1.GABA.RMS_Mag_disp)


# Model 1: GABA.Cr ~ RMS_Mag_disp
# Model 2: GABA.Cr ~ poly(RMS_Mag_disp, 2)
# Model 3: GABA.Cr ~ poly(RMS_Mag_disp, 3)
# Model 4: GABA.Cr ~ poly(RMS_Mag_disp, 4)
# Model 5: GABA.Cr ~ poly(RMS_Mag_disp, 5)
#   Res.Df       RSS Df  Sum of Sq      F Pr(>F)
# 1     27 0.0053334                            
# 2     26 0.0051928  1 1.4063e-04 0.6599 0.4249
# 3     25 0.0049260  1 2.6676e-04 1.2516 0.2748
# 4     24 0.0049229  1 3.1310e-06 0.0147 0.9046
# 5     23 0.0049020  1 2.0969e-05 0.0984 0.7566


# RMS_Mag_speed
lm.C.V1.GABA.RMS_Mag_speed = lm(GABA.Cr ~ RMS_Mag_speed, data = subject_data.V1.C)
fit2.C.V1.GABA.RMS_Mag_speed = lm(GABA.Cr ~ poly(RMS_Mag_speed, 2), data = subject_data.V1.C)
fit3.C.V1.GABA.RMS_Mag_speed = lm(GABA.Cr ~ poly(RMS_Mag_speed, 3), data = subject_data.V1.C)# Best fit
fit4.C.V1.GABA.RMS_Mag_speed = lm(GABA.Cr ~ poly(RMS_Mag_speed, 4), data = subject_data.V1.C)
fit5.C.V1.GABA.RMS_Mag_speed = lm(GABA.Cr ~ poly(RMS_Mag_speed, 5), data = subject_data.V1.C)
anova(lm.C.V1.GABA.RMS_Mag_speed,fit2.C.V1.GABA.RMS_Mag_speed, fit3.C.V1.GABA.RMS_Mag_speed, fit4.C.V1.GABA.RMS_Mag_speed, fit5.C.V1.GABA.RMS_Mag_speed)


# Model 1: GABA.Cr ~ RMS_Mag_speed
# Model 2: GABA.Cr ~ poly(RMS_Mag_speed, 2)
# Model 3: GABA.Cr ~ poly(RMS_Mag_speed, 3)
# Model 4: GABA.Cr ~ poly(RMS_Mag_speed, 4)
# Model 5: GABA.Cr ~ poly(RMS_Mag_speed, 5)
#   Res.Df       RSS Df  Sum of Sq      F  Pr(>F)  
# 1     27 0.0046799                               
# 2     26 0.0040794  1 0.00060058 4.2713 0.05020 .
# 3     25 0.0033513  1 0.00072802 5.1777 0.03252 *
# 4     24 0.0032913  1 0.00006005 0.4270 0.51992  
# 5     23 0.0032340  1 0.00005730 0.4075 0.52953  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(fit3.C.V1.GABA.RMS_Mag_speed)
# Call:
#   lm(formula = GABA.Cr ~ poly(RMS_Mag_speed, 3), data = subject_data.V1.C)
# 
# Residuals:
#        Min        1Q    Median        3Q       Max 
# -0.020628 -0.005071  0.002999  0.006849  0.022187 
# 
# Coefficients:
#                         Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)              0.14554    0.00215  67.693   <2e-16 ***
#   poly(RMS_Mag_speed, 3)1 -0.02830    0.01158  -2.444   0.0219 *  
#   poly(RMS_Mag_speed, 3)2  0.02451    0.01158   2.117   0.0444 *  
#   poly(RMS_Mag_speed, 3)3  0.02698    0.01158   2.330   0.0282 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.01158 on 25 degrees of freedom
# Multiple R-squared:  0.3885,  Adjusted R-squared:  0.3151 
# F-statistic: 5.295 on 3 and 25 DF,  p-value: 0.005775


