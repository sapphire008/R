#GABA Spectroscopy Movement Analysis 3: models and intepretaion
# Raw Data
imported_raw_data_dir = "/nfs/r21_gaba/reprocessing/analysis/imported_data/Set01/"
Item.raw_data_frames = "R_imported_data_frames.Rda"
Item.raw_data_list = "R_imported_data_list.Rda"
Item.raw_subject_data_frames = "R_imported_subject_data_frames.Rda"
Item.raw_subject_data_list = "R_imported_subject_data_list.Rda"
# Data removed outliers
imported_clean_data_dir = "/nfs/r21_gaba/reprocessing/analysis/imported_data/Set03_excluded_outliers/"
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
PlotFunc <-function(df,X,Y,G,TITLE="",LEGENDNAME=NULL,XLAB="X",YLAB="Y",XTRANSFORM=NULL, YTRANSFORM=NULL, ZOOM=2.0) {
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
}

######################################################################################################
##################################        Run Level    ###############################################
######################################################################################################

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&   RAW DATA     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# load raw data
load(paste(imported_raw_data_dir,Item.raw_data_frames,sep=""))
df = data.V1.C; X="RMS_Mag_speed";Y = "GABA.Cr";G = "Subject_ID"; 
TITLE="Control V1 GABA/Cr vs. RMS_Mag_disp";GROUPNAME="Subjects"
XLAB="RMS_Mag_disp";YLAB="GABA/Cr";ZOOM=1.0;
# set up layout
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)

# Plotting control V1 RMS_Mag_disp vs GABA/Cr
PlotFunc(df = data.V1.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control V1 GABA/Cr vs. RMS_Mag_disp",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_disp vs GABA/Cr
PlotFunc(df = data.MFG.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control MFG GABA/Cr vs. RMS_Mag_disp",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control V1 RMS_Mag_speed vs GABA/Cr
PlotFunc(df = data.V1.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control V1 GABA/Cr vs. RMS_Mag_speed",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_speed vs GABA/Cr
PlotFunc(df = data.MFG.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control MFG GABA/Cr vs. RMS_Mag_speed",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr", LEGENDNAME="Controls")
##********************************************************************************************
# Plotting patient V1 RMS_Mag_disp vs GABA/Cr
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
PlotFunc(df = data.V1.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients V1 GABA/Cr vs. RMS_Mag_disp",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_disp vs GABA/Cr
PlotFunc(df = data.MFG.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients MFG GABA/Cr vs. RMS_Mag_disp",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient V1 RMS_Mag_speed vs GABA/Cr
PlotFunc(df = data.V1.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients V1 GABA/Cr vs. RMS_Mag_speed",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_speed vs GABA/Cr
PlotFunc(df = data.MFG.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients MFG GABA/Cr vs. RMS_Mag_speed",LEGENDNAME="Patients",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&   No Outliers   &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# load cleaned data
load(paste(imported_clean_data_dir,Item.clean_data_frames,sep=""))
# Plot Controls
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
# Plotting control V1 RMS_Mag_disp vs GABA/Cr
PlotFunc(df = no_outlier_data.V1.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control V1 GABA/Cr vs. RMS_Mag_disp No outliers",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_disp vs GABA/Cr
PlotFunc(df = no_outlier_data.MFG.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control MFG GABA/Cr vs. RMS_Mag_disp No outliers",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control V1 RMS_Mag_speed vs GABA/Cr
PlotFunc(df = no_outlier_data.V1.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control V1 GABA/Cr vs. RMS_Mag_speed No outliers",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_speed vs GABA/Cr
PlotFunc(df = no_outlier_data.MFG.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control MFG GABA/Cr vs. RMS_Mag_speed No outliers",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr", LEGENDNAME="Controls")
## ********************************************************************************************
# Plot Patients
# Plotting patient V1 RMS_Mag_disp vs GABA/Cr WITHOUT outliers
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
PlotFunc(df = no_outlier_data.V1.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients V1 GABA/Cr vs. RMS_Mag_disp No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_disp vs GABA/Cr WITHOUT outliers
PlotFunc(df = no_outlier_data.MFG.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients MFG GABA/Cr vs. RMS_Mag_disp No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient V1 RMS_Mag_speed vs GABA/Cr WITHOUT outliers
PlotFunc(df = no_outlier_data.V1.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients V1 GABA/Cr vs. RMS_Mag_speed No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_speed vs GABA/Cr WITHOUT outliers
PlotFunc(df = no_outlier_data.MFG.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients MFG GABA/Cr vs. RMS_Mag_speed No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr",LEGENDNAME="Patients")

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& Log Transformed  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Plot Controls
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
# Plotting control V1 RMS_Mag_disp vs GABA/Cr
PlotFunc(df = data.V1.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control V1 GABA/Cr vs. log(RMS_Mag_disp)", XTRANSFORM = log,
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_disp vs GABA/Cr
PlotFunc(df = data.MFG.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control MFG GABA/Cr vs. log(RMS_Mag_disp)", XTRANSFORM = log,
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control V1 RMS_Mag_speed vs GABA/Cr
PlotFunc(df = data.V1.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control V1 GABA/Cr vs. log(RMS_Mag_speed)", XTRANSFORM = log,
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_speed vs GABA/Cr
PlotFunc(df = data.MFG.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control MFG GABA/Cr vs. log(RMS_Mag_speed)", XTRANSFORM = log,
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr", LEGENDNAME="Controls")
## ********************************************************************************************
# Plot Patients
# Plotting patient V1 RMS_Mag_disp vs GABA/Cr WITHOUT outliers
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
PlotFunc(df = data.V1.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients V1 GABA/Cr vs. log(RMS_Mag_disp)",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_disp vs GABA/Cr WITHOUT outliers
PlotFunc(df = data.MFG.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients MFG GABA/Cr vs. log(RMS_Mag_disp)",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient V1 RMS_Mag_speed vs GABA/Cr WITHOUT outliers
PlotFunc(df = data.V1.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients V1 GABA/Cr vs. log(RMS_Mag_speed)",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_speed vs GABA/Cr WITHOUT outliers
PlotFunc(df = data.MFG.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients MFG GABA/Cr vs. log(RMS_Mag_speed)",LEGENDNAME="Patients",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")

# &&&&&&&&&&&&&&&&&&&&&&&&&&&& No Outliers Log Transformed  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Plot Controls
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
# Plotting control V1 RMS_Mag_disp vs GABA/Cr
PlotFunc(df = no_outlier_data.V1.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control V1 GABA/Cr vs. log(RMS_Mag_disp) No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_disp vs GABA/Cr
PlotFunc(df = no_outlier_data.MFG.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control MFG GABA/Cr vs. log(RMS_Mag_disp) No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control V1 RMS_Mag_speed vs GABA/Cr
PlotFunc(df = no_outlier_data.V1.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control V1 GABA/Cr vs. log(RMS_Mag_speed) No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_speed vs GABA/Cr
PlotFunc(df = no_outlier_data.MFG.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control MFG GABA/Cr vs. log(RMS_Mag_speed) No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr", LEGENDNAME="Controls")
## ********************************************************************************************
# Plot Patients
# Plotting patient V1 RMS_Mag_disp vs GABA/Cr WITHOUT outliers
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
PlotFunc(df = no_outlier_data.V1.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients V1 GABA/Cr vs. log(RMS_Mag_disp) No outliers",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_disp vs GABA/Cr WITHOUT outliers
PlotFunc(df = no_outlier_data.MFG.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients MFG GABA/Cr vs. log(RMS_Mag_disp) No outliers",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient V1 RMS_Mag_speed vs GABA/Cr WITHOUT outliers
PlotFunc(df = no_outlier_data.V1.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients V1 GABA/Cr vs. log(RMS_Mag_speed) No outliers",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_speed vs GABA/Cr WITHOUT outliers
PlotFunc(df = no_outlier_data.MFG.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients MFG GABA/Cr vs. log(RMS_Mag_speed) No outliers",LEGENDNAME="Patients",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")

######################################################################################################
##################################    Subject Level    ###############################################
######################################################################################################

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&   RAW DATA     &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# load raw data
load(paste(imported_raw_data_dir,Item.raw_subject_data_frames,sep=""))

# set up layout
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
# Plotting control V1 RMS_Mag_disp vs GABA/Cr
PlotFunc(df = subject_data.V1.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Control V1 GABA/Cr vs. RMS_Mag_disp",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_disp vs GABA/Cr
PlotFunc(df = subject_data.MFG.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Control MFG GABA/Cr vs. RMS_Mag_disp",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control V1 RMS_Mag_speed vs GABA/Cr
PlotFunc(df = subject_data.V1.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Control V1 GABA/Cr vs. RMS_Mag_speed",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_speed vs GABA/Cr
PlotFunc(df = subject_data.MFG.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Control MFG GABA/Cr vs. RMS_Mag_speed",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr", LEGENDNAME="Controls")
##********************************************************************************************
# Plotting patient V1 RMS_Mag_disp vs GABA/Cr
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
PlotFunc(df = subject_data.V1.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Patients V1 GABA/Cr vs. RMS_Mag_disp",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_disp vs GABA/Cr
PlotFunc(df = subject_data.MFG.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Patients MFG GABA/Cr vs. RMS_Mag_disp",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient V1 RMS_Mag_speed vs GABA/Cr
PlotFunc(df = subject_data.V1.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Patients V1 GABA/Cr vs. RMS_Mag_speed",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_speed vs GABA/Cr
PlotFunc(df = subject_data.MFG.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Patients MFG GABA/Cr vs. RMS_Mag_speed",LEGENDNAME="Patients",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&   No Outliers   &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# load cleaned data
load(paste(imported_clean_data_dir,Item.clean_subject_data_frames,sep=""))
# Plot Controls
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
# Plotting control V1 RMS_Mag_disp vs GABA/Cr
PlotFunc(df = no_outlier_subject_data.V1.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control V1 GABA/Cr vs. RMS_Mag_disp No outliers",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_disp vs GABA/Cr
PlotFunc(df = no_outlier_subject_data.MFG.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control MFG GABA/Cr vs. RMS_Mag_disp No outliers",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control V1 RMS_Mag_speed vs GABA/Cr
PlotFunc(df = no_outlier_subject_data.V1.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control V1 GABA/Cr vs. RMS_Mag_speed No outliers",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_speed vs GABA/Cr
PlotFunc(df = no_outlier_subject_data.MFG.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control MFG GABA/Cr vs. RMS_Mag_speed No outliers",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr", LEGENDNAME="Controls")
## ********************************************************************************************
# Plot Patients
# Plotting patient V1 RMS_Mag_disp vs GABA/Cr WITHOUT outliers
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
PlotFunc(df = no_outlier_subject_data.V1.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients V1 GABA/Cr vs. RMS_Mag_disp No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_disp vs GABA/Cr WITHOUT outliers
PlotFunc(df = no_outlier_subject_data.MFG.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients MFG GABA/Cr vs. RMS_Mag_disp No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient V1 RMS_Mag_speed vs GABA/Cr WITHOUT outliers
PlotFunc(df = no_outlier_subject_data.V1.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients V1 GABA/Cr vs. RMS_Mag_speed No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_speed vs GABA/Cr WITHOUT outliers
PlotFunc(df = no_outlier_subject_data.MFG.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients MFG GABA/Cr vs. RMS_Mag_speed No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr",LEGENDNAME="Patients")

# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& Log Transformed  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Plot Controls
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
# Plotting control V1 RMS_Mag_disp vs GABA/Cr
PlotFunc(df = subject_data.V1.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control V1 GABA/Cr vs. log(RMS_Mag_disp)", XTRANSFORM = log,
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_disp vs GABA/Cr
PlotFunc(df = subject_data.MFG.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control MFG GABA/Cr vs. log(RMS_Mag_disp)", XTRANSFORM = log,
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control V1 RMS_Mag_speed vs GABA/Cr
PlotFunc(df = subject_data.V1.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control V1 GABA/Cr vs. log(RMS_Mag_speed)", XTRANSFORM = log,
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_speed vs GABA/Cr
PlotFunc(df = subject_data.MFG.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control MFG GABA/Cr vs. log(RMS_Mag_speed)", XTRANSFORM = log,
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr", LEGENDNAME="Controls")
## ********************************************************************************************
# Plot Patients
# Plotting patient V1 RMS_Mag_disp vs GABA/Cr WITHOUT outliers
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
PlotFunc(df = subject_data.V1.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients V1 GABA/Cr vs. log(RMS_Mag_disp)",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_disp vs GABA/Cr WITHOUT outliers
PlotFunc(df = subject_data.MFG.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients MFG GABA/Cr vs. log(RMS_Mag_disp)",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient V1 RMS_Mag_speed vs GABA/Cr WITHOUT outliers
PlotFunc(df = subject_data.V1.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients V1 GABA/Cr vs. log(RMS_Mag_speed)",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_speed vs GABA/Cr WITHOUT outliers
PlotFunc(df = subject_data.MFG.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients MFG GABA/Cr vs. log(RMS_Mag_speed)",LEGENDNAME="Patients",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")

# &&&&&&&&&&&&&&&&&&&&&&&&&&&& No Outliers Log Transformed  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Plot Controls
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
# Plotting control V1 RMS_Mag_disp vs GABA/Cr
PlotFunc(df = no_outlier_subject_data.V1.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control V1 GABA/Cr vs. log(RMS_Mag_disp) No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_disp vs GABA/Cr
PlotFunc(df = no_outlier_subject_data.MFG.C, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control MFG GABA/Cr vs. log(RMS_Mag_disp) No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting control V1 RMS_Mag_speed vs GABA/Cr
PlotFunc(df = no_outlier_subject_data.V1.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control V1 GABA/Cr vs. log(RMS_Mag_speed) No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting control MFG RMS_Mag_speed vs GABA/Cr
PlotFunc(df = no_outlier_subject_data.MFG.C, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Control MFG GABA/Cr vs. log(RMS_Mag_speed) No outliers", XTRANSFORM = log,
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr", LEGENDNAME="Controls")
## ********************************************************************************************
# Plot Patients
# Plotting patient V1 RMS_Mag_disp vs GABA/Cr WITHOUT outliers
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
PlotFunc(df = no_outlier_subject_data.V1.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients V1 GABA/Cr vs. log(RMS_Mag_disp) No outliers",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_disp vs GABA/Cr WITHOUT outliers
PlotFunc(df = no_outlier_subject_data.MFG.SZ, X="RMS_Mag_disp",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients MFG GABA/Cr vs. log(RMS_Mag_disp) No outliers",
         XLAB="RMS_Mag_disp",YLAB="GABA/Cr")
# Plotting patient V1 RMS_Mag_speed vs GABA/Cr WITHOUT outliers
PlotFunc(df = no_outlier_subject_data.V1.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients V1 GABA/Cr vs. log(RMS_Mag_speed) No outliers",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")
# Plotting patient MFG RMS_Mag_speed vs GABA/Cr WITHOUT outliers
PlotFunc(df = no_outlier_subject_data.MFG.SZ, X="RMS_Mag_speed",Y = "GABA.Cr",G = "Subject_ID",
         TITLE="Patients MFG GABA/Cr vs. log(RMS_Mag_speed) No outliers",LEGENDNAME="Patients",
         XLAB="RMS_Mag_speed",YLAB="GABA/Cr")



################################ NAA.Cr plots #################################################

######################################################################################################
######################################    Run Level    ###############################################
######################################################################################################
load(paste(imported_raw_data_dir,Item.raw_data_frames,sep=""))
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& RAW  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# set up layout
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)

# Plotting control V1 RMS_Mag_disp vs NAA/Cr
PlotFunc(df = data.V1.C, X="RMS_Mag_disp",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Control V1 NAA/Cr vs. RMS_Mag_disp",
         XLAB="RMS_Mag_disp",YLAB="NAA/Cr")
# Plotting control MFG RMS_Mag_disp vs NAA/Cr
PlotFunc(df = data.MFG.C, X="RMS_Mag_disp",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Control MFG NAA/Cr vs. RMS_Mag_disp",
         XLAB="RMS_Mag_disp",YLAB="NAA/Cr")
# Plotting control V1 RMS_Mag_speed vs NAA/Cr
PlotFunc(df = data.V1.C, X="RMS_Mag_speed",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Control V1 NAA/Cr vs. RMS_Mag_speed",
         XLAB="RMS_Mag_speed",YLAB="NAA/Cr")
# Plotting control MFG RMS_Mag_speed vs NAA/Cr
PlotFunc(df = data.MFG.C, X="RMS_Mag_speed",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Control MFG NAA/Cr vs. RMS_Mag_speed",
         XLAB="RMS_Mag_speed",YLAB="NAA/Cr", LEGENDNAME="Controls")
load(paste(imported_clean_data_dir,Item.clean_data_frames,sep=""))
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& No Outliers  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# Plot Controls
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
# Plotting control V1 RMS_Mag_disp vs NAA/Cr
PlotFunc(df = no_outlier_data.V1.C, X="RMS_Mag_disp",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Control V1 NAA/Cr vs. RMS_Mag_disp No outliers",
         XLAB="RMS_Mag_disp",YLAB="NAA/Cr")
# Plotting control MFG RMS_Mag_disp vs NAA/Cr
PlotFunc(df = no_outlier_data.MFG.C, X="RMS_Mag_disp",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Control MFG NAA/Cr vs. RMS_Mag_disp No outliers",
         XLAB="RMS_Mag_disp",YLAB="NAA/Cr")
# Plotting control V1 RMS_Mag_speed vs NAA/Cr
PlotFunc(df = no_outlier_data.V1.C, X="RMS_Mag_speed",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Control V1 NAA/Cr vs. RMS_Mag_speed No outliers",
         XLAB="RMS_Mag_speed",YLAB="NAA/Cr")
# Plotting control MFG RMS_Mag_speed vs NAA/Cr
PlotFunc(df = no_outlier_data.MFG.C, X="RMS_Mag_speed",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Control MFG NAA/Cr vs. RMS_Mag_speed No outliers",
         XLAB="RMS_Mag_speed",YLAB="NAA/Cr", LEGENDNAME="Controls")



######################################################################################################
######################################    Subject Level    ###########################################
######################################################################################################
load(paste(imported_raw_data_dir,Item.raw_subject_data_frames,sep=""))
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& Raw &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# set up layout
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
# Plotting control V1 RMS_Mag_disp vs NAA/Cr
PlotFunc(df = subject_data.V1.C, X="RMS_Mag_disp",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Control V1 NAA/Cr vs. RMS_Mag_disp",
         XLAB="RMS_Mag_disp",YLAB="NAA/Cr")
# Plotting control MFG RMS_Mag_disp vs NAA/Cr
PlotFunc(df = subject_data.MFG.C, X="RMS_Mag_disp",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Control MFG NAA/Cr vs. RMS_Mag_disp",
         XLAB="RMS_Mag_disp",YLAB="NAA/Cr")
# Plotting control V1 RMS_Mag_speed vs NAA/Cr
PlotFunc(df = subject_data.V1.C, X="RMS_Mag_speed",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Control V1 NAA/Cr vs. RMS_Mag_speed",
         XLAB="RMS_Mag_speed",YLAB="NAA/Cr")
# Plotting control MFG RMS_Mag_speed vs NAA/Cr
PlotFunc(df = subject_data.MFG.C, X="RMS_Mag_speed",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Subject Averaged Control MFG NAA/Cr vs. RMS_Mag_speed",
         XLAB="RMS_Mag_speed",YLAB="NAA/Cr", LEGENDNAME="Controls")

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& No Outliers &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# load cleaned data
load(paste(imported_clean_data_dir,Item.clean_subject_data_frames,sep=""))
# Plot Controls
mat = matrix(c(1,1,1,1,2,2,2,2,5,3,3,3,3,4,4,4,4,5),nrow=2,ncol=9,byrow=TRUE)
nf = layout(mat = mat)
# Plotting control V1 RMS_Mag_disp vs NAA/Cr
PlotFunc(df = no_outlier_subject_data.V1.C, X="RMS_Mag_disp",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Control V1 NAA/Cr vs. RMS_Mag_disp No outliers",
         XLAB="RMS_Mag_disp",YLAB="NAA/Cr")
# Plotting control MFG RMS_Mag_disp vs NAA/Cr
PlotFunc(df = no_outlier_subject_data.MFG.C, X="RMS_Mag_disp",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Control MFG NAA/Cr vs. RMS_Mag_disp No outliers",
         XLAB="RMS_Mag_disp",YLAB="NAA/Cr")
# Plotting control V1 RMS_Mag_speed vs NAA/Cr
PlotFunc(df = no_outlier_subject_data.V1.C, X="RMS_Mag_speed",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Control V1 NAA/Cr vs. RMS_Mag_speed No outliers",
         XLAB="RMS_Mag_speed",YLAB="NAA/Cr")
# Plotting control MFG RMS_Mag_speed vs NAA/Cr
PlotFunc(df = no_outlier_subject_data.MFG.C, X="RMS_Mag_speed",Y = "NAA.Cr",G = "Subject_ID",
         TITLE="Control MFG NAA/Cr vs. RMS_Mag_speed No outliers",
         XLAB="RMS_Mag_speed",YLAB="NAA/Cr", LEGENDNAME="Controls")