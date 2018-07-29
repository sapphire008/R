# Linear Mixed Models and Genearlized Linear Mixed Model
# ===============================================================================================
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


####################################################################################################
# PART I: Simple Linear Regression
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&   Subject level &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
sink(file = paste(result_save_dir,"Simple_Linear_Regression.txt"))
cat("+++++++++++++++++++++++++++++++++++++++++++ GABA.Cr +++++++++++++++++++++++++++++++++++++++++++\n")
load(paste(imported_raw_data_dir,Item.raw_subject_data_frames,sep=""))
cat("=========================Subject Raw GABA vs. RMS_Mag_disp Controls V1=========================\n")
lm.Raw.V1.GABA.RMS_Mag_disp = lm(formula = GABA.Cr ~ RMS_Mag_disp, data = subject_data.V1.C)
summary(lm.Raw.V1.GABA.RMS_Mag_disp)
cat("=========================Subject Raw GABA vs. RMS_mag_speed Controls V1=========================\n")
lm.Raw.V1.GABA.RMS_Mag_speed = lm(formula = GABA.Cr ~ RMS_Mag_speed, data = subject_data.V1.C)
summary(lm.Raw.V1.GABA.RMS_Mag_speed)
load(paste(imported_clean_data_dir,Item.clean_subject_data_frames,sep=""))
cat("=========================Subject No Outliers GABA vs. RMS_Mag_disp Controls V1=========================\n")
lm.Clean.V1.GABA.RMS_Mag_disp = lm(formula = GABA.Cr ~ RMS_Mag_disp, data = no_outlier_subject_data.V1.C)
summary(lm.Clean.V1.GABA.RMS_Mag_disp)
cat("=========================Subject No Outliers GABA vs. RMS_Mag_speed Controls V1=========================\n")
lm.Clean.V1.GABA.RMS_Mag_speed = lm(formula = GABA.Cr ~ RMS_Mag_speed, data = no_outlier_subject_data.V1.C)
summary(lm.Clean.V1.GABA.RMS_Mag_speed)
cat("+++++++++++++++++++++++++++++++++++++++++++ NAA.Cr +++++++++++++++++++++++++++++++++++++++++++\n")
load(paste(imported_raw_data_dir,Item.raw_subject_data_frames,sep=""))
cat("=========================Subject Raw NAA vs. RMS_Mag_disp Controls V1=========================\n")
lm.Raw.V1.NAA.RMS_Mag_disp = lm(formula = NAA.Cr ~ RMS_Mag_disp, data = subject_data.V1.C)
summary(lm.Raw.V1.NAA.RMS_Mag_disp)
cat("=========================Subject Raw NAA vs. RMS_mag_speed Controls V1=========================\n")
lm.Raw.V1.NAA.RMS_Mag_speed = lm(formula = NAA.Cr ~ RMS_Mag_speed, data = subject_data.V1.C)
summary(lm.Raw.V1.NAA.RMS_Mag_speed)
load(paste(imported_clean_data_dir,Item.clean_subject_data_frames,sep=""))
cat("=========================Subject No Outliers NAA vs. RMS_Mag_disp Controls V1=========================\n")
lm.Clean.V1.NAA.RMS_Mag_disp = lm(formula = NAA.Cr ~ RMS_Mag_disp, data = no_outlier_subject_data.V1.C)
summary(lm.Clean.V1.NAA.RMS_Mag_disp)
cat("=========================Subject No Outliers NAA vs. RMS_Mag_speed Controls V1=========================\n")
lm.Clean.V1.NAA.RMS_Mag_speed = lm(formula = NAA.Cr ~ RMS_Mag_speed, data = no_outlier_subject_data.V1.C)
summary(lm.Clean.V1.NAA.RMS_Mag_speed)
cat("+++++++++++++++++++++++++++++++++++++++++++ Glx.Cr +++++++++++++++++++++++++++++++++++++++++++\n")
load(paste(imported_raw_data_dir,Item.raw_subject_data_frames,sep=""))
cat("=========================Subject Raw Glx vs. RMS_Mag_disp Controls V1=========================\n")
lm.Raw.V1.Glx.RMS_Mag_disp = lm(formula = Glx.Cr ~ RMS_Mag_disp, data = subject_data.V1.C)
summary(lm.Raw.V1.Glx.RMS_Mag_disp)
cat("=========================Subject Raw Glx vs. RMS_mag_speed Controls V1=========================\n")
lm.Raw.V1.Glx.RMS_Mag_speed = lm(formula = Glx.Cr ~ RMS_Mag_speed, data = subject_data.V1.C)
summary(lm.Raw.V1.Glx.RMS_Mag_speed)
load(paste(imported_clean_data_dir,Item.clean_subject_data_frames,sep=""))
cat("=========================Subject No Outliers Glx vs. RMS_Mag_disp Controls V1=========================\n")
lm.Clean.V1.Glx.RMS_Mag_disp = lm(formula = Glx.Cr ~ RMS_Mag_disp, data = no_outlier_subject_data.V1.C)
summary(lm.Clean.V1.Glx.RMS_Mag_disp)
cat("=========================Subject No Outliers Glx vs. RMS_Mag_speed Controls V1=========================\n")
lm.Clean.V1.Glx.RMS_Mag_speed = lm(formula = Glx.Cr ~ RMS_Mag_speed, data = no_outlier_subject_data.V1.C)
summary(lm.Clean.V1.Glx.RMS_Mag_speed)
sink()


####################################################################################################
# PART II: Linear Mixed Model
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& Run level NLME &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library(nlme)
sink(file = paste(result_save_dir, "Linear_Mixed_Model.txt"))
load(paste(imported_raw_data_dir,Item.raw_data_frames,sep=""))
cat("=========================Raw GABA vs. RMS_Mag_disp Controls V1=========================\n")
lme.Raw.V1.GABA.RMS_Mag_disp = lme(fixed = GABA.Cr ~ RMS_Mag_disp, random = (~1|Subject_ID), data = data.V1.C)
summary(lme.Raw.V1.GABA.RMS_Mag_disp)
cat("=========================Raw GABA vs. RMS_mag_speed Controls V1=========================\n")
lme.Raw.V1.GABA.RMS_Mag_speed = lme(fixed = GABA.Cr ~ RMS_Mag_speed, random =  (~1|Subject_ID), data = data.V1.C)
summary(lme.Raw.V1.GABA.RMS_Mag_speed)
load(paste(imported_clean_data_dir,Item.clean_data_frames,sep=""))
cat("=========================No Outliers GABA vs. RMS_Mag_disp Controls V1=========================\n")
lme.Clean.V1.GABA.RMS_Mag_disp = lme(fixed = GABA.Cr ~ RMS_Mag_disp,  random = (~1|Subject_ID), data = no_outlier_data.V1.C,na.action = na.pass)
summary(lme.Clean.V1.GABA.RMS_Mag_disp)
cat("=========================No Outliers GABA vs. RMS_Mag_speed Controls V1=========================\n")
lme.Clean.V1.GABA.RMS_Mag_speed = lme(fixed = GABA.Cr ~ RMS_Mag_speed, random = (~1|Subject_ID), data = no_outlier_data.V1.C, na.action = na.pass)
summary(lme.Clean.V1.GABA.RMS_Mag_speed)
detach("package:nlme",unload=TRUE)
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& Run level LME4 &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library(lme4)
load(paste(imported_raw_data_dir,Item.raw_data_frames,sep=""))
cat("=========================Raw GABA vs. RMS_Mag_disp Controls V1=========================\n")
lme.Raw.V1.GABA.RMS_Mag_disp = lmer(formula = GABA.Cr ~ RMS_Mag_disp + (1|Subject_ID), data = data.V1.C)
summary(lme.Raw.V1.GABA.RMS_Mag_disp)
cat("=========================Raw GABA vs. RMS_mag_speed Controls V1=========================\n")
lme.Raw.V1.GABA.RMS_Mag_speed = lmer(formula = GABA.Cr ~ RMS_Mag_speed +  (1|Subject_ID), data = data.V1.C)
summary(lme.Raw.V1.GABA.RMS_Mag_speed)
load(paste(imported_clean_data_dir,Item.clean_data_frames,sep=""))
cat("=========================No Outliers GABA vs. RMS_Mag_disp Controls V1=========================\n")
lme.Clean.V1.GABA.RMS_Mag_disp = lmer(formula = GABA.Cr ~ RMS_Mag_disp +  (1|Subject_ID), data = no_outlier_data.V1.C)
summary(lme.Clean.V1.GABA.RMS_Mag_disp)
cat("=========================No Outliers GABA vs. RMS_Mag_speed Controls V1=========================\n")
lme.Clean.V1.GABA.RMS_Mag_speed = lmer(formula = GABA.Cr ~ RMS_Mag_speed +  (1|Subject_ID), data = no_outlier_data.V1.C)
summary(lme.Clean.V1.GABA.RMS_Mag_speed)
sink()


######################################################################################################
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&& Run level LME4 &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
library(lme4)
sink(file = paste(result_save_dir, "Linear_Mixed_Model_2.txt"))
load(paste(imported_raw_data_dir,Item.raw_data_frames,sep=""))
cat("=========================Raw GABA vs. RMS_Mag_disp Controls V1=========================\n")
lme.Raw.V1.GABA.RMS_Mag_disp = lmer(formula = GABA.Cr ~ RMS_Mag_disp + (1+Region_run_order|Subject_ID), data = data.V1.C)
summary(lme.Raw.V1.GABA.RMS_Mag_disp)
cat("=========================Raw GABA vs. RMS_mag_speed Controls V1=========================\n")
lme.Raw.V1.GABA.RMS_Mag_speed = lmer(formula = GABA.Cr ~ RMS_Mag_speed +  (1+Region_run_order|Subject_ID), data = data.V1.C)
summary(lme.Raw.V1.GABA.RMS_Mag_speed)
load(paste(imported_clean_data_dir,Item.clean_data_frames,sep=""))
cat("=========================No Outliers GABA vs. RMS_Mag_disp Controls V1=========================\n")
lme.Clean.V1.GABA.RMS_Mag_disp = lmer(formula = GABA.Cr ~ RMS_Mag_disp +  (1+Region_run_order|Subject_ID), data = no_outlier_data.V1.C)
summary(lme.Clean.V1.GABA.RMS_Mag_disp)
cat("=========================No Outliers GABA vs. RMS_Mag_speed Controls V1=========================\n")
lme.Clean.V1.GABA.RMS_Mag_speed = lmer(formula = GABA.Cr ~ RMS_Mag_speed +  (1+Region_run_order|Subject_ID), data = no_outlier_data.V1.C)
summary(lme.Clean.V1.GABA.RMS_Mag_speed)
sink()
































