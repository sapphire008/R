#GABA Spectroscopy Movement Analysis 2: Summary and data exploration
imported_data_dir = "/Users/Edward/Documents/Assignments/Imaging Research Center/gaba_movement/Set04_clinical_raw_data/"
save_data_dir = "/Users/Edward/Documents/Assignments/Imaging Research Center/gaba_movement/Set05_clinical_excluded_outliers/"
result_save_dir = "/Users/Edward/Documents/Assignments/Imaging Research Center/gaba_movement/results/"
Item.data_frames = "R_imported_dataframes.Rda"
Item.data_list = "R_imported_data_list.Rda"
Item.subject_data_frames = "R_imported_subject_dataframes.Rda"
Item.subject_data_list = "R_imported_subject_data_list.Rda"
Item.no_outlier_data_frames = "R_imported_no_outlier_dataframes.Rda"
Item.no_outlier_data_list = "R_imported_no_outlier_data_list.Rda"
Item.no_outlier_subject_data_frames = "R_imported_no_outlier_subject_dataframes.Rda"
Item.no_outlier_subject_data_list = "R_imported_no_outlier_subject_data_list.Rda"
# Defining numerical columns
test_cols = c("GABA.Cr","Glx.Cr","Gaba.AdjPeak","Glx.AdjPeak","Cr","Off.Res.Cr","NAA","NAA.Cr",
              "RMS_Mag_disp","RMS_Mag_speed","Magnitude.of.Displacement.mm.",
              "Magnitude.of.Speed.mm.","Jaggedness_X","Jaggedness_Y","RMS_X_disp",
              "RMS_Y_disp","RMS_X_speed","RMS_Y_speed")
load(paste(imported_data_dir,Item.data_frames,sep=""))


# A). Do summary statistics for each data set
load(paste(imported_data_dir,Item.data_list,sep=""))
load(paste(imported_data_dir,Item.subject_data_list,sep=""))
# save output to a file
sink(file = paste(result_save_dir,"data_list_Summary.txt",sep = ""))
lapply(data_list,summary,simplify = TRUE)
cat("======================================================================")
cat("\n\nsubject data\n")
lapply(subject_data_list,summary,simplify = TRUE)
sink()

# B). Test if each of the following columns are normal in each group of data
# local function that calculates which entires are not significant (therefore, normal)
normality_test_func <- function(input_data_list,test_cols=NULL,apply_transform=NULL,select_normal = TRUE,return_p_values = FALSE)  {
  # get subset of columns
  if(!is.null(test_cols))
    input_data_list = lapply(input_data_list, function(x) x[test_cols])
  if(!is.null(apply_transform))
    input_data_list = lapply(input_data_list, function(x) apply_transform(x))
  # do the shapiro test for each column listed above of each data set
  normality_test = lapply(input_data_list, function(x) lapply(x,shapiro.test))
  # find the p value
  normality_p_value = lapply(normality_test, function(x) lapply(x,function(x) x$p.value))
  #store as a matrix
  normality_p_value = simplify2array(normality_p_value)
  # selecting whether to use normal or non-normal returns
  if(select_normal)
    K=as.data.frame(which(normality_p_value>0.05,arr.ind=TRUE,useNames=TRUE))
  else
    K=as.data.frame(which(normality_p_value<=0.05,arr.ind=TRUE,useNames=TRUE))
  # get the name of selected returns
  RowNames = rownames(normality_p_value)
  ColNames = colnames(normality_p_value)
  normdist_entries = paste(ColNames[K$col],RowNames[K$row],sep = "$")
  # parse what to return, either the name of the entries, or the p-value matrix
  if(!return_p_values)
    return(normdist_entries)
  else
    return(normality_p_value)
}

sink(file = paste(result_save_dir,"data_normality_test_summary.txt",sep = ""))
# data / run level
cat("data run level: A list of data with significantly normal distribution\n")
normality_test_func(data_list,test_cols,select_normal = TRUE)
cat("\n")
# subject data level
cat("subject level: a list of subject_data with significantly normal distribution\n")  
normality_test_func(subject_data_list, test_cols, select_normal = TRUE)
cat("\n")

# log transformed data
cat("log transoformed data: A list of data with significantly normal distribution\n")
normality_test_func(data_list,test_cols, apply_transform = log, select_normal = TRUE)
cat("\n")
# log transformed subject_data
cat("log transformd subject data:  A list of data with significantly normal distribution\n")
normality_test_func(subject_data_list,test_cols,apply_transform = log, select_normal = TRUE)
cat("\n")
sink()

# None of the movement parameters are normally distributed. Need to do a transformation of the metrics, 
# otherwise cannot use Linear Mixed Model, in which we assume that variables are normally distributed;
# We must use Genearlized Linear Mixed Model, with no assumption of normality in the data

# C). T-test
load(paste(imported_data_dir,Item.subject_data_frames,sep=""))
test_cols = c("Groups","GABA.Cr","Glx.Cr","Gaba.AdjPeak","Glx.AdjPeak","Cr","Off.Res.Cr","NAA","NAA.Cr",
              "RMS_Mag_disp","RMS_Mag_speed","Magnitude.of.Displacement.mm.",
              "Magnitude.of.Speed.mm.","Jaggedness_X","Jaggedness_Y","RMS_X_disp",
              "RMS_Y_disp","RMS_X_speed","RMS_Y_speed")
# V1
df = subject_data.V1.All[test_cols]
df$Groups[df$Groups!="C"] = "SZ" #identify all none-controls as patients
T_Test = t(sapply(df[-1],function(x)
  unlist(t.test(x~df$Groups)[c("estimate","p.value","statistic","conf.int")])))
write.table(T_Test,file = paste(result_save_dir,"Between_group_V1_t_test.csv"),row.names = TRUE,col.names = TRUE, sep = ",")

# MFG
df = subject_data.MFG.All[test_cols]
df$Groups[df$Groups!="C"] = "SZ" #identify all none-controls as patients
T_Test = t(sapply(df[-1],function(x)
  unlist(t.test(x~df$Groups)[c("estimate","p.value","statistic","conf.int")])))
write.table(T_Test,file = paste(result_save_dir,"Between_group_MFG_t_test.csv"),row.names = TRUE,col.names = TRUE, sep = ",")

# D). Inspecting labeled movement and movement summary measure
load(paste(imported_data_dir,Item.data_frames,sep=""))
data.source = data.source[order(-data.source[,21]),]
data.source = data.source[order(-data.source[,22]),]
# indicated bad runs do not necessarily mean a lot of movement!

# E). Identifying outliers
outlier_func <- function (input_vect,vect_names) {
  names(input_vect) = vect_names
  outlier_list = boxplot(input_vect,plot=F)$out
  return(outlier_list)
}
sink(file = paste(result_save_dir,"data_identify_outliers.txt",sep = ""))
cat("V1 controls: \n")
sapply(data.V1.C[test_cols],outlier_func, paste(data.V1.C$Subject_ID,data.V1.C$Session_Runs,sep="_"))
cat("\n")
cat("V1 patients:\n")
sapply(data.V1.SZ[test_cols],outlier_func, paste(data.V1.SZ$Subject_ID,data.V1.SZ$Session_Runs,sep="_"))
cat("\n")
cat("MFG controls:\n")
sapply(data.MFG.C[test_cols],outlier_func, paste(data.MFG.C$Subject_ID,data.MFG.C$Session_Runs,sep="_"))
cat("\n")
cat("MFG patients:\n")
sapply(data.MFG.SZ[test_cols],outlier_func, paste(data.MFG.SZ$Subject_ID,data.MFG.SZ$Session_Runs,sep="_"))
cat("\n")
sink()

# F). Get the data frame without outlier:
#load("/nfs/r21_gaba/reprocessing/analysis/imported_data/Set01_raw_data/R_imported_data_frames.Rda")
remove_outlier<- function(x, mode = "") {
  # remove all the outliers. If the data is near normal distribution,
  # it should keep the data only within 2.7sigma within the mean, or
  #[Q1-1.5*IQR, Q3+1.5*IQR] non-parametrically
  # Boxplot is non-parametric, so it does not assume normality
  BP = boxplot(x,plot=FALSE)
  if(mode=="half"){
    # remove only the outliers greater than the median
    x[(x %in% BP$out[BP$out>BP$stats[3]])]=NA
  }
  else{
    # remove all outliers
    x[(x %in% BP$out)] = NA
  }
  # return x
  return(x)
}

spec_cols = c("GABA.Cr","Glx.Cr","Gaba.AdjPeak","Glx.AdjPeak","Cr","Off.Res.Cr","NAA","NAA.Cr")
move_cols = c("RMS_Mag_disp","RMS_Mag_speed","Magnitude.of.Displacement.mm.",
              "Magnitude.of.Speed.mm.","Jaggedness_X","Jaggedness_Y","RMS_X_disp",
              "RMS_Y_disp","RMS_X_speed","RMS_Y_speed")

no_outlier_data.V1.C = data.V1.C
no_outlier_data.V1.C[spec_cols] = sapply(no_outlier_data.V1.C[spec_cols], remove_outlier,mode = "")
no_outlier_data.V1.C[move_cols] = sapply(no_outlier_data.V1.C[move_cols], remove_outlier,mode = "half")

no_outlier_data.MFG.C = data.MFG.C
no_outlier_data.MFG.C[spec_cols] = sapply(no_outlier_data.MFG.C[spec_cols], remove_outlier,mode = "")
no_outlier_data.MFG.C[move_cols] = sapply(no_outlier_data.MFG.C[move_cols], remove_outlier,mode = "half")

no_outlier_data.V1.SZ = data.V1.SZ
no_outlier_data.V1.SZ[spec_cols] = sapply(no_outlier_data.V1.SZ[spec_cols], remove_outlier,mode = "")
no_outlier_data.V1.SZ[move_cols] = sapply(no_outlier_data.V1.SZ[move_cols], remove_outlier,mode = "half")

no_outlier_data.MFG.SZ = data.MFG.SZ
no_outlier_data.MFG.SZ[spec_cols] = sapply(no_outlier_data.MFG.SZ[spec_cols], remove_outlier,mode = "")
no_outlier_data.MFG.SZ[move_cols] = sapply(no_outlier_data.MFG.SZ[move_cols], remove_outlier,mode = "half")

no_outlier_data.V1.All = rbind(no_outlier_data.V1.C,no_outlier_data.V1.SZ)
no_outlier_data.MFG.All = rbind(no_outlier_data.MFG.C,no_outlier_data.MFG.SZ)

no_outlier_data.controls = rbind(no_outlier_data.V1.C,no_outlier_data.MFG.C)
no_outlier_data.patients = rbind(no_outlier_data.V1.SZ,no_outlier_data.MFG.SZ)

no_outlier_data.source = rbind(no_outlier_data.controls,no_outlier_data.patients)


no_outlier_data_list = list(no_outlier_data.source = no_outlier_data.source,
                         no_outlier_data.controls = no_outlier_data.controls,
                         no_outlier_data.patients = no_outlier_data.patients,
                         no_outlier_data.V1.All = no_outlier_data.V1.All,
                         no_outlier_data.V1.C = no_outlier_data.V1.C,
                         no_outlier_data.V1.SZ = no_outlier_data.V1.SZ,
                         no_outlier_data.MFG.All = no_outlier_data.MFG.All,
                         no_outlier_data.MFG.C = no_outlier_data.MFG.C,
                         no_outlier_data.MFG.SZ = no_outlier_data.MFG.SZ)

# save the data without outlier
save(no_outlier_data.source,no_outlier_data.controls,no_outlier_data.patients,
     no_outlier_data.V1.All,no_outlier_data.V1.C,no_outlier_data.V1.SZ,
     no_outlier_data.MFG.All,no_outlier_data.MFG.C,no_outlier_data.MFG.SZ,
     file = paste(save_data_dir,Item.no_outlier_data_frames,sep=""))#save individual data frames
save(no_outlier_data_list,file = paste(save_data_dir,Item.no_outlier_data_list,sep=""))#save the list
# convert the cleaned data to csv
write.table(no_outlier_data.source,file = paste(save_data_dir,"GABA_movement_no_outlier_data_source.csv",sep=""),sep=",",row.name=FALSE)
write.table(no_outlier_data.controls,file = paste(save_data_dir,"GABA_movement_no_outlier_data_controls.csv",sep=""),,sep=",",row.name=FALSE)
write.table(no_outlier_data.patients,file = paste(save_data_dir,"GABA_movement_no_outlier_data_patients.csv",sep=""),sep=",",row.name=FALSE)
write.table(no_outlier_data.V1.All,file = paste(save_data_dir,"GABA_movement_no_outlier_data_V1_All.csv",sep=""),sep=",",row.name=FALSE)
write.table(no_outlier_data.V1.C,file = paste(save_data_dir,"GABA_movement_no_outlier_data_V1_C.csv",sep=""),sep=",",row.name=FALSE)
write.table(no_outlier_data.V1.SZ,file = paste(save_data_dir,"GABA_movement_no_outlier_data_V1_SZ.csv",sep=""),sep=",",row.name=FALSE)
write.table(no_outlier_data.MFG.All,file = paste(save_data_dir,"GABA_movement_no_outlier_data_MFG_All.csv",sep=""),sep=",",row.name=FALSE)
write.table(no_outlier_data.MFG.C,file = paste(save_data_dir,"GABA_movement_no_outlier_data_MFG_C.csv",sep=""),sep=",",row.name=FALSE)
write.table(no_outlier_data.MFG.SZ,file = paste(save_data_dir,"GABA_movement_no_outlier_data_MFG_SZ.csv",sep=""),sep=",",row.name=FALSE)

# collapse to subject_data:load("/nfs/r21_gaba/reprocessing/analysis/imported_data/Set02/R_imported_no_outlier_data_frames.Rda")
# save_data_dir = "/nfs/r21_gaba/reprocessing/analysis/imported_data/Set04_subject_data_excluded_outliers/"

new_aggregate_fun = function(x,aggregate_cols,by_cols) {
  # aggregate one column each time
  Y = aggregate(x[aggregate_cols[1]], by = x[by_cols], FUN = mean, na.rm = T)
  for(i in 2:length(aggregate_cols)){
    Y = merge(Y, aggregate(x[aggregate_cols[i]], by = x[by_cols], FUN = mean, na.rm = T), all.x = T, all.y = T)
  }
  return(Y)
}

no_outlier_subject_data.source = new_aggregate_fun(no_outlier_data.source,test_cols,c("Subject_ID","Groups","Brain_Region"))


# Separating the data into controls and patients based on the column "Groups"
no_outlier_subject_data.controls = subset(no_outlier_subject_data.source, no_outlier_subject_data.source$Groups == "C",
                               select = c(Subject_ID,Groups,Brain_Region,
                                          GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                          NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                          Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                          RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
no_outlier_subject_data.patients = subset(no_outlier_subject_data.source, no_outlier_subject_data.source$Groups == "SZ" | no_outlier_subject_data.source$Groups == "PO",
                               select = c(Subject_ID,Groups,Brain_Region,
                                          GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                          NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                          Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                          RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
# Separating the data into V1 and MFG based on the column "Brain_Region"
no_outlier_subject_data.V1.All = subset(no_outlier_subject_data.source, no_outlier_subject_data.source$Brain_Region == "v1",
                             select = c(Subject_ID,Groups,Brain_Region,
                                        GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                        NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                        Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                        RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
no_outlier_subject_data.MFG.All = subset(no_outlier_subject_data.source, no_outlier_subject_data.source$Brain_Region == "mfg",
                              select = c(Subject_ID,Groups,Brain_Region,
                                         GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                         NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                         Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                         RMS_Y_disp,RMS_X_speed,RMS_Y_speed))

# Separating the data into subjects x brain_region, based on "Groups" & "Brain_Region"
no_outlier_subject_data.V1.C = subset(no_outlier_subject_data.source, no_outlier_subject_data.source$Groups == "C" & no_outlier_subject_data.source$Brain_Region == "v1",
                           select = c(Subject_ID,Groups,Brain_Region,
                                      GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                      NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                      Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                      RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
no_outlier_subject_data.V1.SZ = subset(no_outlier_subject_data.source, (no_outlier_subject_data.source$Groups == "SZ" | no_outlier_subject_data.source$Groups == "PO") & no_outlier_subject_data.source$Brain_Region == "v1",
                            select = c(Subject_ID,Groups,Brain_Region,
                                       GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                       NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                       Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                       RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
no_outlier_subject_data.MFG.C = subset(no_outlier_subject_data.source, no_outlier_subject_data.source$Groups == "C" & no_outlier_subject_data.source$Brain_Region == "mfg",
                            select = c(Subject_ID,Groups,Brain_Region,
                                       GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                       NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                       Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                       RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
no_outlier_subject_data.MFG.SZ = subset(no_outlier_subject_data.source, (no_outlier_subject_data.source$Groups == "SZ" | no_outlier_subject_data.source$Groups == "PO") & no_outlier_subject_data.source$Brain_Region == "mfg",
                             select = c(Subject_ID,Groups,Brain_Region,
                                        GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                        NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                        Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                        RMS_Y_disp,RMS_X_speed,RMS_Y_speed))

# Make the data into a list so it is easier to inspect properties all together
no_outlier_subject_data_list = list(no_outlier_subject_data.source = no_outlier_subject_data.source,
                         no_outlier_subject_data.controls = no_outlier_subject_data.controls,
                         no_outlier_subject_data.patients = no_outlier_subject_data.patients,
                         no_outlier_subject_data.V1.All = no_outlier_subject_data.V1.All,
                         no_outlier_subject_data.V1.C = no_outlier_subject_data.V1.C,
                         no_outlier_subject_data.V1.SZ = no_outlier_subject_data.V1.SZ,
                         no_outlier_subject_data.MFG.All = no_outlier_subject_data.MFG.All,
                         no_outlier_subject_data.MFG.C = no_outlier_subject_data.MFG.C,
                         no_outlier_subject_data.MFG.SZ = no_outlier_subject_data.MFG.SZ)

# save all the data frames
save(no_outlier_subject_data.source,no_outlier_subject_data.controls,no_outlier_subject_data.patients,
     no_outlier_subject_data.V1.All,no_outlier_subject_data.V1.C,no_outlier_subject_data.V1.SZ,
     no_outlier_subject_data.MFG.All,no_outlier_subject_data.MFG.C,no_outlier_subject_data.MFG.SZ,
     file = paste(save_data_dir,Item.no_outlier_subject_data_frames,sep=""))#save individual data frames
save(no_outlier_subject_data_list,file = paste(save_data_dir,Item.no_outlier_subject_data_list,sep=""))#save the list
# convert the cleaned data to csv
write.table(no_outlier_subject_data.source,file = paste(save_data_dir,"GABA_movement_no_outlier_subject_data_source.csv",sep=""),sep=",",row.name=FALSE)
write.table(no_outlier_subject_data.controls,file = paste(save_data_dir,"GABA_movement_no_outlier_subject_data_controls.csv",sep=""),,sep=",",row.name=FALSE)
write.table(no_outlier_subject_data.patients,file = paste(save_data_dir,"GABA_movement_no_outlier_subject_data_patients.csv",sep=""),sep=",",row.name=FALSE)
write.table(no_outlier_subject_data.V1.All,file = paste(save_data_dir,"GABA_movement_no_outlier_subject_data_V1_All.csv",sep=""),sep=",",row.name=FALSE)
write.table(no_outlier_subject_data.V1.C,file = paste(save_data_dir,"GABA_movement_no_outlier_subject_data_V1_C.csv",sep=""),sep=",",row.name=FALSE)
write.table(no_outlier_subject_data.V1.SZ,file = paste(save_data_dir,"GABA_movement_no_outlier_subject_data_V1_SZ.csv",sep=""),sep=",",row.name=FALSE)
write.table(no_outlier_subject_data.MFG.All,file = paste(save_data_dir,"GABA_movement_no_outlier_subject_data_MFG_All.csv",sep=""),sep=",",row.name=FALSE)
write.table(no_outlier_subject_data.MFG.C,file = paste(save_data_dir,"GABA_movement_no_outlier_subject_data_MFG_C.csv",sep=""),sep=",",row.name=FALSE)
write.table(no_outlier_subject_data.MFG.SZ,file = paste(save_data_dir,"GABA_movement_no_outlier_subject_data_MFG_SZ.csv",sep=""),sep=",",row.name=FALSE)

# This concludes data summary