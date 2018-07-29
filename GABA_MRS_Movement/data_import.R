# GABA Spectroscopy Movement Analysis 1: Data Import and organization

# Initialization
source_data_dir = "/Users/Edward/Documents/Assignments/Imaging Research Center/gaba_movement/Database_Reduced_Measurements_062014.csv"
save_data_dir = "/Users/Edward/Documents/Assignments/Imaging Research Center/gaba_movement/Set04_clinical_raw_data/"
data_type = "_data"
subject_data_type = "_subject_data"


# Data Import
# Read raw data
data.source = read.table(source_data_dir,header = TRUE, sep = ",", na.string = "NA",  dec = ".", strip.white = TRUE)
# Keep only the necessary columns
data.source = subset(data.source, select =  c(Subject_ID,Groups,Good_Runs,Instructed_to_move, movement_class,movement_subject,
                                              used_in_between_groups,Have_movement,Truncated,Too_Short,Session_Runs,replaced_mprage_with_1st_img,
                                              Region_run_order,Brain_Region,time, 
                                              GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                              NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,Magnitude.of.Speed.mm.,
                                              Jaggedness_X,Jaggedness_Y,RMS_X_disp,RMS_Y_disp,RMS_X_speed,RMS_Y_speed))

# Clean up the data set
# Remove data entry with NA movement
data.source = subset(data.source,(!is.na(data.source$RMS_Mag_disp) & !is.na(data.source$RMS_Mag_speed) & !is.na(data.source$GABA.Cr))) #remove the ones missing movement or spectroscopy data
#data.source = na.omit(data.source)#remove any entry with na
# Remove bad runs according to the column "Good_Runs"
data.source = subset(data.source,data.source$Good_Runs == 1)
# Remove data entry with runs too short according to the column "Too_Short"
data.source = subset(data.source,data.source$Too_Short == 0)
# Remove data entry with the subjects are instructed to move according to the column "Instructed_to_move"
data.source = subset(data.source,data.source$Instructed_to_move == 0)
# Remove data entry with mprage replaced with the first run: okay
# data.source = subset(data.source,data.source$replaced_mprage_with_1st_img != 1)
# Remove data entry not used in clinical analysis
data.source = subset(data.source,data.source$used_in_between_groups == 1)
# Select only the outlier data
#data.source = subset(data.source,data.source$movement_subject == 1)

# Inspect for potential wrong labels of Brain_Region column
unique(data.source$Brain_Region)
#[1] v1  mfg
#Levels:  mfg v1 --> Good, no wrong labels

# Separating the data into controls and patients based on the column "Groups"
data.controls = subset(data.source, data.source$Groups == "C",
                       select = c(Subject_ID,Groups, movement_class,Session_Runs,Region_run_order,Brain_Region,time,
                                  GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                  NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                  Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                  RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
data.patients = subset(data.source, data.source$Groups == "SZ" | data.source$Groups == "PO",
                       select = c(Subject_ID,Groups, movement_class,Session_Runs,Region_run_order,Brain_Region,time,
                                  GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                  NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                  Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                  RMS_Y_disp,RMS_X_speed,RMS_Y_speed))

# Separating the data into V1 and MFG based on the column "Brain_Region"
data.V1.All = subset(data.source, data.source$Brain_Region == "v1",
                 select = c(Subject_ID,Groups, movement_class,Session_Runs,Region_run_order,Brain_Region,time,
                            GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                            NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                            Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                            RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
data.MFG.All = subset(data.source, data.source$Brain_Region == "mfg",
                  select = c(Subject_ID,Groups, movement_class,Session_Runs,Region_run_order,Brain_Region,time,
                             GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                             NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                             Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                             RMS_Y_disp,RMS_X_speed,RMS_Y_speed))

# Separating the data into subjects x brain_region, based on "Groups" & "Brain_Region"
data.V1.C = subset(data.source, data.source$Groups == "C" & data.source$Brain_Region == "v1",
                 select = c(Subject_ID,Groups, movement_class,Session_Runs,Region_run_order,Brain_Region,time,
                            GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                            NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                            Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                            RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
data.V1.SZ = subset(data.source, (data.source$Groups == "SZ" | data.source$Groups == "PO") & data.source$Brain_Region == "v1",
                   select = c(Subject_ID,Groups, movement_class,Session_Runs,Region_run_order,Brain_Region,time,
                              GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                              NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                              Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                              RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
data.MFG.C = subset(data.source, data.source$Groups == "C" & data.source$Brain_Region == "mfg",
                   select = c(Subject_ID,Groups, movement_class,Session_Runs,Region_run_order,Brain_Region,time,
                              GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                              NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                              Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                              RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
data.MFG.SZ = subset(data.source, (data.source$Groups == "SZ" | data.source$Groups == "PO") & data.source$Brain_Region == "mfg",
                    select = c(Subject_ID,Groups, movement_class,Session_Runs,Region_run_order,Brain_Region,time,
                               GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                               NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                               Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                               RMS_Y_disp,RMS_X_speed,RMS_Y_speed))

# Make the data into a list so it is easier to inspect properties all together
data_list = list(data.source = data.source,
                 data.controls = data.controls,
                 data.patients = data.patients,
                 data.V1.All = data.V1.All,
                 data.V1.C = data.V1.C,
                 data.V1.SZ = data.V1.SZ,
                 data.MFG.All = data.MFG.All,
                 data.MFG.C = data.MFG.C,
                 data.MFG.SZ = data.MFG.SZ)

# save all the data frames
save(data.source,data.controls,data.patients,
     data.V1.All,data.V1.C,data.V1.SZ,
     data.MFG.All,data.MFG.C,data.MFG.SZ,
     file = paste(save_data_dir,"R_imported",data_type,"frames.Rda",sep=""))#save individual data frames
save(data_list,file = paste(save_data_dir,"R_imported",data_type,"_list.Rda",sep=""))#save the list
# convert the cleaned data to csv
write.table(data.source,file = paste(save_data_dir,"GABA_movement",data_type,"_source.csv",sep=""),sep=",",row.name=FALSE)
write.table(data.controls,file = paste(save_data_dir,"GABA_movement",data_type,"_controls.csv",sep=""),,sep=",",row.name=FALSE)
write.table(data.patients,file = paste(save_data_dir,"GABA_movement",data_type,"_patients.csv",sep=""),sep=",",row.name=FALSE)
write.table(data.V1.All,file = paste(save_data_dir,"GABA_movement",data_type,"_V1_All.csv",sep=""),sep=",",row.name=FALSE)
write.table(data.V1.C,file = paste(save_data_dir,"GABA_movement",data_type,"_V1_C.csv",sep=""),sep=",",row.name=FALSE)
write.table(data.V1.SZ,file = paste(save_data_dir,"GABA_movement",data_type,"_V1_SZ.csv",sep=""),sep=",",row.name=FALSE)
write.table(data.MFG.All,file = paste(save_data_dir,"GABA_movement",data_type,"_MFG_All.csv",sep=""),sep=",",row.name=FALSE)
write.table(data.MFG.C,file = paste(save_data_dir,"GABA_movement",data_type,"_MFG_C.csv",sep=""),sep=",",row.name=FALSE)
write.table(data.MFG.SZ,file = paste(save_data_dir,"GABA_movement",data_type,"_MFG_SZ.csv",sep=""),sep=",",row.name=FALSE)

# check if all the separated data frames add up to the source data frame
lapply(data_list, nrow)

# Take average across run level, yielding subject level data
aggregate_cols = c("GABA.Cr","Glx.Cr","Gaba.AdjPeak","Glx.AdjPeak","Cr","Off.Res.Cr","NAA","NAA.Cr",
                   "RMS_Mag_disp","RMS_Mag_speed","Magnitude.of.Displacement.mm.",
                   "Magnitude.of.Speed.mm.","Jaggedness_X","Jaggedness_Y","RMS_X_disp",
                   "RMS_Y_disp","RMS_X_speed","RMS_Y_speed")
subject_data.source =  aggregate(cbind(GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                       NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                       Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                       RMS_Y_disp,RMS_X_speed,RMS_Y_speed) ~ Subject_ID + Groups + Brain_Region,
                                 data.source, mean)
# Separating the data into controls and patients based on the column "Groups"
subject_data.controls = subset(subject_data.source, subject_data.source$Groups == "C",
                               select = c(Subject_ID,Groups, Brain_Region,
                                          GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                          NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                          Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                          RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
subject_data.patients = subset(subject_data.source, subject_data.source$Groups == "SZ" | subject_data.source$Groups == "PO",
                               select = c(Subject_ID,Groups, Brain_Region,
                                          GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                          NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                          Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                          RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
# Separating the data into V1 and MFG based on the column "Brain_Region"
subject_data.V1.All = subset(subject_data.source, subject_data.source$Brain_Region == "v1",
                     select = c(Subject_ID,Groups, Brain_Region,
                                GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
subject_data.MFG.All = subset(subject_data.source, subject_data.source$Brain_Region == "mfg",
                      select = c(Subject_ID,Groups, Brain_Region,
                                 GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                 NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                 Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                 RMS_Y_disp,RMS_X_speed,RMS_Y_speed))

# Separating the data into subjects x brain_region, based on "Groups" & "Brain_Region"
subject_data.V1.C = subset(subject_data.source, subject_data.source$Groups == "C" & subject_data.source$Brain_Region == "v1",
                   select = c(Subject_ID,Groups, Brain_Region,
                              GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                              NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                              Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                              RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
subject_data.V1.SZ = subset(subject_data.source, (subject_data.source$Groups == "SZ" | subject_data.source$Groups == "PO") & subject_data.source$Brain_Region == "v1",
                    select = c(Subject_ID,Groups, Brain_Region,
                               GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                               NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                               Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                               RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
subject_data.MFG.C = subset(subject_data.source, subject_data.source$Groups == "C" & subject_data.source$Brain_Region == "mfg",
                    select = c(Subject_ID,Groups, Brain_Region,
                               GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                               NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                               Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                               RMS_Y_disp,RMS_X_speed,RMS_Y_speed))
subject_data.MFG.SZ = subset(subject_data.source, (subject_data.source$Groups == "SZ" | subject_data.source$Groups == "PO") & subject_data.source$Brain_Region == "mfg",
                     select = c(Subject_ID,Groups, Brain_Region,
                                GABA.Cr,Glx.Cr,Gaba.AdjPeak,Glx.AdjPeak,Cr,Off.Res.Cr,NAA,
                                NAA.Cr,RMS_Mag_disp,RMS_Mag_speed,Magnitude.of.Displacement.mm.,
                                Magnitude.of.Speed.mm.,Jaggedness_X,Jaggedness_Y,RMS_X_disp,
                                RMS_Y_disp,RMS_X_speed,RMS_Y_speed))

# Make the data into a list so it is easier to inspect properties all together
subject_data_list = list(subject_data.source = subject_data.source,
                 subject_data.controls = subject_data.controls,
                 subject_data.patients = subject_data.patients,
                 subject_data.V1.All = subject_data.V1.All,
                 subject_data.V1.C = subject_data.V1.C,
                 subject_data.V1.SZ = subject_data.V1.SZ,
                 subject_data.MFG.All = subject_data.MFG.All,
                 subject_data.MFG.C = subject_data.MFG.C,
                 subject_data.MFG.SZ = subject_data.MFG.SZ)

# save all the data frames
save(subject_data.source,subject_data.controls,subject_data.patients,
     subject_data.V1.All,subject_data.V1.C,subject_data.V1.SZ,
     subject_data.MFG.All,subject_data.MFG.C,subject_data.MFG.SZ,
     file = paste(save_data_dir,"R_imported",subject_data_type,"frames.Rda",sep=""))#save individual data frames
save(subject_data_list,file = paste(save_data_dir,"R_imported",subject_data_type,"_list.Rda",sep=""))#save the list
# convert the cleaned data to csv
write.table(subject_data.source,file = paste(save_data_dir,"GABA_movement",subject_data_type,"_source.csv",sep=""),sep=",",row.name=FALSE)
write.table(subject_data.controls,file = paste(save_data_dir,"GABA_movement",subject_data_type,"_controls.csv",sep=""),,sep=",",row.name=FALSE)
write.table(subject_data.patients,file = paste(save_data_dir,"GABA_movement",subject_data_type,"_patients.csv",sep=""),sep=",",row.name=FALSE)
write.table(subject_data.V1.All,file = paste(save_data_dir,"GABA_movement",subject_data_type,"_V1_All.csv",sep=""),sep=",",row.name=FALSE)
write.table(subject_data.V1.C,file = paste(save_data_dir,"GABA_movement",subject_data_type,"_V1_C.csv",sep=""),sep=",",row.name=FALSE)
write.table(subject_data.V1.SZ,file = paste(save_data_dir,"GABA_movement",subject_data_type,"_V1_SZ.csv",sep=""),sep=",",row.name=FALSE)
write.table(subject_data.MFG.All,file = paste(save_data_dir,"GABA_movement",subject_data_type,"_MFG_All.csv",sep=""),sep=",",row.name=FALSE)
write.table(subject_data.MFG.C,file = paste(save_data_dir,"GABA_movement",subject_data_type,"_MFG_C.csv",sep=""),sep=",",row.name=FALSE)
write.table(subject_data.MFG.SZ,file = paste(save_data_dir,"GABA_movement",subject_data_type,"_MFG_SZ.csv",sep=""),sep=",",row.name=FALSE)


# This concludes data import