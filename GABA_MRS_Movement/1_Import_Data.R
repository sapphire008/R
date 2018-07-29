# Movement Analysis 11/08/2014
rm(list=ls())
# Import Data
source_data_dir = "/Users/Edward/Documents/Ubuntu/Analysis/data/source/Movement_Database_110914.csv"
save_dir = "/Users/Edward/Documents/Ubuntu/Analysis/data/"
too_short_thresh = 0.1

data.source = read.table(source_data_dir,header = TRUE, sep = ",", na.string = "NA",  dec = ".", strip.white = TRUE)
# Filter Columns
data.source = subset(data.source, select =  c(Subject, Groups, Brain.Region, Run, Region.Run.Order, # grouping parameters
                                              Length.After.Truncation, # House-keekping variables
                                              Good.Runs, Instructed_to_move, used_in_between_groups,Too.Short, # filtering parameters
                                              GABA.Cr,Glx.Cr,NAA.Cr,Cr,D,S)) # variables
# Filter Rows
data.source = subset(data.source,(!is.na(data.source$D) & !is.na(data.source$S) & !is.na(data.source$GABA.Cr) & data.source$Too.Short <=too_short_thresh )) #remove the ones missing/not sufficient movement or spectroscopy data

# Separate data by grouping parameters

# Movement Analysis: within control
mdata.V1.C = subset(data.source, subset = data.source$Brain.Region == "v1" & data.source$Groups== "C", select = c(Subject, Groups, Brain.Region, Run, Region.Run.Order,Good.Runs,Instructed_to_move,Length.After.Truncation, GABA.Cr,Glx.Cr,NAA.Cr,Cr,D,S))
mdata.MFG.C = subset(data.source, subset = data.source$Brain.Region == "mfg" & data.source$Groups== "C", select = c(Subject, Groups, Brain.Region, Run, Region.Run.Order,Good.Runs,Instructed_to_move,Length.After.Truncation, GABA.Cr,Glx.Cr,NAA.Cr,Cr,D,S))

# Clinical Analaysis: in between groups
cdata.V1 = subset(data.source, subset = data.source$Brain.Region == "v1" & data.source$used_in_between_groups == 1 & data.source$Instructed_to_move == 0 & data.source$Good.Runs == 1, 
                  select = c(Subject, Groups, Brain.Region, Run, Region.Run.Order,Length.After.Truncation, GABA.Cr,Glx.Cr,NAA.Cr,Cr,D,S))
cdata.MFG = subset(data.source, subset = data.source$Brain.Region == "mfg" & data.source$used_in_between_groups == 1 & data.source$Instructed_to_move == 0 & data.source$Good.Runs == 1, 
                  select = c(Subject, Groups, Brain.Region, Run, Region.Run.Order,Length.After.Truncation, GABA.Cr,Glx.Cr,NAA.Cr,Cr,D,S))

# save variables
saveRDS(data.source, file=file.path(save_dir,"source","source_filtered_data.rds"))
saveRDS(mdata.V1.C,file = file.path(save_dir,"movement","movement_data_v1_c.rds"))
saveRDS(mdata.MFG.C, file = file.path(save_dir,"movement","movement_data_mfg_c.rds"))
saveRDS(cdata.V1, file=file.path(save_dir,"clinical","clinical_data_v1.rds"))
saveRDS(cdata.MFG, file=file.path(save_dir,"clinical","clinical_data_mfg.rds"))

# write csv tables
write.table(data.source,file = file.path(save_dir,"source","source_filtered_data.csv"), sep=",",row.name=FALSE)
write.table(mdata.V1.C, file = file.path(save_dir,"movement","movement_data_v1_c.csv"), sep=",",row.name=FALSE)
write.table(mdata.MFG.C ,file = file.path(save_dir,"movement","movement_data_mfg_c.csv"), sep=",",row.name=FALSE)
write.table(cdata.V1,file = file.path(save_dir,"clinical","clinical_data_v1.csv"), sep=",",row.name=FALSE)
write.table(cdata.MFG,file = file.path(save_dir,"clinical","clinical_data_mfg.csv"), sep=",",row.name=FALSE)

