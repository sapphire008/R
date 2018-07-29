# model inspection

# Raw Data
imported_raw_data_dir = "/nfs/r21_gaba/reprocessing/analysis/imported_data/Set01/"
Item.raw_data_frames = "R_imported_data_frames.Rda"
Item.raw_data_list = "R_imported_data_list.Rda"
Item.raw_subject_data_frames = "R_imported_subject_data_frames.Rda"
Item.raw_subject_data_list = "R_imported_subject_data_list.Rda"
# Data removed outliers
imported_clean_data_dir = "/nfs/r21_gaba/reprocessing/analysis/imported_data/Set02/"
Item.half_clean_data_frames = "R_imported_no_outlier_data_frames.Rda"
Item.half_clean_data_list = "R_imported_no_outlier_data_list.Rda"
Item.half_clean_subject_data_frames = "R_imported_no_outlier_subject_data_frames.Rda"
Item.half_clean_subject_data_list = "R_imported_no_outlier_subject_data_list.Rda"
# Output saving directory
result_save_dir = "/nfs/r21_gaba/reprocessing/analysis/results/"
# Defining numerical columns
test_cols = c("GABA.Cr","Glx.Cr","Gaba.AdjPeak","Glx.AdjPeak","Cr","Off.Res.Cr","NAA","NAA.Cr",
              "RMS_Mag_disp","RMS_Mag_speed","Magnitude.of.Displacement.mm.",
              "Magnitude.of.Speed.mm.","Jaggedness_X","Jaggedness_Y","RMS_X_disp",
              "RMS_Y_disp","RMS_X_speed","RMS_Y_speed")