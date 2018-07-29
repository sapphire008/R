dataframe = ReadTable('/nfs/r21_gaba/reprocessing/analysis/imported_data/Set02/GABA_movement_no_outlier_data_source.csv');
for n = 1:numel(dataframe)
    if ischar(dataframe{n})
        dataframe{n} = strrep(dataframe{n},'"','');
    end
end

for a = 12:size(dataframe,2)
    for b = 2:size(dataframe,1)
        if ischar(dataframe{b,a})
            dataframe{b,a} = str2double(dataframe{b,a});
        end
    end
end

aggregate_by = {'Subject_ID','Groups','Brain_Region'};
aggregate_only = {'GABA.Cr','Glx.Cr','Gaba.AdjPeak','Glx.AdjPeak','Cr',...
    'Off.Res.Cr','NAA','NAA.Cr','RMS_Mag_disp','RMS_Mag_speed',...
    'Magnitude.of.Displacement.mm.','Magnitude.of.Speed.mm.',...
    'Jaggedness_X','Jaggedness_Y','RMS_X_disp','RMS_Y_disp',...
    'RMS_X_speed','RMS_Y_speed'};

output = aggregateR(dataframe,aggregate_by, @nanmean, aggregate_only);