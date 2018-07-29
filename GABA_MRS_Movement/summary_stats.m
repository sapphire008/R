worksheet = [];
func_handle = @nanmean;
worksheet = [worksheet;func_handle(GABACr),func_handle(RMS_Mag_disp),func_handle(RMS_Mag_speed)];
func_handle = @nanvar;
worksheet = [worksheet;func_handle(GABACr),func_handle(RMS_Mag_disp),func_handle(RMS_Mag_speed)];
func_handle = @skewness;
worksheet = [worksheet;func_handle(GABACr),func_handle(RMS_Mag_disp),func_handle(RMS_Mag_speed)];
func_handle = @kurtosis;
worksheet = [worksheet;func_handle(GABACr),func_handle(RMS_Mag_disp),func_handle(RMS_Mag_speed)];




