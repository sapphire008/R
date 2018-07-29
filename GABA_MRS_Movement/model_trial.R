m1 = gls(GABA.Cr ~ RMS_Mag_speed + Region_run_order + RMS_Mag_speed*Region_run_order,
         data = no_outlier_data.V1.C,
         correlation = corAR1(form = ~ Region_run_order | Subject_ID),
         na.action = na.omit)
summary(m1)

m2 = lme(fixed = GABA.Cr ~ RMS_Mag_speed, random = (~ 1|Subject_ID),
         data = no_outlier_data.V1.C,na.action = na.omit)
summary(m2)

m3 = lme(fixed = GABA.Cr ~ RMS_Mag_speed + Groups, random = (~1|Subject_ID),
         data = no_outlier_data.V1.All, na.action = na.omit)
summary(m3)

# Linear mixed-effects model fit by REML
# Data: no_outlier_data.V1.All 
# AIC       BIC   logLik
# -860.2349 -845.5266 435.1174
# 
# Random effects:
#   Formula: ~1 | Subject_ID
# (Intercept)    Residual
# StdDev:  0.01171969 0.007416699
# 
# Fixed effects: GABA.Cr ~ RMS_Mag_speed + Groups 
# Value  Std.Error DF  t-value p-value
# (Intercept)    0.1526030 0.00334188 89 45.66380  0.0000
# RMS_Mag_speed -0.4133365 0.15415744 89 -2.68126  0.0087
# GroupsSZ      -0.0074011 0.00352092 51 -2.10202  0.0405
# Correlation: 
#   (Intr) RMS_M_
# RMS_Mag_speed -0.711       
# GroupsSZ      -0.582  0.158
# 
# Standardized Within-Group Residuals:
#   Min         Q1        Med         Q3        Max 
# -2.3248964 -0.5437590 -0.0279840  0.5453819  1.8434471 
# 
# Number of Observations: 143
# Number of Groups: 53 


m4 = lme(fixed = GABA.Cr ~ RMS_Mag_disp + Groups, random = (~1|Subject_ID),
         data = no_outlier_data.V1.All, na.action = na.omit)
summary(m4)
# Linear mixed-effects model fit by REML
# Data: no_outlier_data.V1.All 
# AIC       BIC   logLik
# -853.4076 -838.6994 431.7038
# 
# Random effects:
#   Formula: ~1 | Subject_ID
# (Intercept)    Residual
# StdDev:  0.01243814 0.007137835
# 
# Fixed effects: GABA.Cr ~ RMS_Mag_disp + Groups 
# Value   Std.Error DF  t-value p-value
# (Intercept)   0.14981404 0.002922327 89 51.26533  0.0000
# RMS_Mag_disp -0.00417242 0.001530547 89 -2.72610  0.0077
# GroupsSZ     -0.00506051 0.003654079 51 -1.38489  0.1721
# Correlation: 
#   (Intr) RMS_M_
# RMS_Mag_disp -0.536       
# GroupsSZ     -0.544 -0.047
# 
# Standardized Within-Group Residuals:
#   Min            Q1           Med            Q3           Max 
# -2.3638267355 -0.6018714200  0.0009399619  0.5515840656  1.8049850560 
# 
# Number of Observations: 143
# Number of Groups: 53 

m5 = lme(fixed = scale(GABA.Cr) ~ scale(RMS_Mag_speed), random = (~1|Subject_ID),
         data = no_outlier_data.V1.C, na.action = na.omit)
summary(m5)

sm1 = lm(GABA.Cr ~ RMS_Mag_disp, data = no_outlier_subject_data.V1.C)
sm2 = lm(GABA.Cr ~ RMS_Mag_speed, data = no_outlier_subject_data.V1.C)

summary(sm1)
summary(sm2)