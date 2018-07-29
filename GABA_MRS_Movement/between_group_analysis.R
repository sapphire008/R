# Between group analyses

# Between group models

# T Test: without movement
GABA.Cr.C = subset(no_outlier_subject_data.V1.All$GABA.Cr,no_outlier_subject_data.V1.All$Groups == "C")
GABA.Cr.SZ = subset(no_outlier_subject_data.V1.All$GABA.Cr,no_outlier_subject_data.V1.All$Groups == "SZ")
TTest.GABA.Cr = t.test(GABA.Cr.C, GABA.Cr.SZ, alternative = "greater", paired = F,var.equal = F)
# Note that Welch t-test assumes the variance of the two samples are different, a modification of Student's T-Test

# Welch Two Sample t-test
# data:  GABA.Cr.C and GABA.Cr.SZ
# t = 1.3789, df = 46.846, p-value = 0.08723
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   -0.00113684         Inf
# sample estimates:
#   mean of x mean of y 
# 0.1457393 0.1404981 

# ANOVA without movement
ANOVA.GABA.Cr =  aov(GABA.Cr ~ Groups, data = no_outlier_subject_data.V1.All)
#             Df   Sum Sq   Mean Sq F value Pr(>F)
# Groups       1 0.000352 0.0003524   1.912  0.173
# Residuals   50 0.009216 0.0001843

# ANCOVA with movement
# Note: No significant interaction  of movement parameters with Groups
options(contrasts = c("contr.treatment", "contr.poly"))
#options(contrasts = c("contr.helmert", "contr.poly"))
ANCOVA.GABA.Cr.movement = lm(GABA.Cr ~ RMS_Mag_disp + I(RMS_Mag_speed*5) + Groups, no_outlier_subject_data.V1.All,
                             contrasts = )
summary(ANCOVA.GABA.Cr.movement)
# Call:
#   lm(formula = GABA.Cr ~ RMS_Mag_disp + I(RMS_Mag_speed * 5) + 
#        Groups, data = no_outlier_subject_data.V1.All)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.032194 -0.007125  0.001102  0.007967  0.018848 
# 
# Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
# (Intercept)           0.167109   0.006407  26.084  < 2e-16 ***
#   RMS_Mag_disp       -0.008847   0.004506  -1.964  0.05563 .  
# I(RMS_Mag_speed * 5) -0.159051   0.057301  -2.776  0.00794 ** 
#   GroupsSZ           -0.009195   0.003654  -2.517  0.01540 *  
#   ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1
# 
# Residual standard error: 0.01222 on 46 degrees of freedom
# (2 observations deleted due to missingness)
# Multiple R-squared:  0.264,  Adjusted R-squared:  0.2161 
# F-statistic: 5.501 on 3 and 46 DF,  p-value: 0.002582

anova(ANCOVA.GABA.Cr.movement)
# Analysis of Variance Table
# 
# Response: GABA.Cr
#                        Df    Sum Sq    Mean Sq F value  Pr(>F)  
# RMS_Mag_disp            1 0.0008303 0.00083027  5.5602 0.02268 *
#   I(RMS_Mag_speed * 5)  1 0.0006885 0.00068850  4.6108 0.03708 *
#   Groups                1 0.0009457 0.00094569  6.3332 0.01540 *
#   Residuals            46 0.0068689 0.00014932                  
# ---
#   Signif. codes:  0 ?**?0.001 ?*?0.01 ??0.05 ??0.1 ??1


# Get effect of the group
library(effects)
effect("Groups", ANCOVA.GABA.Cr.movement)
# True group mean after accounting for movement
# Groups effect
# Groups
# C        SZ 
# 0.1468837 0.1376885