# Do summary stats

# library(moments)
# library(lawstat)

# specify data
# controls
C_GABA = no_outlier_data.MFG.C$GABA.Cr
C_DISP = no_outlier_data.MFG.C$RMS_Mag_disp
C_SPEED= no_outlier_data.MFG.C$RMS_Mag_speed*5
# patients
Z_GABA = no_outlier_data.MFG.SZ$GABA.Cr
Z_DISP = no_outlier_data.MFG.SZ$RMS_Mag_disp
Z_SPEED= no_outlier_data.MFG.SZ$RMS_Mag_speed*5

# calcualte summary stats
print('mean')
print(sprintf('C --> GABA: %f',mean(C_GABA,na.rm=T)))
print(sprintf('C --> DISP: %f',mean(C_DISP,na.rm=T)))
print(sprintf('C --> SPEED: %f',mean(C_SPEED,na.rm=T)))
print(sprintf('Z --> GABA: %f',mean(Z_GABA,na.rm=T)))
print(sprintf('Z --> DISP: %f',mean(Z_DISP,na.rm=T)))
print(sprintf('Z --> SPEED: %f',mean(Z_SPEED,na.rm=T)))
print("")
print('variance')
print(sprintf('C --> GABA: %f',var(C_GABA,na.rm=T)))
print(sprintf('C --> DISP: %f',var(C_DISP,na.rm=T)))
print(sprintf('C --> SPEED: %f',var(C_SPEED,na.rm=T)))
print(sprintf('Z --> GABA: %f',var(Z_GABA,na.rm=T)))
print(sprintf('Z --> DISP: %f',var(Z_DISP,na.rm=T)))
print(sprintf('Z --> SPEED: %f',var(Z_SPEED,na.rm=T)))
print("")
print('skewness')
print(sprintf('C --> GABA: %f',skewness(C_GABA,na.rm=T)))
print(sprintf('C --> DISP: %f',skewness(C_DISP,na.rm=T)))
print(sprintf('C --> SPEED: %f',skewness(C_SPEED,na.rm=T)))
print(sprintf('Z --> GABA: %f',skewness(Z_GABA,na.rm=T)))
print(sprintf('Z --> DISP: %f',skewness(Z_DISP,na.rm=T)))
print(sprintf('Z --> SPEED: %f',skewness(Z_SPEED,na.rm=T)))
print("")
print('kurtosis')
print(sprintf('C --> GABA: %f',kurtosis(C_GABA,na.rm=T)))
print(sprintf('C --> DISP: %f',kurtosis(C_DISP,na.rm=T)))
print(sprintf('C --> SPEED: %f',kurtosis(C_SPEED,na.rm=T)))
print(sprintf('Z --> GABA: %f',kurtosis(Z_GABA,na.rm=T)))
print(sprintf('Z --> DISP: %f',kurtosis(Z_DISP,na.rm=T)))
print(sprintf('Z --> SPEED: %f',kurtosis(Z_SPEED,na.rm=T)))
print("")
# boxplot
print("")
print('C --> GABA');print(boxplot(C_GABA,plot=F)$stat)
print('C --> DISP');print(boxplot(C_DISP,plot=F)$stat)
print('C --> SPEED');print(boxplot(C_SPEED,plot=F)$stat)
print('Z --> GABA');print(boxplot(Z_GABA,plot=F)$stat)
print('Z --> DISP');print(boxplot(Z_DISP,plot=F)$stat)
print('Z --> SPEED');print(boxplot(Z_SPEED,plot=F)$stat)
print("")
# Shapiro test
print("")
print("shaprio normality test")
print('C --> GABA');print(shapiro.test(C_GABA))
print('C --> DISP');print(shapiro.test(C_DISP))
print('C --> SPEED');print(shapiro.test(C_SPEED))
print('Z --> GABA');print(shapiro.test(Z_GABA))
print('Z --> DISP');print(shapiro.test(Z_DISP))
print('Z --> SPEED');print(shapiro.test(Z_SPEED))
print("")
#Student's T-Test
print("")
print("Student's T-test")
print('GABA');print(t.test(C_GABA,Z_GABA))
print('DISP');print(t.test(C_DISP,Z_DISP))
print('SPEED');print(t.test(C_SPEED,Z_SPEED))
print("")
# Brown-Forsythe test of variance
print("")
G = as.factor(c(rep('C',length(C_GABA)),rep('Z',length(Z_GABA))))
print("Brown-Forsythe test of variance")
YGABA = c(C_GABA,Z_GABA)
GGABA = G;GGABA = GGABA[!is.na(YGABA)];YGABA=YGABA[!is.na(YGABA)]
print('GABA');print(levene.test(YGABA,GGABA,location='median'))
YDISP = c(C_DISP,Z_DISP)
GDISP = G;GDISP = GDISP[!is.na(YDISP)];YDISP=YDISP[!is.na(YDISP)]
print('DISP');print(levene.test(YDISP,GDISP,location='median'))
YSPEED = c(C_SPEED,Z_SPEED)
GSPEED = G;GSPEED = GSPEED[!is.na(YSPEED)];YSPEED=YSPEED[!is.na(YSPEED)]
print('SPEED');print(levene.test(YSPEED,GSPEED,location='median'))




