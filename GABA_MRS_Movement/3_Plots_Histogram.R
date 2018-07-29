# Histogram
library(ggplot2)

f1 = function(x){
  tmp = as.numeric(boxplot(x,plot=F)$stats)
  #tmp = quantile(x,prob=c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(tmp) = c("ymin", "lower", "middle", "upper", "ymax")
  return(tmp)
}
f2 = function(x){
  tmp = boxplot(x)$out
  return(tmp)
}

# RMS_Mag_disp
d = data.frame(x=gl(1,length(data.good$D)),y=data.good$D)
p = ggplot(d,aes(x,y))+geom_violin(fill="skyblue", alpha=0.5) + coord_flip()
p = p + stat_summary(fun.data= f1, geom="boxplot",alpha=0.5) #boxplot
p = p + stat_summary(fun.y=f2, geom="point",size = 4)# outlier
#p = p + geom_point(colour="red",size=4, shape = factor(7),data=data.frame(x=gl(1,length(inst_disp)),y=inst_disp))+scale_shape(solid = FALSE)
p + xlab("") + ylab(expression("RMS of Displacement " * Delta ~ " (mm)")) + 
  theme(axis.ticks.y=element_blank(),axis.text.y=element_blank(),legend.position="none",
        axis.text.x = element_text(size=18),axis.title.x = element_text(size=20))


# RMS_Mag_speed
d = data.frame(x=gl(1,length(data.good$S)),y=data.good$S)
p = ggplot(d,aes(x,y))+geom_violin(fill="skyblue", alpha=0.5) + coord_flip()
p = p + stat_summary(fun.data= f1, geom="boxplot",alpha=0.5) #boxplot
p = p + stat_summary(fun.y=f2, geom="point",size = 4)# outlier
#p = p + geom_point(colour="red",size=4, shape = factor(7),data=data.frame(x=gl(1,length(inst_speed)),y=inst_speed))+scale_shape(solid = FALSE)
p + xlab("") + ylab(expression("RMS of Speed " * Sigma ~ " (mm/s)"))+ 
  theme(axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        axis.text.x = element_text(size=18),axis.title.x = element_text(size=20))

# GABA/Cr
d = data.frame(x=gl(1,length(data.V1.SZ$GABA.Cr)),y=data.V1.SZ$GABA.Cr)
p = ggplot(d,aes(x,y))+geom_violin(fill="skyblue", alpha=0.5) + coord_flip()
p = p + stat_summary(fun.data= f1, geom="boxplot",alpha=0.5) + stat_summary(fun.y=f2, geom="point",size = 4)
#p = p + geom_point(aes(ymax=max(inst_gaba),,position=position_dodge(width=10)),colour="red",size=4, shape = factor(7),data=data.frame(x=gl(1,length(inst_gaba)),y=inst_gaba))+scale_shape(solid = FALSE)
p + xlab("") + ylab("GABA/Cr") + 
  theme(axis.ticks.y=element_blank(),axis.text.y=element_blank(),
        axis.text.x = element_text(size=18),axis.title.x = element_text(size=20))