# Movement Analysis 11/08/2014
library(ggplot2)

# "D:/Edward/Documents/Assignments/Imaging Research Center/GABA Movement Second Submission/Analysis/data/data_results_11202014.RData"

# GABA
lm.GABA.D <- lm(GABA.Cr ~ D, data.clean.GABA.D)
lm.GABA.D$coefficients <- coef(mle.GABA.D)[1:2]
names(lm.GABA.D$coefficients) <- c("(Intercept)","D")
grid <- with(data.clean.GABA.D, expand.grid(D = seq(min(D), max(D), length=20)))
grid$GABA.Cr <- stats::predict(lm.GABA.D, newdata=grid)
err <- stats::predict(lm.GABA.D, newdata=grid, se = TRUE)
grid$ucl <- err$fit + 1.96 * err$se.fit
grid$lcl <- err$fit - 1.96 * err$se.fit
p = ggplot(data.clean.GABA.D, aes(x=D, y=GABA.Cr)) +
  geom_point(shape=1,size=6) + #geom_smooth(method=lm)+
  geom_smooth(aes(ymin = lcl, ymax = ucl), data = grid, stat="identity")+
  xlab(expression("RMS of Displacement " * Delta ~ " (mm)"))+
  ylab("V1 GABA/Cr")
# L = paste("R^2 ~ \"=\" ~", round(summary(lm(GABA.Cr ~ D, data.clean.GABA.D))$adj.r.squared,digits=4))
L = paste("-2 ~ logL ~ \"=\" ~", round(as.numeric(logLik(mle.GABA.D))*-2,digits=2))
p + geom_text(aes(x=1.5,y = 0.17, label = L),size=6,parse=T)+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))


lm.GABA.S <- lm(GABA.Cr ~ S, data.clean.GABA.S)
lm.GABA.S$coefficients <- coef(mle.GABA.S)[1:2]
names(lm.GABA.S$coefficients) <- c("(Intercept)","S")
grid <- with(data.clean.GABA.S, expand.grid(S = seq(min(S), max(S), length=20)))
grid$GABA.Cr <- stats::predict(lm.GABA.S, newdata=grid)
err <- stats::predict(lm.GABA.S, newdata=grid, se = TRUE)
grid$ucl <- err$fit + 1.96 * err$se.fit
grid$lcl <- err$fit - 1.96 * err$se.fit
p = ggplot(data.clean.GABA.S, aes(x=S, y=GABA.Cr)) +
  geom_point(shape=1,size=6) +#geom_smooth(method=lm)+
  geom_smooth(aes(ymin = lcl, ymax = ucl), data = grid, stat="identity")+
  xlab(expression("RMS of Speed " * Sigma ~ " (mm/s)"))+
  ylab("V1 GABA/Cr")
L = paste("-2 ~ logL ~ \"=\" ~", round(as.numeric(logLik(mle.GABA.S))*-2,digits=2))
p + geom_text(aes(x=0.14,y = 0.17, label = L),size=6,parse=T)+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))

# NAA
lm.NAA.D <- lm(NAA.Cr ~ D, data.clean.NAA.D)
lm.NAA.D$coefficients <- coef(mle.NAA.D)[1:2]
names(lm.NAA.D$coefficients) <- c("(Intercept)","D")
grid <- with(data.clean.NAA.D, expand.grid(D = seq(min(D), max(D), length=20)))
grid$NAA.Cr <- stats::predict(lm.NAA.D, newdata=grid)
err <- stats::predict(lm.NAA.D, newdata=grid, se = TRUE)
grid$ucl <- err$fit + 1.96 * err$se.fit
grid$lcl <- err$fit - 1.96 * err$se.fit
p = ggplot(data.clean.NAA.D, aes(x=D, y=NAA.Cr)) +
  geom_point(shape=1,size=6) + #geom_smooth(method=lm)+
  geom_smooth(aes(ymin = lcl, ymax = ucl), data = grid, stat="identity")+
  xlab(expression("RMS of Displacement " * Delta ~ " (mm)"))+
  ylab("V1 NAA/Cr")
# L = paste("R^2 ~ \"=\" ~", round(summary(lm(NAA.Cr ~ D, data.clean.NAA.D))$adj.r.squared,digits=4))
L = paste("-2 ~ logL ~ \"=\" ~", round(as.numeric(logLik(mle.NAA.D))*-2,digits=2))
p + geom_text(aes(x=1.5,y = 2.0, label = L),size=6,parse=T)+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))

lm.NAA.S <- lm(NAA.Cr ~ S, data.clean.NAA.S)
lm.NAA.S$coefficients <- coef(mle.NAA.S)[1:2]
names(lm.NAA.S$coefficients) <- c("(Intercept)","S")
grid <- with(data.clean.NAA.S, expand.grid(S = seq(min(S), max(S), length=20)))
grid$NAA.Cr <- stats::predict(lm.NAA.S, newdata=grid)
err <- stats::predict(lm.NAA.S, newdata=grid, se = TRUE)
grid$ucl <- err$fit + 1.96 * err$se.fit
grid$lcl <- err$fit - 1.96 * err$se.fit
p = ggplot(data.clean.NAA.S, aes(x=S, y=NAA.Cr)) +
  geom_point(shape=1,size=6) +#geom_smooth(method=lm)+
  geom_smooth(aes(ymin = lcl, ymax = ucl), data = grid, stat="identity")+
  xlab(expression("RMS of Speed " * Sigma ~ " (mm/s)"))+
  ylab("V1 NAA/Cr")
L = paste("-2 ~ logL ~ \"=\" ~", round(as.numeric(logLik(mle.NAA.S))*-2,digits=2))
p + geom_text(aes(x=0.13,y = 2.0, label = L),size=6,parse=T)+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))

# Glx
lm.Glx.D <- lm(Glx.Cr ~ D, data.clean.Glx.D)
lm.Glx.D$coefficients <- coef(mle.Glx.D)[1:2]
names(lm.Glx.D$coefficients) <- c("(Intercept)","D")
grid <- with(data.clean.Glx.D, expand.grid(D = seq(min(D), max(D), length=20)))
grid$Glx.Cr <- stats::predict(lm.Glx.D, newdata=grid)
err <- stats::predict(lm.Glx.D, newdata=grid, se = TRUE)
grid$ucl <- err$fit + 1.96 * err$se.fit
grid$lcl <- err$fit - 1.96 * err$se.fit
p = ggplot(data.clean.Glx.D, aes(x=D, y=Glx.Cr)) +
  geom_point(shape=1,size=6) + #geom_smooth(method=lm)+
  geom_smooth(aes(ymin = lcl, ymax = ucl), data = grid, stat="identity")+
  xlab(expression("RMS of Displacement " * Delta ~ " (mm)"))+
  ylab("V1 Glx/Cr")
# L = paste("R^2 ~ \"=\" ~", round(summary(lm(Glx.Cr ~ D, data.clean.Glx.D))$adj.r.squared,digits=4))
L = paste("-2 ~ logL ~ \"=\" ~", round(as.numeric(logLik(mle.Glx.D))*-2,digits=2))
p + geom_text(aes(x=1.5,y = 0.16, label = L),size=6,parse=T)+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))

lm.Glx.S <- lm(Glx.Cr ~ S, data.clean.Glx.S)
lm.Glx.S$coefficients <- coef(mle.Glx.S)[1:2]
names(lm.Glx.S$coefficients) <- c("(Intercept)","S")
grid <- with(data.clean.Glx.S, expand.grid(S = seq(min(S), max(S), length=20)))
grid$Glx.Cr <- stats::predict(lm.Glx.S, newdata=grid)
err <- stats::predict(lm.Glx.S, newdata=grid, se = TRUE)
grid$ucl <- err$fit + 1.96 * err$se.fit
grid$lcl <- err$fit - 1.96 * err$se.fit
p = ggplot(data.clean.Glx.S, aes(x=S, y=Glx.Cr)) +
  geom_point(shape=1,size=6) +#geom_smooth(method=lm)+
  geom_smooth(aes(ymin = lcl, ymax = ucl), data = grid, stat="identity")+
  xlab(expression("RMS of Speed " * Sigma ~ " (mm/s)"))+
  ylab("V1 Glx/Cr")
L = paste("-2 ~ logL ~ \"=\" ~", round(as.numeric(logLik(mle.Glx.S))*-2,digits=2))
p + geom_text(aes(x=0.13,y = 0.16, label = L),size=6,parse=T)+
  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
        axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))

