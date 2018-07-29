# Plot MLELM 
library(ggplot2)

# GABA
lm.GABA.D <- lm(GABA.Cr ~ D, data.clean.GABA.D)
X <- model.matrix(lm.GABA.DS)
Y <- data.clean.GABA.DS$GABA.Cr
R <- residuals.mle(coef(mle.GABA.DS))
P <- X[,1:2] %*% coef(mle.GABA.DS)[c(1:2)]
lm.GABA.D$coefficients <- coef(mle.GABA.DS)[1:2]
names(lm.GABA.D$coefficients) <- c("(Intercept)","D")
grid <- with(data.clean.GABA.D, expand.grid(D = seq(min(D), max(D), length=28)))
grid$GABA.Cr <- stats::predict(lm.GABA.D, newdata=grid)
err <- stats::predict(lm.GABA.D, newdata=grid, se = TRUE)
grid$ucl <- err$fit + 1.96 * err$se.fit
grid$lcl <- err$fit - 1.96 * err$se.fit
df <- data.frame(GABA.Cr = as.numeric(R + P), D= as.numeric(X[,2]))
p = ggplot(df, aes(x=D, y=GABA.Cr)) +
  geom_point(shape=1,size=6) + #geom_smooth(method=lm)+
  geom_smooth(aes(ymin = lcl, ymax = ucl), data = grid, stat="identity")+
  xlab(expression("RMS of Displacement " * Delta ~ " (mm)"))+
  ylab("V1 GABA/Cr Partial Residuals")
# L = paste("R^2 ~ \"=\" ~", round(summary(lm(GABA.Cr ~ D, data.clean.GABA.D))$adj.r.squared,digits=4))
# L = paste("-2 ~ logL ~ \"=\" ~", round(as.numeric(logLik(mle.GABA.D))*-2,digits=2))
# p + geom_text(aes(x=1.5,y = 0.17, label = L),size=6,parse=T)+
p+  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
         axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))


lm.GABA.S <- lm(GABA.Cr ~ S, data.clean.GABA.S)
X <- model.matrix(lm.GABA.DS)
Y <- data.clean.GABA.DS$GABA.Cr
R <- residuals.mle(coef(mle.GABA.DS))
P <- X[,c(1,3)] %*% coef(mle.GABA.DS)[c(1,3)]
lm.GABA.S$coefficients <- coef(mle.GABA.DS)[c(1,3)]
names(lm.GABA.S$coefficients) <- c("(Intercept)","S")
grid <- with(data.clean.GABA.S, expand.grid(S = seq(min(S), max(S), length=29)))
grid$GABA.Cr <- stats::predict(lm.GABA.S, newdata=grid)
err <- stats::predict(lm.GABA.S, newdata=grid, se = TRUE)
grid$ucl <- err$fit + 1.96 * err$se.fit
grid$lcl <- err$fit - 1.96 * err$se.fit
df <- data.frame(GABA.Cr = as.numeric(R + P), S= as.numeric(X[,3]))
p = ggplot(df, aes(x=S, y=GABA.Cr)) +
  geom_point(shape=1,size=6) + #geom_smooth(method=lm)+
  geom_smooth(aes(ymin = lcl, ymax = ucl), data = grid, stat="identity")+
  xlab(expression("RMS of Speed " * Sigma ~ " (mm/s)"))+
  ylab("V1 GABA/Cr Partial Residuals")
#L = paste("R^2 ~ \"=\" ~", round(summary(lm(GABA.Cr ~ D, data.clean.GABA.D))$adj.r.squared,digits=4))
#L = paste("-2 ~ logL ~ \"=\" ~", round(as.numeric(logLik(mle.GABA.D))*-2,digits=2))
# p + geom_text(aes(x=1.5,y = 0.17, label = L),size=6,parse=T)+
p+  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
          axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))

# NAA
lm.NAA.D <- lm(NAA.Cr ~ D, data.clean.NAA.D)
X <- model.matrix(lm.NAA.DS)
Y <- data.clean.NAA.DS$NAA.Cr
R <- residuals.mle(coef(mle.NAA.DS))
P <- X[,1:2] %*% coef(mle.NAA.DS)[c(1:2)]
lm.NAA.D$coefficients <- coef(mle.NAA.DS)[1:2]
names(lm.NAA.D$coefficients) <- c("(Intercept)","D")
grid <- with(data.clean.NAA.D, expand.grid(D = seq(min(D), max(D), length=28)))
grid$NAA.Cr <- stats::predict(lm.NAA.D, newdata=grid)
err <- stats::predict(lm.NAA.D, newdata=grid, se = TRUE)
grid$ucl <- err$fit + 1.96 * err$se.fit
grid$lcl <- err$fit - 1.96 * err$se.fit
df <- data.frame(NAA.Cr = as.numeric(R + P), D= as.numeric(X[,2]))
p = ggplot(df, aes(x=D, y=NAA.Cr)) +
  geom_point(shape=1,size=6) + #geom_smooth(method=lm)+
  geom_smooth(aes(ymin = lcl, ymax = ucl), data = grid, stat="identity")+
  xlab(expression("RMS of Displacement " * Delta ~ " (mm)"))+
  ylab("V1 NAA/Cr Partial Residuals")
# L = paste("R^2 ~ \"=\" ~", round(summary(lm(NAA.Cr ~ D, data.clean.NAA.D))$adj.r.squared,digits=4))
# L = paste("-2 ~ logL ~ \"=\" ~", round(as.numeric(logLik(mle.NAA.D))*-2,digits=2))
# p + geom_text(aes(x=1.5,y = 0.17, label = L),size=6,parse=T)+
p+  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
          axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))


lm.NAA.S <- lm(NAA.Cr ~ S, data.clean.NAA.S)
X <- model.matrix(lm.NAA.DS)
Y <- data.clean.NAA.DS$NAA.Cr
R <- residuals.mle(coef(mle.NAA.DS))
P <- X[,c(1,3)] %*% coef(mle.NAA.DS)[c(1,3)]
lm.NAA.S$coefficients <- coef(mle.NAA.DS)[c(1,3)]
names(lm.NAA.S$coefficients) <- c("(Intercept)","S")
grid <- with(data.clean.NAA.S, expand.grid(S = seq(min(S), max(S), length=29)))
grid$NAA.Cr <- stats::predict(lm.NAA.S, newdata=grid)
err <- stats::predict(lm.NAA.S, newdata=grid, se = TRUE)
grid$ucl <- err$fit + 1.96 * err$se.fit
grid$lcl <- err$fit - 1.96 * err$se.fit
df <- data.frame(NAA.Cr = as.numeric(R + P), S= as.numeric(X[,3]))
p = ggplot(df, aes(x=S, y=NAA.Cr)) +
  geom_point(shape=1,size=6) + #geom_smooth(method=lm)+
  geom_smooth(aes(ymin = lcl, ymax = ucl), data = grid, stat="identity")+
  xlab(expression("RMS of Speed " * Sigma ~ " (mm/s)"))+
  ylab("V1 NAA/Cr Partial Residuals")
#L = paste("R^2 ~ \"=\" ~", round(summary(lm(NAA.Cr ~ D, data.clean.NAA.D))$adj.r.squared,digits=4))
#L = paste("-2 ~ logL ~ \"=\" ~", round(as.numeric(logLik(mle.NAA.D))*-2,digits=2))
# p + geom_text(aes(x=1.5,y = 0.17, label = L),size=6,parse=T)+
p+  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
          axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))


# Glx
lm.Glx.D <- lm(Glx.Cr ~ D, data.clean.Glx.D)
X <- model.matrix(lm.Glx.DS)
Y <- data.clean.Glx.DS$Glx.Cr
R <- residuals.mle(coef(mle.Glx.DS))
P <- X[,1:2] %*% coef(mle.Glx.DS)[c(1:2)]
lm.Glx.D$coefficients <- coef(mle.Glx.DS)[1:2]
names(lm.Glx.D$coefficients) <- c("(Intercept)","D")
grid <- with(data.clean.Glx.D, expand.grid(D = seq(min(D), max(D), length=28)))
grid$Glx.Cr <- stats::predict(lm.Glx.D, newdata=grid)
err <- stats::predict(lm.Glx.D, newdata=grid, se = TRUE)
grid$ucl <- err$fit + 1.96 * err$se.fit
grid$lcl <- err$fit - 1.96 * err$se.fit
df <- data.frame(Glx.Cr = as.numeric(R + P), D= as.numeric(X[,2]))
p = ggplot(df, aes(x=D, y=Glx.Cr)) +
  geom_point(shape=1,size=6) + #geom_smooth(method=lm)+
  geom_smooth(aes(ymin = lcl, ymax = ucl), data = grid, stat="identity")+
  xlab(expression("RMS of Displacement " * Delta ~ " (mm)"))+
  ylab("V1 Glx/Cr Partial Residuals")
# L = paste("R^2 ~ \"=\" ~", round(summary(lm(Glx.Cr ~ D, data.clean.Glx.D))$adj.r.squared,digits=4))
# L = paste("-2 ~ logL ~ \"=\" ~", round(as.numeric(logLik(mle.Glx.D))*-2,digits=2))
# p + geom_text(aes(x=1.5,y = 0.17, label = L),size=6,parse=T)+
p+  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
          axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))


lm.Glx.S <- lm(Glx.Cr ~ S, data.clean.Glx.S)
X <- model.matrix(lm.Glx.DS)
Y <- data.clean.Glx.DS$Glx.Cr
R <- residuals.mle(coef(mle.Glx.DS))
P <- X[,c(1,3)] %*% coef(mle.Glx.DS)[c(1,3)]
lm.Glx.S$coefficients <- coef(mle.Glx.DS)[c(1,3)]
names(lm.Glx.S$coefficients) <- c("(Intercept)","S")
grid <- with(data.clean.Glx.S, expand.grid(S = seq(min(S), max(S), length=29)))
grid$Glx.Cr <- stats::predict(lm.Glx.S, newdata=grid)
err <- stats::predict(lm.Glx.S, newdata=grid, se = TRUE)
grid$ucl <- err$fit + 1.96 * err$se.fit
grid$lcl <- err$fit - 1.96 * err$se.fit
df <- data.frame(Glx.Cr = as.numeric(R + P), S= as.numeric(X[,3]))
p = ggplot(df, aes(x=S, y=Glx.Cr)) +
  geom_point(shape=1,size=6) + #geom_smooth(method=lm)+
  geom_smooth(aes(ymin = lcl, ymax = ucl), data = grid, stat="identity")+
  xlab(expression("RMS of Speed " * Sigma ~ " (mm/s)"))+
  ylab("V1 Glx/Cr Partial Residuals")
#L = paste("R^2 ~ \"=\" ~", round(summary(lm(Glx.Cr ~ D, data.clean.Glx.D))$adj.r.squared,digits=4))
#L = paste("-2 ~ logL ~ \"=\" ~", round(as.numeric(logLik(mle.Glx.D))*-2,digits=2))
# p + geom_text(aes(x=1.5,y = 0.17, label = L),size=6,parse=T)+
p+  theme(axis.text.x = element_text(size=18),axis.title.x = element_text(size=20),
          axis.text.y = element_text(size=18),axis.title.y = element_text(size=20))