data <- readRDS("/Users/Edward/Documents/Ubuntu/Analysis/data/movement/movement_data_v1_c.rds")
data.good <- readRDS("/Users/Edward/Documents/Ubuntu/Analysis/data/movement/movement_data_v1_c_good_only.rds")
data.clean <- readRDS("/Users/Edward/Documents/Ubuntu/Analysis/data/movement/movement_data_v1_c_good_only_without_outlier.rds")
data.clean <- na.omit(data.clean)


# Linear Mixed Models
#library(nlme)
m1 <- lme(GABA.Cr ~ Region.Run.Order, data = data.clean, random = ~Region.Run.Order|Subject, method = "ML", na.action = na.omit)
m2 <- lme(GABA.Cr ~ D + Region.Run.Order, data = data.clean, random = ~Region.Run.Order|Subject, method = "ML", na.action = na.omit)
m3 <- lme(GABA.Cr ~ S + Region.Run.Order, data = data.clean, random = ~Region.Run.Order|Subject, method = "ML", na.action = na.omit)
m4 <- lme(GABA.Cr ~ D + S + Region.Run.Order, data = data.clean, random = ~Region.Run.Order|Subject, method = "ML", na.action = na.omit)

library(lme4)
library(languageR)
m1 <- lmer(GABA.Cr ~ 1 + (1|Subject), data.clean, REML = FALSE)
m2 <- lmer(GABA.Cr ~ D + (1|Subject), data.clean, REML = FALSE)
m3 <- lmer(GABA.Cr ~ S + (1|Subject), data.clean, REML = FALSE)
m4 <- lmer(GABA.Cr ~ D + S + (1|Subject), data.clean, REML = FALSE)
# Intraclass correlation (ICC)
ICC.m4 <- m4

library(lqmm)
mytau <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
m1 <- lqmm(GABA.Cr ~ D + S , random = ~1, group = Subject, covariance = "pdSymm", 
           tau = mytau, nK = 7, type = "normal", data = data.clean,
           control = lqmmControl(method="df"))
summary(boot(m1, R= 100)) # not always yielding significant results, hooveing between 0.02 and 0.1
# different quantiles of output variable (conditional variable, covariates) have different fits, beta values


library(glmmML)
m1 <- glmmML(GABA.Cr ~ D + S, family = "poisson", data = data, cluster = Subject)
summary(m1)


library(nlme)
m1 <- nlme(GABA.Cr ~ D + S, data = data, groups = Subject)

library(gss)
m1 <- ssanova(GABA.Cr ~ D + S, data = data, random = ~1|Subject, method="m")

library(nlme)
m1 <- lme(GABA.Cr ~ D + S, data = data, random = ~1+Region.Run.Order|Subject, na.action = na.omit, 
          subset = data$Good.Runs == 1, weight =  varIdent(form = ~1 | Movement.Group), method = "ML")

