##################################################
# fcs imputation with random intercepts
##################################################

library(fdir)
library(rblimp)
library(rockchalk)
library(lme4)
library(mitml)

##################################################
# set working directory and load data
##################################################

set()
load('problemsolving2level.rda')

##################################################
# fcs imputation (ex15.imp)
##################################################

mymodel <- rblimp_fcs(
  data = problemsolving2level,
  clusterid = 'school',
  ordinal = 'condition hispanic frlunch',
  fixed = 'condition',
  variables = 'psolvepst psolvepre hispanic frlunch condition',
  seed = 90291,
  burn = 1000,
  iter = 10000,
  nimps = 20,
  chains = 20)
output(mymodel)

##################################################
# analysis and pooling
##################################################

# mitml list
implist <- as.mitml(mymodel)

# within-cluster (group mean) center level-1 predictors and add cluster-specific group means to the data
for (i in 1:length(implist)) {
  implist[[i]] <- gmc(implist[[i]], x = c('psolvepre','hispanic','frlunch'), by = c('school'), FUN = mean, suffix = c('.meanj', '.cwc'), fulldataframe = TRUE)
}

# analysis
fit <- with(implist, lmer(psolvepst ~ psolvepre.cwc + hispanic.cwc + frlunch.cwc + psolvepre.meanj + hispanic.meanj + frlunch.meanj + condition + (1 | school), REML = T))

# pooling
estimates <- testEstimates(fit, extra.pars = T)
estimates
confint(estimates)

##################################################
# wald test of null model
##################################################

nullanalysis <- with(implist, lmer(psolvepst ~ 1 + (1 | school)))
testModels(fit, nullanalysis, method = 'D1')


