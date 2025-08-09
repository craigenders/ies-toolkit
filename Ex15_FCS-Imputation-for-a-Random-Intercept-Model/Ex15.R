##################################################
# fcs imputation with random intercepts
##################################################

library(rblimp)
library(rockchalk)
library(lme4)
library(mitml)

##################################################
# load data from github
##################################################

data_url <- "https://raw.githubusercontent.com/craigenders/ies-toolkit/main/Data/problemsolving2level.rda"
load(gzcon(url(data_url, open = "rb")))

##################################################
# fcs imputation (ex15.imp)
##################################################

mymodel <- rblimp_fcs(
  data = problemsolving2level,
  clusterid = 'school',
  ordinal = 'condition hispanic frlunch',
  fixed = 'condition psolvepre',
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


