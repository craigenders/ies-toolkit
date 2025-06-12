##################################################
# mcmc 3-level mlm
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
load('problemsolving3level.rda')

##################################################
# fit model with mcmc (ex18.1.imp)
##################################################

mymodel <- rblimp(
  data = problemsolving3level,
  clusterid = 'school student',
  ordinal = 'male frlunch condition',
  fixed = 'month7 male condition',
  center = 'grandmean = male frlunch teachexp',
  model = ' 
   probsolve ~ month7 male frlunch teachexp condition month7*condition | month7',
  simple = 'month7 | condition',
  seed = 90291,
  burn = 20000,
  iter = 50000)
output(mymodel)
simple_plot(probsolve ~ month7 | condition, mymodel)

##################################################
# fit model and save imputations (ex18.2.imp)
##################################################

mymodel <- rblimp(
  data = problemsolving3level,
  clusterid = 'school student',
  ordinal = 'male frlunch condition',
  fixed = 'month7 male condition',
  center = 'grandmean = male frlunch teachexp',
  model = ' 
   probsolve ~ month7 male frlunch teachexp condition month7*condition | month7',
  simple = 'month7 | condition',
  seed = 90291,
  burn = 20000,
  iter = 50000,
  nimps = 20,
  chains = 20)
output(mymodel)
simple_plot(probsolve ~ month7 | condition, mymodel)

##################################################
# analysis and pooling
##################################################

# mitml list
implist <- as.mitml(mymodel)

# center covariates at the pooled grand mean
mean_male <- mean(unlist(lapply(implist, function(df) mean(df$male))))
mean_frlunch <- mean(unlist(lapply(implist, function(df) mean(df$frlunch))))
mean_teachexp <- mean(unlist(lapply(implist, function(df) mean(df$teachexp))))
for (i in 1:length(implist)) {
  implist[[i]]$male.cgm <- implist[[i]]$male - mean_male
  implist[[i]]$frlunch.cgm <- implist[[i]]$frlunch - mean_frlunch
  implist[[i]]$teachexp.cgm <- implist[[i]]$teachexp - mean_teachexp
}

# analysis
fit <- with(implist, lmer(probsolve ~ month7  + male.cgm + frlunch.cgm + teachexp.cgm + condition + month7:condition + (1 + month7 | school/student), REML = T))

# pooling
estimates <- testEstimates(fit, extra.pars = T)
estimates
confint(estimates) # confidence intervals

# test conditional effect (simple slope) at condition = 0
testConstraints(fit, constraints = 'month7 + month7*condition*0')

# test conditional effect (simple slope) at condition = 1
testConstraints(fit, constraints = 'month7 + month7*condition*1')

