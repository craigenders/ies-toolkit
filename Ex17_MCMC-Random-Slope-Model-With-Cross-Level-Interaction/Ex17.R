##################################################
# mcmc mlm with cross-level interaction
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
# fit model with mcmc (ex17.1.imp)
##################################################

mymodel <- rblimp(
  data = problemsolving3level,
  clusterid = 'student',
  ordinal = 'hispanic frlunch condition',
  fixed = 'month condition',
  center = 'grandmean = frlunch',
  model = ' 
   probsolve ~ month hispanic frlunch condition month*condition month*hispanic | month',
  simple = 'month | condition; month | hispanic',
  seed = 90291,
  burn = 10000,
  iter = 20000)
output(mymodel)
simple_plot(probsolve ~ month | condition, mymodel)
simple_plot(probsolve ~ month | hispanic, mymodel)

##################################################
# fit model and save imputations (ex17.2.imp)
##################################################

mymodel <- rblimp(
  data = problemsolving3level,
  clusterid = 'student',
  ordinal = 'hispanic frlunch condition',
  fixed = 'month condition',
  center = 'grandmean = frlunch',
  model = ' 
   probsolve ~ month hispanic frlunch condition month*condition month*hispanic | month',
  simple = 'month | condition; month | hispanic',
  seed = 90291,
  burn = 10000,
  iter = 20000,
  nimps = 20,
  chains = 20)
output(mymodel)
simple_plot(probsolve ~ month | condition, mymodel)
simple_plot(probsolve ~ month | hispanic, mymodel)

##################################################
# analysis and pooling
##################################################

# mitml list
implist <- as.mitml(mymodel)

# center covariate at the grand mean
mean_frlunch <- mean(unlist(lapply(implist, function(df) mean(df$frlunch))))
for (i in 1:length(implist)) {
  implist[[i]]$frlunch.cgm <- implist[[i]]$frlunch - mean_frlunch
}

# analysis
fit <- with(implist, lmer(probsolve ~ month + frlunch.cgm + hispanic + condition + month:condition + month:hispanic + (1 + month | student), REML = T))

# pooling
estimates <- testEstimates(fit, extra.pars = T)
estimates
confint(estimates) # confidence intervals

# test conditional effect (simple slope) at condition = 0
testConstraints(fit, constraints = 'month + month*condition*0')

# test conditional effect (simple slope) at condition = 1
testConstraints(fit, constraints = 'month + month*condition*1')

# test conditional effect (simple slope) at hispanic = 0
testConstraints(fit, constraints = 'month + month*hispanic*0')

# test conditional effect (simple slope) at hispanic = 1
testConstraints(fit, constraints = 'month + month*hispanic*1')

