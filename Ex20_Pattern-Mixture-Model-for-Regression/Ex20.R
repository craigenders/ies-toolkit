##################################################
# mcmc regression w mnar pattern mixture model
##################################################

library(fdir)
library(rblimp)
library(mitml)

##################################################
# set working directory and load data
##################################################

set()
load('behaviorachievement.rda')

##################################################
# fit model with mcmc (ex20.imp)
##################################################

mymodel <- rblimp(
  data = behaviorachievement,
  transform = 'm = 1 - ismissing(read9)',
  ordinal = 'm',
  model = '  
   focal.model:
   read9 ~ 1@beta0com m@beta0diff read1 lrnprob1 behsymp1;
   missingness.model:
   m ~ 1@missmean;
   predictor.model:
   read1 lrnprob1 behsymp1 ~ m;
   auxiliary.model:
   stanread7 read2  ~ read9 m read1 lrnprob1 behsymp1',
  parameters = 'cohensd = -.20;
   beta0diff = cohensd * sqrt(read9.totalvar);
   pmis = phi(missmean);
   pcom = 1 - pmis;
   beta0 = (beta0com * pcom) + ((beta0com + beta0diff) * pmis)',
  seed = 90291,
  burn = 1000,
  iter = 10000)
output(mymodel)
