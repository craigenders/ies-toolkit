##################################################
# mcmc regression w mnar selection model
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
# fit model with mcmc (ex19.imp)
##################################################

mymodel <- rblimp(
  data = behaviorachievement,
  transform = 'm = ismissing(read9)',
  ordinal = 'm',
  model = '  
   focal.model:
   read9 ~ read1 lrnprob1 behsymp1;
   missingness.model:
   m ~ read9 lrnprob1;
   auxiliary.model:
   stanread7 read2  ~ read9 read1 lrnprob1 behsymp1',
  seed = 90291,
  burn = 1000,
  iter = 10000)
output(mymodel)
