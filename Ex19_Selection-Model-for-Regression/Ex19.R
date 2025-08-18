##################################################
# mcmc regression w mnar selection model
##################################################

library(rblimp)
library(mitml)

##################################################
# load data from github
##################################################

data_url <- "https://raw.githubusercontent.com/craigenders/ies-toolkit/main/Data/behaviorachievement.rda"
load(gzcon(url(data_url, open = "rb")))

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
