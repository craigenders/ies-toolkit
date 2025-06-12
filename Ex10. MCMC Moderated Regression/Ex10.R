##################################################
# mcmc moderated regression
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
# fit model with mcmc (ex10.1.imp)
##################################################

mymodel <- rblimp(
  data = behaviorachievement,
  ordinal = 'atrisk',
  center = 'read1 lrnprob1 atrisk',
  model = '  
   focal.model:
   read9 ~ read1 lrnprob1 read1*lrnprob1 atrisk ;
   auxiliary.models:
   stanread7 read2  ~ read9 read1 lrnprob1 atrisk',
  simple = 'read1 | lrnprob1',
  seed = 90291,
  burn = 5000,
  iter = 10000)
output(mymodel)

simple_plot(read9 ~ read1 | lrnprob1, mymodel)
jn_plot(read9 ~ read1 | lrnprob1, mymodel)

##################################################
# fit model and save imputations (ex10.2.imp)
##################################################

mymodel <- rblimp(
  data = behaviorachievement,
  ordinal = 'atrisk',
  center = 'read1 lrnprob1 atrisk',
  model = '  
   focal.model:
   read9 ~ read1 lrnprob1 read1*lrnprob1 atrisk ;
   auxiliary.models:
   stanread7 read2  ~ read9 read1 lrnprob1 atrisk',
  simple = 'read1 | lrnprob1',
  seed = 90291,
  burn = 5000,
  iter = 10000,
  nimps = 20,
  chains = 20)
output(mymodel)

simple_plot(read9 ~ read1 | lrnprob1, mymodel)
jn_plot(read9 ~ read1 | lrnprob1, mymodel)

##################################################
# analysis and pooling
##################################################

# mitml list
implist <- as.mitml(mymodel)

# center lower-order variables
mean_read1 <- mean(unlist(lapply(implist, function(df) mean(df$read1))))
mean_lrnprob1 <- mean(unlist(lapply(implist, function(df) mean(df$lrnprob1))))
mean_atrisk <- mean(unlist(lapply(implist, function(df) mean(df$atrisk))))
for (i in 1:length(implist)) {
  implist[[i]]$read1.cgm <- implist[[i]]$read1 - mean_read1
  implist[[i]]$lrnprob1.cgm <- implist[[i]]$lrnprob1 - mean_lrnprob1
  implist[[i]]$atrisk.cgm <- implist[[i]]$atrisk - mean_atrisk
}

# analysis
fit <- with(implist, lm(read9 ~ read1.cgm + lrnprob1.cgm + read1.cgm:lrnprob1.cgm + atrisk.cgm))

# pooling + barnard & rubin df for t-tests
estimates <- testEstimates(fit, extra.pars = T, df.com = 133)
estimates
confint(estimates) # confidence intervals

# pooled r-square
rsquare <- mean(unlist(lapply(implist, (function(x) summary(lm(read9 ~ read1.cgm + lrnprob1.cgm + read1.cgm:lrnprob1.cgm + atrisk.cgm, data = x))$r.squared))))
rsquare

##################################################
# test conditional effects (simple slopes)
##################################################

# pooled standard deviation of the moderator
lrnprob1.sd <- mean(unlist(lapply(implist, (function(x) sd(x$lrnprob1.cgm)))))
lrnprob1.sd

# test conditional effect (simple slope) at +1 SD above the mean
slp_high <- 'read1.cgm + read1.cgm*lrnprob1.cgm*1*10.77'
testConstraints(fit, constraints = slp_high, df.com = 133)

# test conditional effect (simple slope) at the mean
slp_mean <- 'read1.cgm + read1.cgm*lrnprob1.cgm*0*10.77'
testConstraints(fit, constraints = slp_mean, df.com = 133)

# test conditional effect (simple slope) at -1 SD below the mean
slp_low <- 'read1.cgm + read1.cgm*lrnprob1.cgm*-1*10.77'
testConstraints(fit, constraints = slp_low, df.com = 133)