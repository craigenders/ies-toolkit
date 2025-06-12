##################################################
# fiml linear regression
##################################################

library(fdir)
library(lavaan)
library(semTools)

##################################################
# set working directory and load data
##################################################

set()
load('behaviorachievement.rda')

##################################################
# analysis
##################################################

# estimate model in lavaan
model <- 'read9 ~ b1*read1 + b2*lrnprob1 + b3*behsymp1'
fit <- sem.auxiliary(model, behaviorachievement, fixed.x = F, aux = c("hispanic","read2","stanread7"))
summary(fit, rsquare = T, standardize = T)

# wald test that all slopes equal 0
wald.constraints <- 'b1 == 0; b2 == 0; b3 == 0;'
lavTestWald(fit, constraints = wald.constraints)

# missing data patterns and proportion observed data (coverage)
inspect(fit, "patterns")
inspect(fit, "coverage")