##################################################
# fiml linear regression
##################################################

library(lavaan)

##################################################
# load data from github
##################################################

data_url <- "https://raw.githubusercontent.com/craigenders/ies-toolkit/main/Data/behaviorachievement.rda"
load(gzcon(url(data_url, open = "rb")))

##################################################
# analysis
##################################################

# estimate model in lavaan
model <- 'read9 ~ b1*read1 + b2*lrnprob1 + b3*behsymp1'
fit <- sem(model, behaviorachievement, fixed.x = F, missing = "fiml")
# missing data patterns and proportion observed data (coverage)
inspect(fit, "patterns")
inspect(fit, "coverage")
summary(fit, rsquare = T, standardize = T)

# wald test that all slopes equal 0
wald.constraints <- 'b1 == 0; b2 == 0; b3 == 0;'
lavTestWald(fit, constraints = wald.constraints)



