##################################################
# mcmc curvilinear regression
##################################################

library(fdir)
library(lavaan)
library(mdmb)

##################################################
# set working directory and load data
##################################################

set()
load('mathachievement.rda')

##################################################
# estimate means and center variables
##################################################

# estimate sample statistics in lavaan
model <- "stanread ~ 1; atrisk ~ 1; mathpost ~ 1; anxiety ~ 1; frlunch ~ 1; efficacy ~ 1; mathpre ~ 1;"
descriptives <- inspectSampleCov(model, mathachievement, missing = "fiml")

# center lower-order variable
mathachievement$anxiety.cgm <- mathachievement$anxiety - descriptives$mean["anxiety"]

##################################################
# analysis
##################################################

# summaries to determine ranges for pseudo-imputations
summary(mathachievement)

# set ranges (nodes) for pseudo-imputations
nodes.frlunch <- c(0,1)
nodes.efficacy <- seq(1, 6, by = .20) 
nodes.anxiety <- seq(-30, 30, by = 2)
nodes.mathpost <- seq(30, 90, by = 2)
nodes.atrisk <- c(0,1)
nodes.stanread <- c(20, 80, by = 2)

# model for frlunch predictor
model.frlunch <- list("model" = "logistic", "formula" = frlunch ~ mathpre, nodes = nodes.frlunch)

# model for efficacy predictor
model.efficacy <- list("model" = "linreg", "formula" = efficacy ~ frlunch + mathpre, nodes = nodes.efficacy)

# model for anxiety predictor
model.anxiety <- list("model"="linreg", "formula" = anxiety.cgm ~ efficacy + frlunch + mathpre, nodes = nodes.anxiety)

# focal model for mathpost outcome
model.mathpost <- list("model" = "linreg", "formula" = mathpost ~ anxiety.cgm + I(anxiety.cgm^2) + efficacy + frlunch + mathpre, nodes = nodes.mathpost)

# model for atrisk auxiliary variable
model.atrisk <- list("model" = "logistic", "formula" = atrisk ~ mathpost + anxiety.cgm + efficacy + frlunch + mathpre, nodes = nodes.atrisk)

# combine predictor models into a list
predictor.models <- list(frlunch = model.frlunch, efficacy = model.efficacy, anxiety = model.anxiety, mathpost = model.mathpost)

# estimate factored regression model w mdmb
fit <- frm_em(dat = mathachievement, dep = model.atrisk, ind = predictor.models) 
summary(fit)

# 8/17/24: doesn't run with additional auxiliary variable

# model for stanread auxiliary variable
# model.stanread <- list("model" = "linreg", "formula" = stanread ~ atrisk + mathpost + anxiety.cgm + efficacy + frlunch + mathpre, nodes = nodes.stanread)

# combine predictor models into a list
# predictor.models <- list(frlunch = model.frlunch, efficacy = model.efficacy, anxiety = model.anxiety, mathpost = model.mathpost, atrisk = model.atrisk)

# estimate factored regression model w mdmb
# fit <- frm_em(dat = mathachievement, dep = model.stanread, ind = predictor.models) 
# summary(fit)

