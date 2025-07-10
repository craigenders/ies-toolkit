##################################################
# fiml regression w binary and ordinal predictors
##################################################

library(mdmb)

##################################################
# load data from github
##################################################

data_url <- "https://raw.githubusercontent.com/craigenders/ies-toolkit/main/Data/mathachievement.rda"
load(gzcon(url(data_url, open = "rb")))

##################################################
# analysis
##################################################

# summaries to determine ranges for pseudo-imputations
summary(mathachievement)

# set ranges (nodes) for pseudo-imputations
nodes.frlunch <- c(0,1)
nodes.efficacy <- seq(1, 6, by = 1)
nodes.mathpost <- seq(30, 90, by = 2)
nodes.atrisk <- c(0,1)
nodes.stanread <- seq(20, 80, by = 2)

# model for frlunch predictor
model.frlunch <- list("model" = "logistic", "formula" = frlunch ~ condition + mathpre, nodes = nodes.frlunch)

# model for efficacy predictor
model.efficacy <- list("model" = "linreg", "formula" = efficacy ~ frlunch + condition + mathpre, nodes = nodes.efficacy)

# model for mathpost outcome
model.mathpost <- list("model" = "linreg", "formula" = mathpost ~ condition + frlunch + efficacy + mathpre, nodes = nodes.mathpost)

# model for atrisk auxiliary variable
model.atrisk <- list("model" = "logistic", "formula" = atrisk ~ mathpost + condition + frlunch + efficacy + mathpre, nodes = nodes.atrisk)

# the commented out code below works if atrisk is the final variable in the sequence

# combine predictor models into a list
# predictor.models <- list(frlunch = model.frlunch, efficacy = model.efficacy, mathpost = model.mathpost)

# estimate factored regression model w mdmb
# fit <- frm_em(dat = mathachievement, dep = model.atrisk, ind = predictor.models)
# summary(fit)

# the code below doesn't run when stanread is the final variable in the sequence

# model for stanread auxiliary variable
model.stanread <- list("model" = "linreg", "formula" = stanread ~ atrisk + mathpost + condition + frlunch + efficacy + mathpre, nodes = nodes.stanread)

# combine predictor models into a list
predictor.models <- list(frlunch = model.frlunch, efficacy = model.efficacy, mathpost = model.mathpost, atrisk = model.atrisk)

# estimate factored regression model w mdmb
fit <- frm_em(dat = mathachievement, dep = model.stanread, ind = predictor.models)
summary(fit)
