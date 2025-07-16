##################################################
# fiml logistic regression
##################################################

library(mdmb)

##################################################
# load data from github
##################################################

data_url <- "https://raw.githubusercontent.com/craigenders/ies-toolkit/main/Data/behaviorachievement.rda"
load(gzcon(url(data_url, open = "rb")))

##################################################
# analysis
##################################################

# summaries to determine ranges for pseudo-imputations
summary(behaviorachievement)

# set ranges (nodes) for pseudo-imputations
nodes.stanread7 <- seq(80, 420, by = 5)
nodes.read2 <- seq(10, 160, by = 2)
nodes.readgrp9 <- c(0,1)
nodes.read1 <- seq(30, 160, by = 2)
nodes.lrnprob1 <- seq(20, 100, by = 2)
nodes.behsymp1 <- seq(10, 100, by = 2)

# model for behsymp1 predictor
model.behsymp1 <- list( "model" = "linreg", "formula" = behsymp1 ~ 1, nodes = nodes.behsymp1)

# model for lrnprob1 predictor:
model.lrnprob1 <- list( "model" = "linreg", "formula" = lrnprob1 ~ behsymp1, nodes = nodes.lrnprob1)

# model for read1 predictor
model.read1 <- list( "model" = "linreg", "formula" = read1 ~ lrnprob1 + behsymp1, nodes = nodes.read1)

# model for readgrp9 outcome
model.readgrp9 <- list( "model" = "logistic", "formula" = readgrp9 ~ read1 + lrnprob1 + behsymp1, nodes = nodes.readgrp9)

# focal model for read2 auxiliary variable f(read2|readgrp9,read1,lrnprob1,behsymp1)
model.read2 <- list("model" = "linreg", "formula" = read2 ~ readgrp9 + read1 + lrnprob1 + behsymp1, nodes = nodes.read2)

# model for stanread7 auxiliary variable 
model.stanread7 <- list("model" = "linreg", "formula" = stanread7 ~ read2 + readgrp9 + read1 + lrnprob1 + behsymp1, nodes = nodes.stanread7)

# combine predictor models into a list
predictor.models <- list(behsymp1 = model.behsymp1, lrnprob1 = model.lrnprob1, read1 = model.read1, readgrp9 = model.readgrp9, read2 = model.read2)

# estimate factored regression model w mdmb
fit <- frm_em(dat = behaviorachievement, dep = model.stanread7, ind = predictor.models) 
summary(fit)

