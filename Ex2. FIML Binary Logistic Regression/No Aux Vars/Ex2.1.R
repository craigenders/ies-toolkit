# example x:logistic regression with fiml estimation
library(fdir)
library(mdmb)

##################################################
# read data
##################################################

# set working directory
set()

# read raw data from working directory
data <- read.table("behaviorachievement.dat", na.strings = "999")
names(data) <- c("id","male","hispanic","atriskgrp","atrisk","behsymp1","lrnprob1",
   "read1","read2","read3","read9","read9grp","stanread7",
   "math1","math2","math3","math9","math9grp","stanmath7")

##################################################
# analysis
##################################################

# summarize incomplete predictors to determine ranges for pseudo-imputations
summary(data[,c("read9grp","read1","lrnprob1","behsymp1")])

# set ranges (nodes) for pseudo-imputations
nodes.read9grp <- c(0,1)
nodes.read1 <- seq(30, 160, by = 2)
nodes.lrnprob1 <- seq(20, 100, by = 2)
nodes.behsymp1 <- seq(10, 100, by = 2)

# model for behsymp1 predictor: f(behsymp1)
model.behsymp1 <- list( "model" = "linreg", "formula" = behsymp1 ~ 1, nodes = nodes.behsymp1)

# model for lrnprob1 predictor: f(lrnprob1|behsymp1)
model.lrnprob1 <- list( "model" = "linreg", "formula" = lrnprob1 ~ behsymp1, nodes = nodes.lrnprob1)

# model for read1 predictor: f(read1|lrnprob1,behsymp1)
model.read1 <- list( "model" = "linreg", "formula" = read1 ~ lrnprob1 + behsymp1, nodes = nodes.read1)

# model for read9grp outcome: f(read9grp|read1,lrnprob1,behsymp1)
model.read9grp <- list( "model" = "logistic", "formula" = read9grp ~ read1 + lrnprob1 + behsymp1, nodes = nodes.read9grp)

# combine predictor models into a list
predictor.models <- list(behsymp1 = model.behsymp1, lrnprob1 = model.lrnprob1, read1 = model.read1)

# estimate factored regression model w mdmb
fit <- frm_em(dat = data, dep = model.read9grp, ind = predictor.models) 
summary(fit)
