##################################################
# fiml moderated regression
##################################################

library(fdir)
library(lavaan)
library(mdmb)

##################################################
# set working directory and load data
##################################################

set()
load('behaviorachievement.rda')

##################################################
# estimate means and center variables
##################################################

# estimate sample statistics in lavaan
model <- "stanread7 ~ 1; read2 ~ 1; read9 ~ 1; read1 ~ 1; lrnprob1 ~ 1; atrisk ~ 1;"
descriptives <- inspectSampleCov(model, behaviorachievement, missing = "fiml")

# center lower-order variables
behaviorachievement$read1.cgm <- behaviorachievement$read1 - descriptives$mean["read1"]
behaviorachievement$lrnprob1.cgm <- behaviorachievement$lrnprob1 - descriptives$mean["lrnprob1"]

##################################################
# analysis
##################################################

# summaries to determine ranges for pseudo-imputations
summary(behaviorachievement)

# set ranges (nodes) for pseudo-imputations
nodes.stanread7 <- seq(80, 420, by = 5)
nodes.read2 <- seq(10, 160, by = 2)
nodes.read9 <- seq(30, 130, by = 2)
nodes.read1 <- seq(-55, 75, by = 2)
nodes.lrnprob1 <- seq(-30, 50, by = 2)
nodes.atrisk <- c(0,1)

# model for behsymp1 predictor
model.atrisk <- list( "model" = "logistic", "formula" = atrisk ~ 1, nodes = nodes.atrisk)

# model for lrnprob1 predictor
model.lrnprob1 <- list( "model" = "linreg", "formula" = lrnprob1.cgm ~ atrisk, nodes = nodes.lrnprob1)

# model for read1 predictor
model.read1 <- list( "model" = "linreg", "formula" = read1.cgm ~ lrnprob1.cgm + atrisk, nodes = nodes.read1)

# focal model for read9 outcome
model.read9 <- list( "model" = "linreg", "formula" = read9 ~ read1.cgm + lrnprob1.cgm + read1.cgm*lrnprob1.cgm + atrisk, nodes = nodes.read9)

# model for read2 auxiliary variable
model.read2 <- list("model" = "linreg", "formula" = read2 ~ read9 + read1.cgm + lrnprob1.cgm + atrisk, nodes = nodes.read2)

# model for stanread7 auxiliary variable
model.stanread7 <- list("model" = "linreg", "formula" = stanread7 ~ read2 + read9 + read1.cgm + lrnprob1.cgm + atrisk, nodes = nodes.stanread7)

# combine predictor models into a list
predictor.models <- list(atrisk = model.atrisk, lrnprob1 = model.lrnprob1, read1 = model.read1, read9 = model.read9, read2 = model.read2)

# estimate factored regression model w mdmb
fit <- frm_em(dat = behaviorachievement, dep = model.stanread7, ind = predictor.models)
summary(fit)
