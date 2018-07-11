

# Title: Plausible Counterfactual Explanations

# Finds counterfactual explanations for predictions that are close to original instance,
#   only one feature is different and that are likely based on distribution of data
#
# Optimization is done via grid search
#
# Can be used for a (1D) feature importance measure:
# Number of data points that can be turned into a counterfactual by changing feature x_j
#

source("./counterfactual-functions.R")
## Build a model
library(data.table)
library(mlr)
library("iml")
library("ggplot2")
library(randomForest)
load("~/repos/interpretable-ml-book/data/bike.RData")


bike.task = makeRegrTask(data = bike, target = "cnt")
mod.bike = mlr::train(mlr::makeLearner(cl = 'regr.randomForest', id = 'bike-rf'), bike.task)
bike.x = bike[setdiff(names(bike), "cnt")]

w = get.prob.fun(bike.x)
pred = Predictor$new(mod.bike, bike.x, y = bike$cnt)


x = bike[3,]
res = get.counterfactuals(pred = pred, x = x, function(pred){pred > 2000}, plausibility = w)
res.pos = res[counterfact==TRUE, ]

best = res.pos[nrow(res.pos),]
res.pos[order(plausibility)]

x
res[, .(has.cf = any(.prediction)), by = feature]

# Iterate over all instances
# Summary Per feature how many will find cf
index = sample(1:nrow(bike.x), size = 10)
in.cf = lapply(index, function(x) {
  res = get.counterfactuals(pred = pred, x = bike.x[x,], function(pred){pred > 3500}, plausibility = w)
  res$id = x
  res
})

res.pos = res[counterfact==TRUE, ]

in.cf2 = rbindlist(in.cf)

in.cf3 = in.cf2[, .(has.cf = any(.prediction)), by = list(id, feature)]

in.cf3[, sum(has.cf)/length(index), by = feature]

res.df = data.frame(res)
ggplot(in.cf2[in.cf2$feature == "temp",]) + geom_line(aes(x = value, y = .prediction, group = id))



 ## Later
# Implement spheres
# Implement Wachter CF
