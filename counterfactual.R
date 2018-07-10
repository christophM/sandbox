

# Title: Plausible Counterfactual Explanations

# Finds counterfactual explanations for predictions that are close to original instance, 
#   only one feature is different and that are likely based on distribution of data
# 
# Optimization is done via grid search
# Optimizes: 

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

shuffle = function(x, times = 1) {
  replace = FALSE
  if(times > 1) {
    replace = TRUE
  }
  data.frame(lapply(x,  function(x) x[sample(1:length(x), size = times * length(x), replace = replace)]))
}

bike.x.shuffled = shuffle(bike.x, times = 1)
bike.x.shuffled = shuffle(bike.x, times = 1)
bike.x$real.dat = 1
bike.x.shuffled$real.dat = 0
bike.x.combined = rbind(bike.x, bike.x.shuffled)
bike.x.combined$real.dat = as.factor(bike.x.combined$real.dat)

rf.dist = randomForest(real.dat ~ ., data = bike.x.combined, ntree = 500)

w = function (dat) {
  predict(rf.dist, dat, type = "prob")[,2]
}


pred = Predictor$new(mod.bike, bike)


get.counterfactual = function(pred, x, y.fun, feature, plausibility) {
  dat = pred$data$get.x()
  if (class(dat[[feature]]) == "factor") {
    qt = unique(dat[[feature]]) 
  } else {
    qt = quantile(as.matrix(dat[, feature, with = FALSE], probs = seq(from = 0, to = 1, length.out = 10))[,1])
  }
  X = x[rep(1, times = length(qt)),]
  X[feature] = qt
  p = plausibility(X)
  data.frame(feature = feature, value = qt, plausibility = p,
    plausibility.drop = plausibility(x) - p,
    y.fun(pred$predict(newdata = X)))
}

get.counterfactuals = function(pred, x, y.fun, dat, plausibility) {
  res = lapply(pred$data$feature.names, function(feature) { 
    get.counterfactual(pred = pred, x = x, y.fun = y.fun, feature = feature, plausibility = plausibility)
  })
  rbindlist(res)
}
x = bike[3,]
res = get.counterfactuals(pred = pred, x = x, function(pred){pred > 2000}, plausibility = w)
res.pos = res[.prediction==TRUE, ]

best = res.pos[nrow(res.pos),]
res.pos[order(plausibility)]

x
res[, .(has.cf = any(.prediction)), by = feature]


## Implement simple counterfactual
# - iterate over all features
# - iterate over all values
# - replace instance of interest feature value
# - get prediction
# - check if prediction as desired
# 

# Iterate over all instances, check coverage

# Checkout RAshomon


## Later
# Implement spheres
# Implement Wachter CF



# 
# library(data.table)
# library(mlr)
# 
# devtools::load_all()
# data(bike)
# 
# madk = function (x) {
#   median(abs(x - median(x)))
# }
# 
# 
# mad = function (X) {
#  apply(X, 2, madk)
# }
# 
# 
# 
# mad.dist = function(x, x.star, mads) {
#   sum(abs(x - x.star) / mads)
# }
# 
# data(cervical)
# cervical.x = cervical[-which(names(cervical) %in% "Biopsy")]
# 
# cervical.task = makeClassifTask(data = cervical, target = "Biopsy")
# mod.cervical = mlr::train(mlr::makeLearner(cl = 'classif.randomForest', predict.type = "prob"), cervical.task)
# 
# f.mod = function(x){
#   getPredictionProbabilities(predict(mod.cervical, newdata = x))
# }
# 
# 
# loss.iris = function(x, x.initial = cervical.x[1,], y.target = 2, X = cervical.x, f = f.mod, lambda = 0.1
#   ){
#   mads = mad(X)
#   mads.positive = mads > 0
#   #lambda * (f(x) - y.target) + gower_dist(x, x.initial)
#   lambda * (f(x) - y.target)^2 + mad.dist(x[mads.positive], x.initial[mads.positive], mads[mads.positive])
#   
# }
# 
# 
# loss.iris(cervical.x[1,], lambda = 10)
# loss.iris(cervical.x[2,], lambda = 10)
# loss.iris(cervical.x[3,], lambda = 10)
# loss.iris(cervical.x[4,], lambda = 10)
# loss.iris(cervical.x[5,], lambda = 10)
# loss.iris(cervical.x[6,], lambda = 10)
# lapply(1:nrow(cervical.x), function(i){loss.iris(cervical.x[i,])})
# 
# optim(par =  cervical.x[4,], fn = loss.iris )
