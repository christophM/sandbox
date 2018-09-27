# Title: Multicrit performance and interpretability

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

pred = Predictor$new(mod.bike, bike.x, y = bike$cnt)

ale = FeatureEffect(pred, "temp")

# GE function

# Linearity measure
score_linearity = function(predictor, dat) {

}

# Measure linearity per feature
score_linearity_feature = function(predictor, feature_name) {
  # Fit ALE plot
  ale = FeatureEffect$new(predictor, feature_name)
  # make continuous spline function
  # https://en.wikipedia.org/wiki/Smoothing_spline
  ale.inter  = smooth.spline(ale$results$temp, ale$results$.ale)
  # measure spline complexity
  # TODO: Weight by the density at each grid point
  mean(abs(predict(ale.inter, unique(predictor$X[feature_name]))$y))
}


xx = score_linearity_feature(pred, "temp")

xx(29)

# Interaction measure
score_interaction = function(predictor, dat) {

}

