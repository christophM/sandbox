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
lm.bike = mlr::train(mlr::makeLearner(cl = 'regr.lm'), bike.task)

bike.x = bike[setdiff(names(bike), "cnt")]

pred = Predictor$new(mod.bike, bike.x, y = bike$cnt)
pred.lm = Predictor$new(lm.bike, bike.x, y = bike$cnt)
FeatureEffect$new(pred.lm, "temp")$plot()
score_linearity_feature(pred, "temp")
score_linearity_feature(pred, "hum")
score_linearity_feature(pred, "windspeed")

# GE function

# Linearity measure
score_linearity = function(predictor) {
  scores = lapply(predictor$data$feature.names, function(feature_name) {
    score_linearity_feature(predictor, feature_name)
  })
  # NAs are from categorical features
  mean(unlist(scores), na.rm = TRUE)
}

score_linearity(pred)
score_linearity(pred.lm)

# Measure linearity per feature
score_linearity_feature = function(predictor, feature_name) {
  feature_type = predictor$data$feature.types[feature_name]
  if(feature_type == "categorical") {
    NA
  } else {
    # Fit ALE plot
    ale = FeatureEffect$new(predictor, feature_name)

    # Weight by density
    dens = density(predictor$data$X[,feature_name,with=FALSE][[1]])
    dens_fun = approxfun(dens$x, dens$y)

    # make continuous spline function
    # https://en.wikipedia.org/wiki/Smoothing_spline
    ale.inter = smooth.spline(ale$results[, feature_name], ale$results$.ale, w = dens_fun(ale$results[, feature_name]))

    # measure spline complexity
    weighted.mean(abs(predict(ale.inter, dens$x, deriv = 2)$y), w = dens$y)
  }
}


xx = score_linearity_feature(pred, "temp")

xx(29)

# Interaction measure
score_interaction = function(predictor, dat) {

}

