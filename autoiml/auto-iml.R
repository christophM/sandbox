# Title: Multicrit performance and interpretability

## Build a model
library(data.table)
library(mlr)
library("iml")
library("ggplot2")
library(randomForest)
library(sensitivity)
load("~/repos/interpretable-ml-book/data/bike.RData")
source("sobol.R")

bike.task = makeRegrTask(data = bike, target = "cnt")
mod.bike = mlr::train(mlr::makeLearner(cl = 'regr.randomForest', id = 'bike-rf'), bike.task)
lm.bike = mlr::train(mlr::makeLearner(cl = 'regr.lm'), bike.task)
rpart.bike = mlr::train(mlr::makeLearner(cl = 'regr.rpart'), bike.task)

bike.x = bike[setdiff(names(bike), "cnt")]

pred = Predictor$new(mod.bike, bike.x, y = bike$cnt)
pred.lm = Predictor$new(lm.bike, bike.x, y = bike$cnt)
pred.rpart = Predictor$new(rpart.bike, bike.x, y = bike$cnt)




sum(unlist(sobol(pred, n = 10000)))
sum(unlist(sobol(pred.lm, n = 10000)))
sum(unlist(sobol(pred.rpart, n = 10000)))


predict.fun = function(X){
  pred.lm$predict(X)[1]
}

# sobol(predict.fun, bike.x, bike.x)
#
# sobol = function(predictor){
#
# }


# GE function

FeatureEffect$new(pred.lm, "temp")$plot()
score_linearity_feature(pred, "temp")
score_linearity_feature(pred, "hum")
score_linearity_feature(pred, "windspeed")

score_linearity(pred)
round(score_linearity(pred.lm))
score_linearity(pred.rpart)


score_linearity_feature(pred.rpart, "temp")
score_linearity_feature(pred.rpart, "hum")
score_linearity_feature(pred.rpart, "windspeed")
