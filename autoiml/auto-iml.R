# Title: Multicrit performance and interpretability

## Build a model
library(data.table)
library(mlr)
library("iml")
library("ggplot2")
library(randomForest)
library(sensitivity)
load("~/repos/interpretable-ml-book/data/bike.RData")
source("autoiml/iml-measures.R")

bike.task = makeRegrTask(data = bike, target = "cnt")



mod.bike = mlr::train(mlr::makeLearner(cl = 'regr.svm', id = 'bike-rf'), bike.task)
lm.bike = mlr::train(mlr::makeLearner(cl = 'regr.lm'), bike.task)
rpart.bike = mlr::train(mlr::makeLearner(cl = 'regr.rpart'), bike.task)

bike.x = bike[setdiff(names(bike), "cnt")]

pred = Predictor$new(mod.bike, bike.x, y = bike$cnt)
pred.lm = Predictor$new(lm.bike, bike.x, y = bike$cnt)
pred.rpart = Predictor$new(rpart.bike, bike.x, y = bike$cnt)


library(mlrMBO)
par.set = makeParamSet(
  makeIntegerParam ("max_depth" , lower = 1,
    upper = 10),
  makeDiscreteParam("booster", values = c("gbtree", "gblinear")),
  makeNumericParam("alpha", lower = 0, upper = 1),
  makeIntegerParam("nrounds", lower = 1, upper = 500)
)

rin = makeResampleInstance(cv2 , bike.task)

lrn = mlr::makeLearner(cl = 'regr.xgboost.mod')

fn = function(x){
  lrn = setHyperPars(lrn, par.vals = x)
  perf = resample(learner = lrn, show.info = FALSE,
    task = bike.task , resampling = rin ,
    measures = list(mse))$aggr
  mod = train(lrn, bike.task)
  pred = Predictor$new(mod, bike.x)
  sob = interaction.strength(pred)
  lin = score_linearity(pred)
  c(mae = perf, interaction = sob, lin = lin)
}
obj.fun = makeMultiObjectiveFunction(fn = fn, par.set = par.set, n.objectives = 3, has.simple.signature = FALSE)

ctrl = makeMBOControl(n.objectives = 3L)
ctrl = setMBOControlInfill(ctrl, crit = crit.cb)
ctrl = setMBOControlMultiObj(ctrl, method = "parego")

mbo.lrn = makeLearner("regr.randomForest", predict.type = "se")

design = generateDesign(n = 10L, par.set = par.set, fun = lhs::randomLHS)

mbo.iml = mbo(fun = obj.fun, design = design, learner = mbo.lrn, control = ctrl)


pareto.set = rbindlist(lapply(mbo.iml$pareto.set, data.frame))
cbind(round(mbo.iml$pareto.front, 2), pareto.set)


xgb = setHyperPars(lrn, par.vals = mbo.iml$pareto.set[[6]])
mod = train(xgb, bike.task)
summary(mod$learner.model)


pred = Predictor$new(mod, bike.x)
plist = lapply(colnames(bike.x), function(x){
  FeatureEffect$new(pred, x, method = "pdp+ice")$plot()
})
plist
Interaction$new(pred, grid.size = 100)$plot()

sum(unlist(sobol(pred, n = 1000)))
sum(unlist(sobol(pred.lm, n = 1000)))
sum(unlist(sobol(pred.rpart, n = 1000)))


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



Interaction$new(pred)$plot()

Interaction$new(pred.lm)$plot()

Interaction$new(pred.rpart)$plot()

surrogate_sim(pred)
surrogate_sim(pred, "lm")

surrogate_sim(pred.lm)
surrogate_sim(pred.lm, "lm")

surrogate_sim(pred.rpart)
surrogate_sim(pred.rpart, "lm")


surrogate_any = function(pred) {
  max(surrogate_sim(pred, "tree"), surrogate_sim(pred, "lm"))
}



surrogate_any(pred)
surrogate_any(pred.lm)
surrogate_any(pred.rpart)



