library("iml")
library("mlr")

devtools::load_all("~/repos/paper_2018_autoiml/")

generate_task = function(n){
  n_features = 20
  x = rnorm(n * n_features)
  x = data.frame(matrix(x, ncol = n_features))
  y = 7 * x[,1] + 5 * x[,2] * x[,3] - sqrt(abs(x[,3])) - x[,4]^4 * x[,5] + rnorm(n)
  makeRegrTask(data = cbind(data.frame(y = y), x), target = "y")
}

tsk = generate_task(10000)
dat = getTaskData(tsk)

# train model
lrn = makeLearner("regr.ranger") 
mod = train(lrn, tsk)
pred = Predictor$new(mod, dat)

# AleApprox
fc = FunComplexity$new(pred)




predic = getPredictionResponse(predict(mod, tsk))
dat.new = dat
dat.new$y = predic
tsk.new = makeRegrTask(data = dat.new, target = "y")
tsk.new = tsk
# Train xgboost
lrn2 = makeLearner("regr.gamboost")
mod2 = train(lrn2, tsk.new)

# Train lm
lrn3 = makeLearner("regr.lm")
mod3 = train(lrn3, tsk.new)



# Test on test data
test.tsk = generate_task(50000) 
test.dat = getTaskData(test.tsk)
pred.fc = fc$predict(test.dat)
pred1 = getPredictionResponse(predict(mod, test.tsk))
pred2 = getPredictionResponse(predict(mod2, test.tsk))
pred3 = getPredictionResponse(predict(mod3, test.tsk))

fc.miss = is.na(pred.fc)

measureMSE(test.dat$y[!fc.miss], pred.fc[!fc.miss])
measureMSE(test.dat$y[!fc.miss], pred1[!fc.miss])
measureMSE(test.dat$y[!fc.miss], pred2[!fc.miss])
measureMSE(test.dat$y[!fc.miss], pred3[!fc.miss])

fe.gam = FeatureEffects$new(Predictor$new(mod2, data = dat))
plot(fe.gam)






## Explain the residuals
residual = predic - fc$predict(dat)
resid.dat = dat
resid.dat$y = residual
resid.tsk = makeRegrTask(data = resid.dat, target = "y")

tree = makeLearner("regr.rpart")
resid.tree = train(tree, resid.tsk)
resid.tree$learner.model
