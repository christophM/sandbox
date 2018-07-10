library("iml")
library("ggplot2")
library("dplyr")
library("randomForest")
library(gridExtra)
library(mlr)
library(ALEPlot)


load(url("https://github.com/christophM/interpretable-ml-book/blob/master/data/bike.RData?raw=true"))

#bike$temp2 = bike$temp + rnorm(n = nrow(bike), sd = 1)
bike.x = bike[setdiff(names(bike), "cnt")]


shuffle = function(x, times = 1) {
  replace = FALSE
  if(times > 1) {
    replace = TRUE
  }
  data.frame(lapply(x,  function(x) x[sample(1:length(x), size = times * length(x), replace = replace)]))
}

bike.x.shuffled = shuffle(bike.x, times = 1)


bike.x$real.dat = 1
bike.x.shuffled$real.dat = 0
bike.x.combined = rbind(bike.x, bike.x.shuffled)
bike.x.combined$real.dat = as.factor(bike.x.combined$real.dat)

rf.dist = randomForest(real.dat ~ ., data = bike.x.combined, ntree = 500)

w = function (dat) {
  predict(rf.dist, dat, type = "prob")[,2]
}

w(bike.x[1,])
w(bike.x.shuffled[1,])



bike.task = makeRegrTask(data = bike, target = "cnt")
mod.bike = mlr::train(mlr::makeLearner(cl = 'regr.rpart', id = 'bike-rf'), bike.task)
mod.bike$learner.model
pred.bike = Predictor$new(mod.bike, data = bike)
pdp = Partial$new(pred.bike, "temp", ice = FALSE) 




ddat  = pdp$.__enclos_env__$private$dataDesign

summary(w(ddat))


ddat$weight = w(ddat)


ddat$prediction = pdp$.__enclos_env__$private$qResults

ddat2 =  ddat %>% group_by(temp) %>%
  summarise(wpredict = sum(prediction))

ddat2 = ddat %>% 
  filter(weight > 0.9) %>%
  group_by(temp) %>%
  summarise(wsum = sum(weight), wpredict = sum(weight * prediction)/wsum, predict = mean(prediction))

p1 = plot(pdp)
p2 = ggplot(ddat2) + geom_line(aes(x = temp, y = wpredict))
p3 = ggplot(ddat2) + geom_line(aes(x = temp, y = predict))
bike.xy = bike
bike.xy$prediction = getPredictionResponse(predict(mod.bike, newdata = bike.xy))
p4 = ggplot(bike.xy, aes(x = temp, y = prediction)) + geom_point() + geom_smooth()
grid.arrange(p1, p2, p3, p4)


pred.fun = function(X.model, newdata) X.model$predict(newdata)[,1]

ALEPlot(bike.x, X.model = pred.bike, J = "temp", pred.fun = pred.fun)


plot(bike.x$temp, pred.bike$predict(bike.x)[,1])
p1 

filter(ddat, temp == max(temp)) %>% arrange(temp) %>% select(temp, temp, weight, prediction) 


ddat2.ice = ddat %>% 
  filter(weight > 0.8) 

ggplot(ddat2.ice) + geom_line(aes(group = .id.dist, x = temp, y = prediction))


