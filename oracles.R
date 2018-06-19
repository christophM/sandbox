


library("randomForest")
data(bike)


bike$temp2 = bike$temp + rnorm(n = nrow(bike), sd = 1)

bike.x = bike[setdiff(names(bike), "cnt")]


shuffle = function(x) {
  data.frame(lapply(x,  function(x) x[sample(1:length(x))]))
}

bike.x.shuffled = shuffle(bike.x)

bike.x$real.dat = 1
bike.x.shuffled$real.dat = 0
bike.x.combined = rbind(bike.x, bike.x.shuffled)
bike.x.combined$real.dat = as.factor(bike.x.combined$real.dat)

rf.dist = randomForest(real.dat ~ ., data = bike.x.combined, keep.forest = TRUE)



w = function (dat) {
  predict(rf.dist, dat, type = "prob")[,2]
}

w(bike.x[1,])
w(bike.x.shuffled[1,])



library(mlr)
bike.task = makeRegrTask(data = bike, target = "cnt")
mod.bike = mlr::train(mlr::makeLearner(cl = 'regr.randomForest', id = 'bike-rf'), bike.task)

pred.bike = Predictor$new(mod.bike, data = bike)
pdp = Partial$new(pred.bike, "temp2", ice = FALSE) 


library(iml)


ddat  = pdp$.__enclos_env__$private$dataDesign

summary(w(ddat))


ddat$weight = w(ddat)

library(dplyr)

ddat$prediction = pdp$.__enclos_env__$private$qResults
ddat2 = ddat %>% group_by(temp2) %>%
  filter(weight > 0.8) %>%
  summarise(wsum = sum(weight), wpredict = sum(weight * prediction)/wsum)

library(ggplot2)
p1 = plot(pdp)
p2 = ggplot(ddat2) + geom_line(aes(x = temp2, y = wpredict))

library(gridExtra)
grid.arrange(p1, p2)

library(ALEPlot)

pred.fun = function(X.model, newdata) X.model$predict(newdata)[,1]

ALEPlot(bike.x, X.model = pred.bike, J = "temp2", pred.fun = pred.fun)




filter(ddat, temp2 == max(temp2)) %>% arrange(temp) %>% select(temp2, temp, weight, prediction) 



