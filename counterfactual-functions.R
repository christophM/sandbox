
get.prob.fun = function(dat) {
  shuffle = function(x, times = 1) {
    replace = FALSE
    if(times > 1) {
      replace = TRUE
    }
    data.frame(lapply(x,  function(x) x[sample(1:length(x), size = times * length(x), replace = replace)]))
  }
  dat.shuffled = shuffle(dat, times = 1)
  dat.shuffled = shuffle(dat, times = 1)
  dat$real.dat = 1
  dat.shuffled$real.dat = 0
  dat.combined = rbind(dat, dat.shuffled)
  dat.combined$real.dat = as.factor(dat.combined$real.dat)

  rf.dist = randomForest(real.dat ~ ., data = bike.x.combined, ntree = 500)

  function (x) {
    predict(rf.dist, x, type = "prob")[,2]
  }
}



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
  prediction = pred$predict(newdata = X)
  data.frame(feature = feature, value = qt, plausibility = p,
    plausibility.drop = plausibility(x) - p,
    .prediction = prediction,
    counterfact = y.fun(prediction)[,1])
}


get.counterfactuals = function(pred, x, y.fun, dat, plausibility) {
  res = lapply(pred$data$feature.names, function(feature) {
    get.counterfactual(pred = pred, x = x, y.fun = y.fun, feature = feature, plausibility = plausibility)
  })
  rbindlist(res)
}
