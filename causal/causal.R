# Simulation for causal ML
library("iml")
library("mlr")
library("rpart")
library("dplyr")
library("ggplot2") 
set.seed(42)
n = 1000

fx = function(n) rnorm(n)
fm = function(n, x) rnorm(n, mean = x)
fy = function(n, x, m) rnorm(n, mean = 0.1 * x + 0.9 * m)
fz = function(n, y) rnorm(n, mean = -y, sd = 2)
x =  fx(n)
m = fm(n, x)
y = fy(n, x, m)
z = fz(n, y)

dat = data.frame(x, m, z, y)

tsk = makeRegrTask(data = dat, target = "y")
lrn = makeLearner("regr.lm")


mod = train(lrn, tsk)
pred = Predictor$new(mod, data = dat)
fes = FeatureEffects$new(pred, method = "pdp")
plot(fes, ncol = 1)


library("ranger")
fm.h = ranger(m ~ x, data = dat)
fy.h = ranger(y ~ x + m, data = dat)
fz.h = ranger(z ~ y, data = dat)

x.pdp = seq(from = min(x), to = max(x), length.out = 20)
dat.new = cbind(x = rep(x.pdp, times = nrow(dat)), dat[c("m", "y", "z")])

dat.new$m = predict(fm.h, data = dat.new)$predictions
dat.new$y  = predict(fy.h, data = dat.new)$predictions
dat.new$z = predict(fz.h, data = dat.new)$predictions
dat.new.aggregated = dat.new  %>% group_by(x) %>% summarize(y = mean(y))
ggplot(dat.new.aggregated) + geom_line(aes(x = x, y = y))
# Discover causal graphs in wine data
library("pcalg")
