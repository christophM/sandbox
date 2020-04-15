library(iml)
library(mlr)
library(ggplot2)

# =============================================================================
# Situation: x is mediator for other feature
# =============================================================================
n = 100

x1 = rnorm(n)
x = rnorm(n, mean = x1)
y = sin(x) + x1 + rnorm(n, sd = 0.1)
dat = data.frame(x, x1, y)
tsk = makeRegrTask(data = dat, target = "y")
lrn = makeLearner("regr.gamboost")
mod = train(lrn, tsk)
pred = Predictor$new(mod, data = dat)

eff = FeatureEffect$new(pred, "x")
true.effect = data.frame(x = seq(from = -3, to = 3, length.out = n))
true.effect$y = sin(true.effect$x)
plot(eff) + geom_line(aes(x = x, y = y), data = true.effect, lty = 2)

# =============================================================================
# Situation: Effect of x is mediated by other feature
# =============================================================================

n = 100

x = rnorm(n)
x1 = rnorm(n, mean = x)
y = 0.1 * x + sin(x1) + rnorm(n, sd = 0.1)
dat = data.frame(x, x1, y)
tsk = makeRegrTask(data = dat, target = "y")
lrn = makeLearner("regr.gamboost")
mod = train(lrn, tsk)
pred = Predictor$new(mod, data = dat)

eff = FeatureEffect$new(pred, "x")
direct.effect = data.frame(x = seq(from = -3, to = 3, length.out = n))
direct.effect$y = 0.1 * direct.effect$x
plot(eff) + geom_line(aes(x = x, y = y), data = direct.effect, lty = 2)


# =============================================================================
# Situation: Effect of x is mediated by other feature; no direct effect 
# =============================================================================

n = 1000

x = rnorm(n)
x1 = rnorm(n, mean = x)
y = sin(x1) + rnorm(n, sd = 0.1)
dat = data.frame(x, x1, y)
tsk = makeRegrTask(data = dat, target = "y")
lrn = makeLearner("regr.rpart")
mod = train(lrn, tsk)
pred = Predictor$new(mod, data = dat)

eff = FeatureEffect$new(pred, "x")
direct.effect = data.frame(x = seq(from = -3, to = 3, length.out = n))
direct.effect$y = 0
plot(eff) + geom_line(aes(x = x, y = y), data = direct.effect, lty = 2)

# => PDP estimates the direct effect correctly
# => PDP does not estimate the total effect





