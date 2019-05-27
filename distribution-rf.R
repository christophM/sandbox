library("ggplot2")
library("ranger")
library("solitude")
# Simulate data
# set.seed(42)

generate = function(n) {
  x1 = rnorm(n, mean = 10, sd = 2)
  x4 = rnorm(n, mean = x1^2, sd = 3)
  x2 = rpois(n, lambda = 2)
  x3 = ifelse(x2 > 3, x1 + rnorm(n), x4 + rnorm(n, mean = 2)^2)
  x2 = as.factor(x2)
  data.frame(x1, x2, x3, x4)
}

n = 100
shuffle = function(dat) {
  data.frame(lapply(dat, function(x) sample(x)))
}

generate2 = function(n, y = TRUE){
  dat = generate(n)
  dat2 = shuffle(dat)
  tdat = rbind(dat, dat2)
  if (y) tdat$y =  rep(c(1,0), each = nrow(dat))
  tdat
}

# Calculate P_X from random forest probability p = P_X / (P_X + P_{X,marginal})
# Therefore P_X = p/(1-p) * P_{X,marginal}

# marginal distribution is product of all marginal feature distributions
# kernel density for each feature
get_marginals = function(dat) {
  # Density estimator for each feature
  density_funs = lapply(dat, function(x) {
    if (inherits(x, "factor")) {
      freqs = table(x) / length(x)
      function(z) freqs[z]
    } else {
      approxfun(density(x))
    }
  })
  names(density_funs) = names(dat)
  density_funs
}



dat = generate2(1000, y = FALSE)
tdat = dat[1:1000,]


# Test for one feature
grid.size = 100
x.interest = dat[rep(10, times = grid.size),]
x.interest$x3 = seq(from = min(dat$x3), to = max(dat$x4), length.out = grid.size)

marginals = get_marginals(dat)
x3m = marginals$x3(x.interest$x3)

p = predict(mod, data = x.interest)$prediction
px = approxfun(x.interest$x3, x3m * p / (1 - p))

px(x.interest$x3)

plot(p)
cbind(x.interest, p, x3m)

plot(x.interest$x3, x3m)
plot(x.interest$x3, p)
plot(x.interest$x3, x3m * p/(1-p))



## Test whether this recovers correct distribution in multivariate normal distribution

# simulate multivariate normal distribution with 3 features and some correlation
library("MASS")
n = 10000
mu = c(1,2,3)
sigm = matrix(c(1, 0.7, 0, 0.7, 1, 0, 0, 0, 1), ncol = 3)
dat = data.frame(mvrnorm(n = n, mu = mu, Sigma = sigm))

discrim_rf = function(dat){
  dat2 = shuffle(dat)
  dat$y = 1
  dat2$y = 0
  dat = rbind(dat, dat2)
  ranger(y ~ ., data = dat)
}
# train the random forest
rf = discrim_rf(dat)


get_p = function(rf, dat, feature, i, x.seq) {
  marginals = get_marginals(dat)
  x.interest = dat[rep(i, times = length(x.seq)), ]
  x.interest[[feature]] =  x.seq
  predict(rf, data = x.interest)$prediction
}

get_px = function(rf, dat, feature, i) {
  grid.size = 1000
  marginals = get_marginals(dat)
  x.interest = dat[rep(i, times = grid.size), ]
  x.interest[[feature]] = seq(from = min(dat[[feature]]),
			      to = max(dat[[feature]]),
			      length.out = grid.size)
  p = predict(rf, data = x.interest)$prediction
  marginal_grid =  marginals[[feature]](x.interest[[feature]])
  approxfun(x.interest[[feature]], marginal_grid * p / (1 - p))
}




i = 32
j = "X1" 
dat[i, ]
px = get_px(rf, dat, j, i)
marginals = get_marginals(dat)


# compute conditional distribution for x.interest
x.interest = dat[i, ]
marginals = get_marginals(dat)
x.seq = seq(from = min(dat[[j]]), to = max(dat[[j]]), length.out = 100)

plot(x.seq, px(x.seq))
plot(x.seq, marginals[[j]](x.seq))
# compare with true conditional distribution
library(condMVNorm)
params = condMVN(mean = mu, sigma = sigm, dependent.ind = 1,
	given.ind = c(2,3), X.given = unlist(dat[i, c(2,3), drop = TRUE]))
params
plot(x.seq, dnorm(x.seq, mean = params$condMean, sd = params$condVar))
abline(v = params$condMean)

plot.df = data.frame(
  x = rep(x.seq, times = 4),
  type = rep(c("px.rf", "marginal", "true.cond", "p.rf"), each = length(x.seq)),
  dens = c(px(x.seq),
	   marginals[[j]](x.seq),
	   dnorm(x.seq, mean = params$condMean, sd = params$condVar),
	   get_p(rf, dat, j, i, x.seq))
)

p = ggplot(plot.df) + geom_line(aes(x = x, y = dens, group = type, color = type))
print(p)
## Use this stuff for ICE
## Use this stuff for feature importance
