library("ggplot2")
library("ranger")
library("checkmate")
# Simulate data
# set.seed(42)

# Other approaches
# https://arxiv.org/pdf/1801.04211.pdf (but px has to be known ... )
# Maybe combine my approach with the one above.

generate = function(n) {
  x1 = rnorm(n, mean = 10, sd = 2)
  x4 = rnorm(n, mean = x1^2, sd = 3)
  x2 = rpois(n, lambda = 2)
  x3 = ifelse(x2 > 3, x1 + rnorm(n), x4 + rnorm(n, mean = 2)^2)
  x2 = as.factor(x2)
  data.frame(x1, x2, x3, x4)
}

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


## Test whether this recovers correct distribution in multivariate normal distribution

# simulate multivariate normal distribution with 3 features and some correlation
library("MASS")
n = 1000
mu = c(1,2,3)
sigm = matrix(c(1, 0.7, 0, 0.7, 1, 0, 0, 0, 1), ncol = 3)
dat = data.frame(mvrnorm(n = n, mu = mu, Sigma = sigm))

# Add some noise
noise = data.frame(matrix(rnorm(n = n * 100), ncol = 100))
colnames(noise) = sprintf("noise%i", 1:100)
dat = cbind(dat, noise)

discrim_rf = function(dat){
  dat2 = shuffle(dat)
  dat$y = 1
  dat2$y = 0
  dat = rbind(dat, dat2)
  ranger(y ~ ., data = dat, min.node.size = 100)
}
# train the random forest
rf = discrim_rf(dat)
pp = predict(rf, dat, predict.all = TRUE)


predict_rf_feat = function(rf, xj.name, dat) {
  # Get indices of trees that used xj
  x.id = which(colnames(dat) == xj.name)
  tree.ids = unlist(lapply(rf$forest$split.varIDs, function(x) x.id %in% x))
  preds = predict(rf, dat, predict.all = TRUE)$predictions 
  preds.subset = preds[, tree.ids]
  rowMeans(preds.subset)
}

pred = predict_rf_feat(rf,  "X1", dat)

get_p = function(rf, dat, feature, i, x.seq) {
  marginals = get_marginals(dat)
  x.interest = dat[rep(i, times = length(x.seq)), ]
  x.interest[[feature]] =  x.seq
  #predict(rf, data = x.interest)$prediction
  predict_rf_feat(rf, dat = x.interest, xj.name = feature)
}

get_px = function(rf, dat, feature, i) {
  grid.size = 100
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

library('dplyr')
plot.df = plot.df %>% group_by(type) %>%
	mutate(dens = dens / sum(dens)) %>%
	ungroup() %>%
	data.frame()


kl = function(p.vec, q.vec) {
  assert_numeric(p.vec, len = length(q.vec))
  assert_numeric(q.vec, len = length(p.vec))
  p.vec = p.vec / sum(p.vec)
  q.vec = q.vec / sum(q.vec)
  - sum(p.vec * log(q.vec / p.vec))
}

kl.proposed = kl(plot.df[plot.df$type == "true.cond", "dens"],
		 plot.df[plot.df$type == "px.rf", "dens"])
kl.baseline = kl(plot.df[plot.df$type == "true.cond", "dens"],
		 plot.df[plot.df$type == "marginal", "dens"])

p = ggplot(plot.df) + geom_line(aes(x = x, y = dens, group = type, color = type)) + 
	ggtitle(sprintf("KL BL: %.3f, KL RF: %.3f", kl.baseline, kl.proposed))
print(p)



## Alternative: Predict xj from rest

dd2 = dat
dd2$y = NULL
rf2 = ranger(X1 ~ ., data = dd2)
feature = "X1"
x.interest = dat[rep(i, times = length(x.seq)), ]
x.interest[[feature]] =  x.seq
print(predict(rf2, data = x.interest)$prediction)


## Use this stuff for ICE
## Use this stuff for feature importance
