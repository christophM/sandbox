library("ggplot2")
library("ranger")
library("solitude")
# Simulate data
set.seed(42)

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

shuffle2 = function(dat){
  data.frame(lapply(dat, function(x) {
  if (inherits(x, "factor")){
    lvls = unique(x)
    factor(sample(lvls, replace = TRUE, size = length(x)), levels = levels(x))
    #sample(x)
  } else {
    minx = min(x)
    maxx = max(x)
    runif(length(x), min = minx, max = maxx)
    #sample(x)
  }
  }))
}

generate2 = function(n, y = TRUE){
  dat = generate(n)
  dat2 = shuffle(dat)
  tdat = rbind(dat, dat2)
  if (y) tdat$y =  rep(c(1,0), each = nrow(dat))
  tdat
}

# Train RF
train.dat = generate2(300)
library("ranger")
mod = ranger(y ~ ., data = train.dat)


# Check performance
test.dat = generate2(1000)
preds = predict(mod, data = test.dat)
test.dat$pred = preds$predictions

	facet_grid(y ~ x2)



# Try out isolation forest

# TODO: Continue here

dat = generate2(1000, y = FALSE)

tdat = dat[1:1000,]
sol = isolation_forest(tdat, num.tree = 1000)

dat$pred = predict(sol, data = dat)
dat$y = rep(c(1,0), each = 1000)
ggplot(dat)  + geom_boxplot(aes(x = y, y = pred, group = y))




# Calculate P_X from random forest probability p = P_X / (P_X + P_{X,marginal})
# Therefore P_X = p/(1-p) * P_{X,marginal}

# marginal distribution is product of all marginal feature distributions
# kernel density for each feature


# use ecdf, because it is more stable


get_marginal = function(dat) {
  # Density estimator for each feature
  density_funs = lapply(dat, function(x) {
    if (inherits(x, "factor")) {
      freqs = table(x) / length(x)
      function(z) freqs[z]
    } else {
      approxfun(density(x))
    }
  })
  function(dats) {
    p = rep(1, times = nrow(dats))
    for (j in 1:ncol(dats)){
      p = p * density_funs[[j]](dats[[j]])
    }
    as.vector(p)
}
}
get_marginal_ecdf = function(dat) {
  # Density estimator for each feature
  density_funs = lapply(dat, ecdf)
  function(dats) {
    p = rep(1, times = nrow(dats))
    for (j in 1:ncol(dats)){
      p = p * density_funs[[j]](dats[[j]])
    }
    as.vector(p)
}
}



dat = generate2(1000, y = FALSE)
tdat = dat[1:1000,]

marginal =  get_marginal(tdat)
marginal_ecdf = get_marginal_ecdf(tdat)
ecdf.marginal = marginal_ecdf(tdat)
p.marginal = marginal(dat)

p = predict(mod, data = dat)$prediction

px = p.marginal * p / (1 - p)
p_ecdf =  ecdf.marginal * p / (1 - p)

summary(px)

dat$px = px


ggplot(test.dat) + geom_point(aes(x = x1, y = x3, color = log(px))) + 
	facet_grid(y ~ x2)



# Test for one feature
grid.size = 100
x.interest = dat[rep(1, times = grid.size),]
x.interest$x3 = seq(from = min(dat$x3), to = max(dat$x4), length.out = grid.size)

x3_marginal = approxfun(density(tdat$x3))

p = predict(mod, data = x.interest)$prediction
x3m = x3_marginal(x.interest$x3)

x3m_scaled = x3m/sum(x3m)

x3m * p / (1-p)

x3m_scaled * p / (1 - p)


x3m_scaled * p / (1 - p)
p


plot(p)
cbind(x.interest, p, x3m)

plot(x.interest$x3, x3m)
plot(x.interest$x3, p)
plot(x.interest$x3, x3m * p/(1-p))


