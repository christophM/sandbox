library(iml)
library(mlr)
library(sparsepca)
library(mxnet)
library(keras)
library(ggplot2)

wine = read.csv("~/repos/paper_2018_autoiml/data/winequalityN.csv")
wine$type = NULL
wine = na.omit(wine)

tsk = makeRegrTask(data = wine, target = "quality")
lrn = makeLearner("regr.ranger")
mod = train(lrn, tsk)

pred = Predictor$new(mod, data = wine)


X = wine
X$quality = NULL

facs2 = spca(X, scale = TRUE, alpha = 0.01, k = 6)
xx = facs2$loadings
rownames(xx) = colnames(X)
xx


loads = as.matrix(X) %*% xx

dim(loads)

summary(loads)


predictions = pred$predict(wine)

plot(loads[,1], predictions[,1], xlim  = c(-300, 0))

plot(wine$free.sulfur.dioxide, predictions[,1])

cor(loads, predictions[,1])



## Autoencoder approach

model <- keras_model_sequential()
model = model %>%
  layer_dense(units = 10, activation = "relu", input_shape = ncol(X)) %>%
  layer_dense(units = 5, activation = "relu") %>%
  layer_dense(units = 10, activation = "relu") %>%
  layer_dense(units = ncol(X))

summary(model)

model = model %>% 
  compile(loss = "mean_squared_error", optimizer = "adam")

checkpoint <- callback_model_checkpoint(
  filepath = "model.hdf5", 
  save_best_only = TRUE, 
  period = 1,
  verbose = 1
)

early_stopping <- callback_early_stopping(patience = 5)

xm = as.matrix(X)
train.i = 1:4000
test.i = setdiff(1:nrow(X), train.i)
Xtrain = xm[train.i,]
Xtest = xm[test.i,]

model %>% fit(
  x = Xtrain, 
  y = Xtrain, 
  epochs = 200, 
  batch_size = 32,
  validation_data = list(Xtest, Xtest), 
  callbacks = list(checkpoint, early_stopping)
)


pred_train = predict(model, Xtrain)
summary(pred_train)

## Tryout total.sulfur.dioxide based estimation trees


library(detpack)
forest = read.csv("~/Downloads/covtype.data")

size = 1000

forest2 = forest[sample(1:nrow(forest), size = size), ]

#dtf = as.matrix(X)
dtf = as.matrix(forest2)

dtf = t(dtf)

a = 0.0000000000001

dtree  = det.construct(dtf, dtalim = 100, alphag = a,
		       alphad = 0.00000001,
		       cores = 2)
summary(dtree)

qq = det.query(dtree, dtf[, , drop = FALSE])

plot.dat = expand.grid(
  free.sulfur.dioxide = seq(from = min(wine$free.sulfur.dioxide), to = max(wine$free.sulfur.dioxide), length.out = 100),
  total.sulfur.dioxide = seq(from = min(wine$total.sulfur.dioxide), to = max(wine$total.sulfur.dioxide), length.out = 100)
)




plot_dens = function(i){
x = X[rep(i, times = 100 * 100),, drop = FALSE]
x$free.sulfur.dioxide = plot.dat$free.sulfur.dioxide
x$total.sulfur.dioxide= plot.dat$total.sulfur.dioxide
x$qq =  det.query(dtree, t(as.matrix(x)))
x$qq = x$qq / sum(x$qq)

p = ggplot(x, aes(x = free.sulfur.dioxide, y = total.sulfur.dioxide)) + 
  geom_tile(aes(fill = qq)) + 
  geom_point(data = X[i,], size = 3, color = "red") + 
  geom_point(data = wine, alpha = 0.1)
print(p)
}

lapply(1:100, function(i) {
  plot_dens(i)
  Sys.sleep(3)
})




plot(wine$free.sulfur.dioxide, wine$total.sulfur.dioxide)

