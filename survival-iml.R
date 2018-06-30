library(randomForestSRC)
?randomForestSRC


## use the pbc data from the survival package
## events are transplant (1) and death (2)
data(pbc, package = "survival")
pbc$id <- NULL

## modified Gray's weighted log-rank splitting
pbc.cr <- rfsrc(Surv(time, status) ~ ., pbc, nsplit = 10)

## log-rank event-one specific splitting
pbc.log1 <- rfsrc(Surv(time, status) ~ ., pbc, nsplit = 10,
  splitrule = "logrank", cause = c(1,0), importance="permute")


## Problem: rfsrc silently drops NA predictions
pred = Predictor$new(pbc.cr, data = pbc, type = "risk", predict.fun = function(model, newdata) {predict(model, newdata = newdata)$predicted})
pdp = Partial$new(pred, feature = "trig")
pdp$plot()


## log-rank event-two specific splitting
pbc.log2 <- rfsrc(Surv(time, status) ~ ., pbc, nsplit = 10,
  splitrule = "logrank", cause = c(0,1), importance="permute")

## extract VIMP from the log-rank forests: event-specific
## extract minimal depth from the Gray log-rank forest: non-event specific
var.perf <- data.frame(md = max.subtree(pbc.cr)$order[, 1],
  vimp1 = 100 * pbc.log1$importance[ ,1],
  vimp2 = 100 * pbc.log2$importance[ ,2])
print(var.perf[order(var.perf$md), ])


xx = predict(pbc.log1)
head(xx$predicted[,1])


## Cox model
# the type of predicted value.
# Choices are the linear predictor ("lp"),
# the risk score exp(lp) ("risk"), the expected number of events given the covariates and
# follow-up time ("expected"), and the terms of the linear predictor ("terms").
# The survival probability for a subject is equal to exp(-expected).
cp = coxph(Surv(time, status) ~ ., pbc)


type = "expected"
test.dat = pbc[1,]
predict(cp, newdata = test.dat, type = type)

test.dat = pbc[1,]
test.dat$time = 100
predict(cp, newdata = test.dat, type = type)

pbc.ok = pbc[which(!is.na(predict(cp, pbc) )),]

pred = Predictor$new(cp, data = pbc.ok, type = "risk")
pdp = Partial$new(pred, feature = "albumin")
pdp$plot()



library("partykit")
data("GBSG2", package = "TH.data")
bst <- cforest(Surv(time, cens) ~ ., data = GBSG2, ntree = 50)

### estimate conditional Kaplan-Meier curves
print(predict(bst, newdata = GBSG2[1:2,], OOB = TRUE, type = "prob"))
print(predict(bst, newdata = GBSG2[1:2,], OOB = TRUE, type = "response"))
print(predict(bst, newdata = GBSG2[1:2,], OOB = TRUE, type = "weight"))


library("iml")


pred = Predictor$new(bst, data = GBSG2)
pdp = Partial$new(pred, feature = "tsize")
pdp$plot()

