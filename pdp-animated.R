load(url("https://github.com/christophM/interpretable-ml-book/blob/master/data/bike.RData?raw=true"))
library("mlr")
library("iml")
library("ggplot2")
library(gganimate)
library(dplyr)
library(tidyr)

bike.task = makeRegrTask(data = bike, target = "cnt")
mod.bike = mlr::train(mlr::makeLearner(cl = 'regr.randomForest', id = 'bike-rf'), bike.task)

set.seed(42)
bike.sub = bike[sample(1:nrow(bike), size = 15),]

pred.bike = Predictor$new(mod.bike, data = bike.sub)
pdp = FeatureEffect$new(pred.bike, "temp", method = "ice", grid.size = 7)


res.raw = pdp$results
theme_set(theme_classic(base_size = 20))


## Step 1: Define grid over feature
grid.dat = data.frame(temp = unique(res.raw$temp), .id = 1, temp2 = 1)
p = ggplot(grid.dat) + geom_vline(aes(xintercept = temp)) +
  scale_y_continuous("Prediction", limits = c(min(res.raw$.y.hat), max(res.raw$.y.hat))) +
  transition_components(.id, temp)  +
  shadow_mark() +
  ggtitle("1) Define grid along feature") +
  scale_x_continuous("Temperature") +
  ease_aes("quartic-in-out")
  #scale_y_continuous("Prediction")
p = animate(p, duration = 4)
anim_save("pdp1.gif", p)

## Step 2: Get predictions for data set when at grid
grid.dat = data.frame(temp = unique(res.raw$temp))
res.raw$tt = res.raw$temp
p = ggplot(res.raw, aes(x = temp, y = .y.hat)) +
  geom_vline(data = grid.dat, aes(xintercept = temp)) +
  geom_point(aes(color = factor(.id)), size = 3)+
  transition_time(tt) +
  shadow_mark() +
  scale_color_discrete(guide = "none") +
  ease_aes("quartic-in-out") +
  ggtitle("2) Model predictions at grid points") +
  scale_x_continuous("Temperature") +
  scale_y_continuous("Prediction")
p = animate(p, duration = 4)
anim_save("pdp2.gif", p)

## Step 3: Connect the curves -> ICE
res.raw$.id2 = factor(res.raw$.id)
p = ggplot(res.raw, aes(x = temp, y = .y.hat))  +
  #geom_vline(aes(xintercept = temp), data = grid.dat) +
  geom_line(aes(group = .id), size  = 2)  +
  geom_point(data = res.raw[c("temp", ".y.hat", ".id2")], aes(color = .id2), size = 3) +
  transition_reveal(.id, temp) +
  scale_color_discrete(guide = "none") +
  ggtitle("3) Line per data instance -> ICE curves") +
  scale_x_continuous("Temperature") +
  scale_y_continuous("Prediction")
p = animate(p, duration = 3)
anim_save("pdp3.gif", p)


## Step 4: Average lines -> PDP

res = res.raw %>% group_by(temp) %>%
  mutate(pdp = mean(.y.hat)) %>%
  ungroup() %>%
  gather(type, .y.hat, .y.hat, pdp) %>%
  mutate(type = factor(type))

p = ggplot(res) +
  geom_line(aes(x = temp, y = .y.hat, group = .id), size = 2) +
  geom_point(data = res.raw[c("temp", ".y.hat", ".id2")], aes(color = .id2, x = temp, y = .y.hat), size = 3) +
  transition_states(type, 1, 1, wrap = FALSE) +
  ggtitle("4) Average curves to get a PDP") +
  scale_color_discrete(guide = "none") +
  scale_x_continuous("Temperature") +
  scale_y_continuous("Prediction")
p = animate(p)
anim_save("pdp4.gif", p)

system("gifsicle  pdp1.gif pdp2.gif pdp3.gif pdp4.gif > pdp.gif")


