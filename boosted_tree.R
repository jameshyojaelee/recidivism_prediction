# boosted tree
rm(list = ls())
library(gbm)

rd <- read.csv("recidivism_data_sample.csv")
head(rd)
#drop ID because it's meaningless
rd <- rd[,-1]

# Data split --------------------------------------------------------------
set.seed(1234)
n <- nrow(rd)
v <- sample(n,4000,replace = FALSE)

train.dat <- rd[v,]
test.dat <- rd[-v,]


# Boosted Trees Model -----------------------------------------------------

btmod.class <- gbm(recidivate ~  ., data = train.dat, shrinkage = 0.01,
                   distribution = "bernoulli", n.trees = 2000, 
                   interaction.depth = 1)
btmod.class$train.error #Again, this is not a valid proxy for test error!
predict(btmod.class, newdata = test.dat)
predict(btmod.class, newdata = test.dat, n.trees = 1500)
predict(btmod.class, newdata = test.dat, n.trees = 1500, type = "response")


# Boosted Trees CV --------------------------------------------------------

#Implementing CV over two parameters: depths and number of trees
depths <- c(1,2,3,4,5,6,7,8)
bt.models <- list()
set.seed(12345)
for (i in 1:length(depths)){
  
  bt.models[[i]] <- gbm(recidivate ~  ., 
                        data = train.dat, shrinkage = 0.01,
                        distribution="bernoulli", n.trees = 2000, 
                        interaction.depth = depths[i], 
                        cv.folds = 10)
  print(i)
}

save(bt.models, file = "bt_cv_models.Rdata")


# Inspect results ---------------------------------------------------------

load("bt_cv_models.Rdata")

bt.one <- bt.models[[5]]
bt.one$n.trees
bt.one$interaction.depth
bt.one$cv.error
plot(x = 1:bt.one$n.trees, y = bt.one$cv.error)

n.depths <- length(bt.models)
depths <- rep(NA, n.depths)
min.cv.error <- rep(NA, n.depths)
best.n.trees <- rep(NA, n.depths)

for (i in 1:n.depths){
  
  bt.curr <- bt.models[[i]]
  depths[i] <- bt.curr$interaction.depth
  min.cv.error[i] <- min(bt.curr$cv.error)
  best.n.trees[i] <- which.min(bt.curr$cv.error)
  rm(bt.curr)
}

min.cv.error
m <- which.min(min.cv.error)

final.ntrees <- best.n.trees[m]
final.ntrees
final.depth <- depths[m]
final.depth


# Use final model ---------------------------------------------------

test.dat.preds <- predict(bt.models[[m]], newdata = test.dat, 
                          n.trees = final.ntrees)
test.dat.preds.class <- test.dat.preds
test.dat.preds.class[test.dat.preds.class >= 0.5] <- 1
test.dat.preds.class[test.dat.preds.class < 0.5] <- 1

#Test set MSE:
mean((test.dat.preds.class - test.dat$recidivate)^2)


# 0.2041584