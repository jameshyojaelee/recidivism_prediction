# boosted tree
rm(list = ls())
library(gbm)

rd <- read.csv("recidivism_data_sample.csv")
head(rd)
#drop ID because it's meaningless
rd <- rd[,-1]

#convert race into factor variables
rd$race <- as.factor(rd$race)

# Data split
set.seed(1234)
n <- nrow(rd)
v <- sample(n,4000,replace = FALSE)

train.dat <- rd[v,]
test.dat <- rd[-v,]

# Boosted Trees CV 
depths <- c(1,2,3,4,5,6,7,8)
bt.models <- list()
set.seed(1234)

for (i in 1:length(depths)){
  bt.models[[i]] <- gbm(recidivate ~  ., 
                        data = train.dat, shrinkage = 0.01,
                        distribution="bernoulli", n.trees = 2000, 
                        interaction.depth = depths[i], 
                        cv.folds = 10)
  print(i)
}
save(bt.models, file = "bt_cv_models.Rdata")
load("bt_cv_models.Rdata")

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

# final model
test.dat.preds <- predict(bt.models[[m]], newdata = test.dat, 
                          n.trees = final.ntrees)

#Test set MSE:
mean((test.dat.preds - test.dat$recidivate)^2)


# 0.2041584