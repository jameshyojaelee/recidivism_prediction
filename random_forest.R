# random forest -----------------------------------------------------------
#    application of bagging to CART but with an additional layer
library(randomForest)

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

train.dat.class <- train.dat
test.dat.class <- test.dat

# Random Forest "CV" ------------------------------------------------------
n.vars <- c(1,2,3,4,5,6,7,8)
rf.models <- list()

#CV to choose m (number of variables to consider at each split)
set.seed(3956)
for (i in 1:length(n.vars)){
  
  rf.models[[i]] <- randomForest(recidivate ~ .,
                                 train.dat, ntree = 1000,
                                 mtry = n.vars[i])
  print(i)
  
}
save(rf.models, file = "rf_cv_models.Rdata")


load("rf_cv_models.Rdata")
n.mods <- length(rf.models)
oob.mse <- rep(NA,n.mods)
n.vars <- rep(NA,n.mods)
for (i in 1:n.mods){
  oob.mse[i] <- min(rf.models[[i]]$mse)
  n.vars[i] <- rf.models[[i]]$mtry
}

best.mod <- which.min(oob.mse)
n.vars[best.mod]


# Use final model ---------------------------------------------------
test.dat.rfpreds <- predict(rf.models[[best.mod]], newdata = test.dat)

#Test set MSE:
mean((test.dat.rfpreds - test.dat$recidivate)^2)



