# random forest 
#    application of bagging to CART but with an additional layer
library(randomForest)

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

#convert into factor
train.dat.class <- train.dat
train.dat.class$recidivate <- as.factor(train.dat.class$recidivate)
test.dat.class <- test.dat
test.dat.class$recidivate <- as.factor(test.dat.class$recidivate)

# Random Forest "CV"
n.vars <- c(1,2,3,4,5,6,7,8)
rf.models <- list()

#CV to choose m (number of variables to consider at each split)
set.seed(3956)
for (i in 1:length(n.vars)){
  
  rf.models[[i]] <- randomForest(recidivate ~ .,
                                 train.dat.class, ntree = 1000,
                                 mtry = n.vars[i])
  print(i)
}
save(rf.models, file = "rf_cv_models.Rdata")

load("rf_cv_models.Rdata")

n.mods <- length(rf.models)
oob.err <- rep(NA,n.mods)
n.vars <- rep(NA,n.mods)

#must use err.rate instead of mse since classification variables are factors
for (i in 1:n.mods){
  oob.err[i] <- min(rf.models[[i]]$err.rate)
  n.vars[i] <- rf.models[[i]]$mtry
}
best.mod <- which.min(oob.err)
n.vars[best.mod]


# Prediction
predprob <- predict(rf.models[[best.mod]], newdata = test.dat.class, type = "prob")
test.dat.rfpreds <- data.frame(predprob[,2])

#Test set MSE:
mean(((predprob[,2]) - test.dat$recidivate)^2)
# 0.228813

test.dat.rfpreds.class <- as.numeric(test.dat.rfpreds > 0.5)


# accuracy
acc <- mean(test.dat.rfpreds.class == test.dat$recidivate)
# 0.644

# precision
prec <- mean(test.dat$recidivate[test.dat.rfpreds.class == 1] == 1)
# 0.6783505

# recall
recall <- mean(test.dat.rfpreds.class[test.dat$recidivate == 1] == 1)
# 0.3717514
