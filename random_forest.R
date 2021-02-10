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

# Random Forest "CV"
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
oob.err <- rep(NA,n.mods)
n.vars <- rep(NA,n.mods)

for (i in 1:n.mods){
  oob.err[i] <- min(rf.models[[i]]$err.rate)
  n.vars[i] <- rf.models[[i]]$mtry
}

best.mod <- which.min(oob.err)
n.vars[best.mod]


# Prediction
test.dat.rfpreds <- predict(rf.models[[best.mod]], newdata = test.dat)


#Test set MSE:
mean((as.numeric(test.dat.rfpreds) - as.numeric(test.dat$recidivate))^2)
# 0.2179616

library(ggplot2)
ggplot(data=NULL, aes(x=test.dat.rfpreds, y=test.dat$recidivate)) + geom_point()


test.dat.rfpreds.class <- as.numerictest.dat.rfpreds > 0.5

# accuracy
acc <- mean(test.dat.rfpreds.class == test.dat$recidivate)
#0.669

# precision
prec <- mean(test.dat$recidivate[test.dat.rfpreds.class == 1] == 1)
# 0.6867672

# recall
recall <- mean(test.dat.rfpreds.class[test.dat$recidivate == 1] == 1)
#0.4632768
