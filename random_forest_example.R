rm(list = ls())
library(randomForest)


# Load data ---------------------------------------------------------------

wdf <- read.csv("asylum_conjoint_data.csv")
head(wdf)


# Separate into training and test sets ------------------------------------

set.seed(123)
test.units <- sample(rownames(wdf),1000,replace = FALSE)

test.dat <- wdf[rownames(wdf) %in% test.units,]
train.dat <- wdf[!(rownames(wdf) %in% test.units),]


# Creating classification versions of data --------------------------------

train.dat.class <- train.dat
train.dat.class$NAsySeekAccepted <- 
  as.factor(train.dat.class$NAsySeekAccepted > 5)
names(train.dat.class)[1] <- "MajorityAccepted"

test.dat.class <- test.dat
test.dat.class$NAsySeekAccepted <- 
  as.factor(test.dat.class$NAsySeekAccepted > 5)
names(test.dat.class)[1] <- "MajorityAccepted"


# Random Forest Model -----------------------------------------------------

#Regression model
set.seed(207)
rf.mod <- randomForest(NAsySeekAccepted ~ ., train.dat, ntree = 1000)
predict(rf.mod, newdata = test.dat)

summary(rf.mod)
rf.mod$mtry #This is the number of variables randomly considered per tree
rf.mod$mse #This is the out-of-bag MSE at each stage of the model
plot(x = 1:1000, y = rf.mod$mse)


#Classification model
#Note: outcome MUST BE A FACTOR VARIABLE for classification to be performed
str(train.dat.class)
set.seed(207)
rf.mod.class <- randomForest(MajorityAccepted ~ ., train.dat.class, 
                             ntree = 1000)
predict(rf.mod.class, newdata = test.dat.class, type = "response")
predict(rf.mod.class, newdata = test.dat.class, type = "prob")[1:10,]

rf.mod.class$mtry
rf.mod.class$err.rate[1:10,] #OOB is the overall error rate
plot(x = 1:1000, y = rf.mod.class$err.rate[,1])


# Random Forest "CV" ------------------------------------------------------

#"Cross-validating" over number of randomly sampled variables (m)
#Random Forests implicitly OOB error estimation (bagging version of CV)
#i.e. observations not randomly sampled in each bag (bootstrap iteration)
#can be used as validation data for that iteration

n.vars <- c(1,2,3,4,5,6,7)
rf.models <- list()

#CV to choose m (number of variables to consider at each split)
set.seed(207)
for (i in 1:length(n.vars)){
  
  rf.models[[i]] <- randomForest(NAsySeekAccepted ~ .,
                           train.dat, ntree = 1000,
                           mtry = n.vars[i])
  print(i)
  
}
#save(rf.models, file = "rf_cv_models.Rdata")


# Inspect results ---------------------------------------------------------

#load("rf_cv_models.Rdata")

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
mean((test.dat.rfpreds - test.dat$NAsySeekAccepted)^2)

