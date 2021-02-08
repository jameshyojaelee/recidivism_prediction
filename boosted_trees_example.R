rm(list = ls())
library(gbm)


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
  as.numeric(train.dat.class$NAsySeekAccepted > 5)
names(train.dat.class)[1] <- "MajorityAccepted"

test.dat.class <- test.dat
test.dat.class$NAsySeekAccepted <- 
  as.numeric(test.dat.class$NAsySeekAccepted > 5)
names(test.dat.class)[1] <- "MajorityAccepted"


# Boosted Trees Model -----------------------------------------------------

#Specify distribution = "gaussian" for regression
set.seed(207)
btmod <- gbm(NAsySeekAccepted ~  ., data = train.dat, shrinkage = 0.01,
             distribution = "gaussian", n.trees = 2000, 
             interaction.depth = 1)
summary(btmod)
btmod$interaction.depth
btmod$train.error
#This is not a valid proxy for test error!

predict(btmod, newdata = test.dat)
#What should we set for n.trees?



#We have no way of knowing since we haven't done CV!

#But as an example:
predict(btmod, newdata = test.dat, n.trees = 1500)

#Note: it is important to play around with n.trees and shrinkage values
#to make sure (very roughly speaking) you have more than enough trees
#(i.e. you get to the point of actually overfitting),
#So that you can select the optimal number later via CV


#Classification version
#Note: outcome MUST BE A (0,1) VARIABLE for classification to be performed
btmod.class <- gbm(MajorityAccepted ~  ., data = train.dat.class, shrinkage = 0.01,
                   distribution = "bernoulli", n.trees = 2000, 
                   interaction.depth = 1)
btmod.class$train.error #Again, this is not a valid proxy for test error!
predict(btmod.class, newdata = test.dat.class)
predict(btmod.class, newdata = test.dat.class, n.trees = 1500)
predict(btmod.class, newdata = test.dat.class, n.trees = 1500, type = "response")
#Predicted probabilities are output when type = "response"


# Boosted Trees CV --------------------------------------------------------

#Cross-validating over depths and (implicitly) number of trees
#We will add the cv.folds option to the gbm function to implement CV,
#which will ensure there is a CV error estimate 
#for every iteration (i.e. every new tree added to model)

#Implementing CV over two parameters: depths and number of trees
depths <- c(1,2,3,4,5,6,7)
bt.models <- list()
set.seed(2019207)
for (i in 1:length(depths)){
  
  bt.models[[i]] <- gbm(NAsySeekAccepted ~  ., 
                               data = train.dat, shrinkage = 0.01,
                               distribution="gaussian", n.trees = 2000, 
                               interaction.depth = depths[i], 
                               cv.folds = 10)
  print(i)
  
}
#save(bt.models, file = "bt_cv_models.Rdata")


# Inspect results ---------------------------------------------------------

#load("bt_cv_models.Rdata")

#Let's look at one of the model sequences
bt.one <- bt.models[[5]]
bt.one$n.trees
bt.one$interaction.depth
bt.one$cv.error
plot(x = 1:bt.one$n.trees, y = bt.one$cv.error)

#Now, we want to consider all of the models: 
#All values of interaction depth AND all number of trees,
#To find the best combination based on these CV results!

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

#Test set MSE:
mean((test.dat.preds - test.dat$NAsySeekAccepted)^2)

