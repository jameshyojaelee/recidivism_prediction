# random forest -----------------------------------------------------------
#    application of bagging to CART but with an additional layer
library(randomForest)

rd <- read.csv("recidivism_data_sample.csv")

head(rd)
summary(rd)

# Data split --------------------------------------------------------------
set.seed(1234)
n <- nrow(rd)
v <- sample(n,4000,replace = FALSE)
train.dat <- rd[v,]
test.dat <- rd[-v,]


