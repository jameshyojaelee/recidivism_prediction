library(tree)

rd <- read.csv("recidivism_data_sample.csv")

head(rd)
summary(rd)
#drop ID because it's meaningless
rd <- rd[,-1]

# Regression Tree ---------------------------------------------------------

# Data split --------------------------------------------------------------
set.seed(123)
n <- nrow(rd)
v <- sample(n,4000,replace = FALSE)
train.dat <- rd[v,]
test.dat <- rd[-v,]

rtree1 <- tree(recidivate ~ ., data = train.dat)
summary(rtree1)
plot(rtree1)
text(rtree1,pretty = 33)
rtree1

rtree2 <- tree(recidivate ~ ., data = train.dat, mindev = 0.002)
summary(rtree2)
plot(rtree2)
rtree2

rtree3 <- tree(recidivate ~ ., data = train.dat, mindev = 0.003)
summary(rtree3)
plot(rtree3)
text(rtree3)

rtest1 <- predict(rtree1, newdata = test.dat, type = "vector")
rtest2 <- predict(rtree2, newdata = test.dat, type = "vector")
rtest3 <- predict(rtree3, newdata = test.dat, type = "vector")

mean((test.dat$recidivate - rtest1)^2)
mean((test.dat$recidivate - rtest2)^2)
mean((test.dat$recidivate - rtest3)^2)

# Classification trees ----------------------------------------------------

train.dat.class <- train.dat
test.dat.class <- test.dat

train.dat.class$recidivate <- 
  as.factor(train.dat.class$recidivate)
test.dat.class$recidivate <- 
  as.factor(test.dat.class$recidivate)

head(train.dat.class)

ctree1 <- tree(recidivate ~ ., data = train.dat.class)
summary(ctree1)
plot(ctree1)
text(ctree1,pretty = 0)

ctree2 <- tree(recidivate ~ ., data = train.dat.class, mindev = 0.002)
summary(ctree2)
plot(ctree2)

ctree3 <- tree(recidivate ~ ., data = train.dat.class, mindev = 0)
summary(ctree3)
plot(ctree3)

ctest1.predprobs <- predict(ctree1, newdata = test.dat.class, type = "vector")
head(ctest1.predprobs)

ctest1 <- predict(ctree1, newdata = test.dat.class, type = "class")
ctest2 <- predict(ctree2, newdata = test.dat.class, type = "class")
ctest3 <- predict(ctree3, newdata = test.dat.class, type = "class")

mean(test.dat.class$recidivate != ctest1)
mean(test.dat.class$recidivate != ctest2)
mean(test.dat.class$recidivate != ctest3)




