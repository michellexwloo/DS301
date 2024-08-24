## Problem 4
library(class)
library(caret)

setwd("~/Downloads/DS301")
source("~/Downloads/DS301/mnist_load_script.R")
load_mnist()

# (a)
index.train = sample(1:dim(train$x)[1], 3000, replace=FALSE)
index.test = sample(1:dim(test$x)[1], 100, replace=FALSE)

trainX = train$x[index.train,]
testX = test$x[index.test,]
trainY = train$y[index.train]
testY = test$y[index.test]

flds = createFolds(trainY, k=10, list = TRUE, returnTrain = FALSE)
K = c(1,5,7,9)
cv_error = matrix(NA, 10, length(K))

for (j in 1:length(K)) {
  k = K[j]
  for (i in 1:10) {
    test_index = flds[[i]]
    testX_fold = trainX[test_index,]
    trainX_fold = trainX[-test_index,]
    
    trainY_fold = trainY[-test_index]
    testY_fold = trainY[test_index]
    
    knn.pred = knn(trainX_fold, testX_fold, trainY_fold, k=k)
    cv_error[i,j] = mean(testY_fold != knn.pred)
  }
}

apply(cv_error,2,mean)
optimal_k = K[which.min(apply(cv_error,2,mean))]
cat("Optimal K:", optimal_k, "\n")
knn.pred = knn(trainX, testX, trainY, k=optimal_k)
table(testY, knn.pred)
mean(testY != knn.pred)



## Problem 5

# (a)
par(mfrow=c(1,5))
for (i in 1:5) {
  show_digit(train.fash$x[i,], axes=F)
  title(paste("Label:", train.fash$y[i]))
}

# (b)
index.train = sample(1:dim(train.fash$x)[1], 3000, replace=FALSE)
index.test = sample(1:dim(test.fash$x)[1], 100, replace=FALSE)

trainX = train.fash$x[index.train,]
testX = test.fash$x[index.test,]
trainY = train.fash$y[index.train]
testY = test.fash$y[index.test]

flds = createFolds(trainY, k=10, list = TRUE, returnTrain = FALSE)
K = c(1,5,7,9)
cv_error = matrix(NA, 10, length(K))

for (j in 1:length(K)) {
  k = K[j]
  for (i in 1:10) {
    test_index = flds[[i]]
    testX_fold = trainX[test_index,]
    trainX_fold = trainX[-test_index,]
    
    trainY_fold = trainY[-test_index]
    testY_fold = trainY[test_index]
    
    knn.pred = knn(trainX_fold, testX_fold, trainY_fold, k=k)
    cv_error[i,j] = mean(testY_fold != knn.pred)
  }
}

apply(cv_error,2,mean)
optimal_k = K[which.min(apply(cv_error,2,mean))]
cat("Optimal K:", optimal_k, "\n")
knn.pred = knn(trainX, testX, trainY, k=optimal_k)
table(testY, knn.pred)
mean(testY != knn.pred)