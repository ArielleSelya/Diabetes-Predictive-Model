########################################################################
# Single-layer Neural Net (NN) Classification 
# Author: Arielle S. Selya
# Date posted: 2/28/2021
########################################################################
load("Managed Sanford Data.RData")
require(nnet)
# Set parameters to loop over
params <- list(size = c(1, 2, 5, 10, 15, 20), maxit=c(100,150,200), 
               decay=c(0, 0.1, 0.3, 0.5, 0.9))
params <- do.call(expand.grid, params)

# Set outer-loop scores and best parms
outer.train.table.all <- array(0, dim=c(2,2))
outer.test.table.all <- array(0, dim=c(2,2))
outer.scores.train <- NULL
outer.scores.test <- NULL
outer.bestparams <- NULL

train.cell00 <- NULL
train.cell01 <- NULL
train.cell10 <- NULL
train.cell11 <- NULL
test.cell00 <- NULL
test.cell01 <- NULL
test.cell10 <- NULL
test.cell11 <- NULL
cv.scores <- NULL

# Split into 5 outer folds
outer.ind <- sample(1:dim(Sanford.data.diabetes)[1])
n.outer.folds <- length(outer.ind)/5
outer.ind <- split(outer.ind, ceiling(seq_along(outer.ind)/n.outer.folds))

for(o in 1:5){ # Outer loop CV
  
  # Create empty arrays for best parameters
  best.parms <- NULL
  best.score <- -100
  
  # Create empty tables for accumulation
  train.table.all <- array(0, dim=c(2,2))
  test.table.all <- array(0, dim=c(2,2))
  
  # Define train/test samples for outer loop
  Sanford.data.diabetes.outer <- Sanford.data.diabetes[-outer.ind[[o]],]
  
  for(p in 1:nrow(params)){ # Parameter loop
    
    # Split into 5 inner folds
    inner.ind <- sample(1:dim(Sanford.data.diabetes.outer)[1])
    n.inner.folds <- length(inner.ind)/5
    inner.ind <- split(inner.ind, ceiling(seq_along(inner.ind)/n.inner.folds))
    
    for(i in 1:5){ # Inner loop
      
      nn.result <- nnet(Unplanned.Visits.bin ~ ., data=Sanford.data.diabetes.outer[-inner.ind[[i]],], size=params$decay[p],
                        maxit = params$maxit[p], abstol=.01, skip=T, decay=params$decay[p])
      # Get predicted values
      train.result <- predict(nn.result, type="class")
      test.result <- predict(nn.result, newdata=Sanford.data.diabetes.outer[inner.ind[[i]],], type="class")
      # Prevent single-column tables
      train.result <- as.factor(train.result)
      test.result <- as.factor(test.result)
      if(length(levels(train.result))==1){
        if(levels(train.result)=="1") levels(train.result) <- c("1","0")
        else if(levels(train.result)=="0") levels(train.result) <- c("0","1")
      }
      if(length(levels(test.result))==1){
        if(levels(test.result)=="1") levels(test.result) <- c("1","0")
        else if(levels(test.result)=="0") levels(test.result) <- c("0","1")
      }
      train.result <- factor(train.result, levels=c("0","1"))
      test.result <- factor(test.result, levels=c("0","1"))
      
      # Get tables of real vs. predicted classes
      train.table <- prop.table(table(Sanford.data.diabetes.outer$Unplanned.Visits.bin[-inner.ind[[i]]], train.result),1)
      test.table <- prop.table(table(Sanford.data.diabetes.outer$Unplanned.Visits.bin[inner.ind[[i]]], test.result),1)      
      # Increment tables
      for(j in 1:dim(train.table)[1])
        for(k in 1:dim(train.table)[2])
          train.table.all[j,k] <- train.table.all[j,k] + train.table[j,k]
      for(j in 1:dim(test.table)[1])
        for(k in 1:dim(test.table)[2])
          test.table.all[j,k] <- test.table.all[j,k] + test.table[j,k]
      
      # Increment CV scores (i.e. balanced accuracy)
      cv.scores <- c(cv.scores, (test.table[1,1] + test.table[2,2])/2)
    }
    # Compute mean CV scores over inner folds
    mean.score <- mean(cv.scores)
    if(mean.score > best.score){
      best.score <- mean.score
      best.params <- params[p,]
    }
  }
  
  # Train and test classifier using best parameters from inner loop, on outer data set
  nn.result <- nnet(Unplanned.Visits.bin ~ ., data=Sanford.data.diabetes[-outer.ind[[o]],], size=best.params$decay,
                    maxit = best.params$maxit, abstol=.01, skip=T, decay=best.params$decay)
  # Get predicted values
  train.result <- predict(nn.result, type="class")
  test.result <- predict(nn.result, newdata=Sanford.data.diabetes[outer.ind[[o]],], type="class")
  # Prevent single-column tables
  train.result <- as.factor(train.result)
  test.result <- as.factor(test.result)
  if(length(levels(train.result))==1){
    if(levels(train.result)=="1") levels(train.result) <- c("1","0")
    else if(levels(train.result)=="0") levels(train.result) <- c("0","1")
  }
  if(length(levels(test.result))==1){
    if(levels(test.result)=="1") levels(test.result) <- c("1","0")
    else if(levels(test.result)=="0") levels(test.result) <- c("0","1")
  }
  train.result <- factor(train.result, levels=c("0","1"))
  test.result <- factor(test.result, levels=c("0","1"))
  
  # Get tables of real vs. predicted classes
  train.table <- prop.table(table(Sanford.data.diabetes$Unplanned.Visits.bin[-outer.ind[[o]]], train.result),1)
  test.table <- prop.table(table(Sanford.data.diabetes$Unplanned.Visits.bin[outer.ind[[o]]], test.result),1)      
  
  # Increment tables
  for(j in 1:dim(train.table)[1])
    for(k in 1:dim(train.table)[2])
      outer.train.table.all[j,k] <- outer.train.table.all[j,k] + train.table[j,k]
  for(j in 1:dim(test.table)[1])
    for(k in 1:dim(test.table)[2])
      outer.test.table.all[j,k] <- outer.test.table.all[j,k] + test.table[j,k]
  print(prop.table(outer.train.table.all, 1))
  print(prop.table(outer.test.table.all, 1))
  
  # Increment outer scores
  outer.scores.train <- c(outer.scores.train, (train.table[1,1] + train.table[2,2])/2)
  outer.scores.test <- c(outer.scores.test, (test.table[1,1] + test.table[2,2])/2)
  outer.bestparams <- rbind(outer.bestparams, best.params)
  
  # Increment arrays for variance
  train.cell00 <- c(train.cell00, train.table[1,1])
  train.cell01 <- c(train.cell01, train.table[1,2])
  train.cell10 <- c(train.cell10, train.table[2,1])
  train.cell11 <- c(train.cell11, train.table[2,2])
  test.cell00 <- c(test.cell00, test.table[1,1])
  test.cell01 <- c(test.cell01, test.table[1,2])
  test.cell10 <- c(test.cell10, test.table[2,1])
  test.cell11 <- c(test.cell11, test.table[2,2])
  
  print(o)
}

# Examine outer-loop CV scores and best parameters
mean(outer.scores.train)
sd(outer.scores.train)
mean(outer.scores.test)
sd(outer.scores.test)
outer.bestparams

# Create averaged confusion matrix across all CV iterations
train.table.all <- train.table.all/i
test.table.all <- test.table.all/i

NN.train <- train.table.all
NN.test <- test.table.all

NN.train.CR <- train.cell00  # CR = correct rejections
NN.train.FA <- train.cell01  # FA = false alarms
NN.train.M <- train.cell10   # M = misses
NN.train.H <- train.cell11   # H = hits

NN.test.CR <- test.cell00
NN.test.FA <- test.cell01
NN.test.M <- test.cell10
NN.test.H <- test.cell11

# Get means and sd's of each cell
#  Training set (Table 4)
summary(NN.train.CR)
sd(NN.train.CR)
summary(NN.train.FA)
sd(NN.train.FA)
summary(NN.train.M)
sd(NN.train.M)
summary(NN.train.H)
sd(NN.train.H)

#  Test set (Table 2)
summary(NN.test.CR)
sd(NN.test.CR)
summary(NN.test.FA)
sd(NN.test.FA)
summary(NN.test.M)
sd(NN.test.M)
summary(NN.test.H)
sd(NN.test.H)
