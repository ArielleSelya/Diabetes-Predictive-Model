########################################################################
# Linear Support Vector Machine (SVM) Classification 
# Author: Arielle S. Selya
# Date posted: 2/28/2021
########################################################################
load("Managed Sanford Data.RData")
require(e1071)

# Set parameters to loop over
params <- c(0.1, 0.5, 1, 5, 10, 25, 50)

# Set variables to record outer-loop scores and best parms
outer.scores.train <- NULL
outer.scores.test <- NULL
outer.bestparams <- NULL

# Create empty tables/arrays to accumulate results across CV iterations 
outer.train.table.all <- array(0, dim=c(2,2))
outer.test.table.all <- array(0, dim=c(2,2))
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

# Loop over CV iterations
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
      
      # Train model
      svm.result <- svm(Unplanned.Visits.bin ~ ., data=Sanford.data.diabetes.outer[-inner.ind[[i]],], type = "C-classification",
                        kernel="linear", cost=params[p])
      # Get training and testing results
      train.result <- predict(svm.result)
      test.result <- predict(svm.result, newdata=Sanford.data.diabetes.outer[inner.ind[[i]],])
      train.table <- prop.table(table(Sanford.data.diabetes.outer$Unplanned.Visits.bin[-inner.ind[[i]]], train.result),1)
      test.table <- prop.table(table(Sanford.data.diabetes.outer$Unplanned.Visits.bin[inner.ind[[i]]], test.result),1)
      
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
      
      # Increment CV scores (i.e. sensitivity + specificity)
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
  svm.result <- svm(Unplanned.Visits.bin ~ ., data=Sanford.data.diabetes[-outer.ind[[o]],], type = "C-classification",
                    kernel="linear", cost=best.params)
  train.result <- predict(svm.result)
  test.result <- predict(svm.result, newdata=Sanford.data.diabetes[outer.ind[[o]],])
  
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
mean(outer.scores.test)
sd(outer.scores.test)
apply(outer.bestparams,2,"mean")

# Create averaged confusion matrix across all CV iterations
train.table.all <- train.table.all/i
test.table.all <- test.table.all/i

SVM.L.train <- train.table.all
SVM.L.test <- test.table.all

SVM.L.train.CR <- train.cell00  # CR = correct rejections
SVM.L.train.FA <- train.cell01  # FA = false alarms
SVM.L.train.M <- train.cell10   # M = misses
SVM.L.train.H <- train.cell11   # H = hits

SVM.L.test.CR <- test.cell00
SVM.L.test.FA <- test.cell01
SVM.L.test.M <- test.cell10
SVM.L.test.H <- test.cell11

# Get means and sd's of each cell
#  Training set
summary(SVM.L.train.CR)
sd(SVM.L.train.CR)
summary(SVM.L.train.FA)
sd(SVM.L.train.FA)
summary(SVM.L.train.M)
sd(SVM.L.train.M)
summary(SVM.L.train.H)
sd(SVM.L.train.H)

#  Test set
summary(SVM.L.test.CR)
sd(SVM.L.test.CR)
summary(SVM.L.test.FA)
sd(SVM.L.test.FA)
summary(SVM.L.test.M)
sd(SVM.L.test.M)
summary(SVM.L.test.H)
sd(SVM.L.test.H)

