########################################################################
# Double-layer Neural Net (DNN) Classification Results
# Author: Arielle S. Selya
# Date posted: 12/27/2020
########################################################################
load("Managed Sanford Data.RData")
require(deepnet)

# Create empty tables/arrays to accumulate results across CV iterations 
train.table.all <- array(0, dim=c(2,2))
test.table.all <- array(0, dim=c(2,2))
train.cell00 <- NULL
train.cell01 <- NULL
train.cell10 <- NULL
train.cell11 <- NULL
test.cell00 <- NULL
test.cell01 <- NULL
test.cell10 <- NULL
test.cell11 <- NULL

# Loop over CV iterations
for(i in 1:1000){
  # Randomly select 10% to hold out for testing
  samp <- sample(1:dim(Sanford.data.diabetes)[1], round(0.1*dim(Sanford.data.diabetes)[1]))
  # Train DNN model
  deepnet.result <- nn.train(as.matrix(Sanford.data.diabetes[-samp,-1]),
                             as.matrix(Sanford.data.diabetes[-samp,1]),
                             hidden=c(10, 10, 10), learningrate=0.8, momentum=0.5,
                             numepochs=10)
  # Get predicted values
  # Get predicted values
  train.result <- round(nn.predict(deepnet.result, x=as.matrix(Sanford.data.diabetes[-samp,-1])))
  test.result <- round(nn.predict(deepnet.result, x=as.matrix(Sanford.data.diabetes[samp,-1])))
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
  train.table <- prop.table(table(Sanford.data.diabetes$Unplanned.Visits.bin[-samp], train.result),1)
  test.table <- prop.table(table(Sanford.data.diabetes$Unplanned.Visits.bin[samp], test.result),1)
  # Print out row percentages of prediction tables
  print(train.table, 1)
  print(test.table, 1)
  # Increment tables (for average confusion matrix)
  train.table.all <- train.table.all + train.table
  test.table.all <- test.table.all + test.table
  # Increment arrays (for variance calculations within each cell)
  train.cell00 <- c(train.cell00, train.table[1,1])
  train.cell01 <- c(train.cell01, train.table[1,2])
  train.cell10 <- c(train.cell10, train.table[2,1])
  train.cell11 <- c(train.cell11, train.table[2,2])
  test.cell00 <- c(test.cell00, test.table[1,1])
  test.cell01 <- c(test.cell01, test.table[1,2])
  test.cell10 <- c(test.cell10, test.table[2,1])
  test.cell11 <- c(test.cell11, test.table[2,2])
 }
# Create averaged confusion matrix across all CV iterations
train.table.all <- train.table.all/i
test.table.all <- test.table.all/i

DNN.L.train <- train.table.all
DNN.L.test <- test.table.all

DNN.L.train.CR <- train.cell00
DNN.L.train.FA <- train.cell01
DNN.L.train.M <- train.cell10
DNN.L.train.H <- train.cell11

DNN.L.test.CR <- test.cell00
DNN.L.test.FA <- test.cell01
DNN.L.test.M <- test.cell10
DNN.L.test.H <- test.cell11

# Get means and sd's of each cell
#  Training set (Table 3)
summary(DNN.L.train.CR)
sd(DNN.L.train.CR)
summary(DNN.L.train.FA)
sd(DNN.L.train.FA)
summary(DNN.L.train.M)
sd(DNN.L.train.M)
summary(DNN.L.train.H)
sd(DNN.L.train.H)

#  Test set (Table 2)
summary(DNN.L.test.CR)
sd(DNN.L.test.CR)
summary(DNN.L.test.FA)
sd(DNN.L.test.FA)
summary(DNN.L.test.M)
sd(DNN.L.test.M)
summary(DNN.L.test.H)
sd(DNN.L.test.H)
