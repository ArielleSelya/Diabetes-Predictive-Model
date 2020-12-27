########################################################################
# Linear Support Vector Machine (SVM) Classification Results
# Author: Arielle S. Selya
# Date posted: 12/27/2020
########################################################################
# Load data
load("Managed Sanford Data.RData")
require(e1071)

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
  print(i)
  # Randomly select 10% to hold out for testing
  samp <- sample(1:dim(Sanford.data.diabetes)[1], round(0.1*dim(Sanford.data.diabetes)[1]))
  # Train SVM model
  svm.result <- svm(Unplanned.Visits.bin ~ ., data=Sanford.data.diabetes[-samp,], type = "C-classification",
                    kernel="linear")
  train.result <- predict(svm.result)
  test.result <- predict(svm.result, newdata=Sanford.data.diabetes[samp,])
  
  train.table <- prop.table(table(Sanford.data.diabetes$Unplanned.Visits.bin[-samp], train.result),1)
  test.table <- prop.table(table(Sanford.data.diabetes$Unplanned.Visits.bin[samp], test.result),1)
  # Print out row percentages of prediction tables
  print(train.table, 1)
  print(test.table, 1)
  # Increment tables (for average confusion matrix)
  for(j in 1:dim(train.table)[1])
    for(k in 1:dim(train.table)[2])
      train.table.all[j,k] <- train.table.all[j,k] + train.table[j,k]
  for(j in 1:dim(test.table)[1])
    for(k in 1:dim(test.table)[2])
      test.table.all[j,k] <- test.table.all[j,k] + test.table[j,k]
  print(prop.table(train.table, 1))
  print(prop.table(test.table, 1))
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

SVM.L.train <- train.table.all
SVM.L.test <- test.table.all

SVM.L.train.CR <- train.cell00
SVM.L.train.FA <- train.cell01
SVM.L.train.M <- train.cell10
SVM.L.train.H <- train.cell11

SVM.L.test.CR <- test.cell00
SVM.L.test.FA <- test.cell01
SVM.L.test.M <- test.cell10
SVM.L.test.H <- test.cell11

# Get means and sd's of each cell
#  Training set (Table 3)
summary(SVM.L.train.CR)
sd(SVM.L.train.CR)
summary(SVM.L.train.FA)
sd(SVM.L.train.FA)
summary(SVM.L.train.M)
sd(SVM.L.train.M)
summary(SVM.L.train.H)
sd(SVM.L.train.H)

#  Test set (Table 2)
summary(SVM.L.test.CR)
sd(SVM.L.test.CR)
summary(SVM.L.test.FA)
sd(SVM.L.test.FA)
summary(SVM.L.test.M)
sd(SVM.L.test.M)
summary(SVM.L.test.H)
sd(SVM.L.test.H)
