########################################################################
# Quadratic Discriminant Analysis (QDA) Classification Results
# Author: Arielle S. Selya
# Date posted: 12/27/2020
########################################################################
load("Managed Sanford Data.RData")
require(MASS)

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
  # Train QDA model
  train.qda <- qda(Unplanned.Visits.bin ~ ., 
                   data=Sanford.data.diabetes[-samp,])
  # Get predicted class for training and testing sets
  train.result <- predict(train.qda, Sanford.data.diabetes[-samp,])$class
  test.result <- predict(train.qda, Sanford.data.diabetes[samp,])$class
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

QDA.train <- train.table.all
QDA.test <- test.table.all

QDA.train.CR <- train.cell00  # CR = correct rejections
QDA.train.FA <- train.cell01  # FA = false alarms
QDA.train.M <- train.cell10   # M = misses
QDA.train.H <- train.cell11   # H = hits

QDA.test.CR <- test.cell00
QDA.test.FA <- test.cell01
QDA.test.M <- test.cell10
QDA.test.H <- test.cell11

# Get means and sd's of each cell
#  Training set (Table 3)
summary(QDA.train.CR)
sd(QDA.train.CR)
summary(QDA.train.FA)
sd(QDA.train.FA)
summary(QDA.train.M)
sd(QDA.train.M)
summary(QDA.train.H)
sd(QDA.train.H)

#  Test set (Table 2)
summary(QDA.test.CR)
sd(QDA.test.CR)
summary(QDA.test.FA)
sd(QDA.test.FA)
summary(QDA.test.M)
sd(QDA.test.M)
summary(QDA.test.H)
sd(QDA.test.H)
